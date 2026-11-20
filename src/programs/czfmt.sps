(import (chezscheme) (czfmt) (analysis config) (analysis document) (analysis text)
        (analysis diagnostic) (analysis path) (analysis posix))

(define usage
  "usage: czfmt [--check] [--diff] [--range L1:L2] [--config FILE] PATH...

Formats each file, and each Scheme file under each directory, in place. Only
whitespace changes: indentation, spaces inside delimiters and at line ends,
and a missing final line break; every line break stays, and a file whose
reading would change is left as it is. - reads standard input and writes the
result to standard output.

  --check        write nothing; list the files that would change
  --diff         write nothing; print the changes as a unified diff
  --range L1:L2  format only lines L1 to L2, counted from 1
  --config FILE  use FILE instead of the .cz files above each path

The style comes from the (format ...) section of the project's .cz.

Exit status: 0 when nothing needed formatting or every file was formatted,
1 when --check found a file to change, 2 when a file could not be formatted
or on a usage, configuration or internal error.
")

;; Kept for the whole run: a port on a descriptor closes the descriptor
;; when the collector reclaims it.
(define stdout (standard-output-port))
(define stderr (transcoded-port (standard-error-port) (native-transcoder)))

(define (report fmt . args)
  (apply fprintf stderr fmt args)
  (newline stderr)
  (flush-output-port stderr))

(define (fail fmt . args)
  (apply report fmt args)
  (exit 2))

(define-record-type options
  (nongenerative) (sealed #t)
  (fields (mutable check?) (mutable diff?) (mutable range) (mutable config) (mutable paths)))

(define (parse-range s)
  (let* ([colon (let loop ([i 0]) (cond [(fx= i (string-length s)) #f] [(char=? (string-ref s i) #\:) i] [else (loop (fx+ i 1))]))]
         [from (and colon (string->number (substring s 0 colon) 10))]
         [to (and colon (string->number (substring s (fx+ colon 1) (string-length s)) 10))])
    (unless (and from to (fixnum? from) (fixnum? to) (fx<= 1 from to))
      (fail "--range takes L1:L2, two line numbers from 1 with L1 <= L2\n~a" usage))
    (cons (fx- from 1) (fx- to 1))))

(define (parse-options arguments)
  (let ([o (make-options #f #f #f #f '())])
    (let loop ([args arguments])
      (define (value) (if (pair? (cdr args)) (cadr args) (fail "~a needs a value\n~a" (car args) usage)))
      (if (null? args)
          (begin
            (when (null? (options-paths o)) (fail "no path to format\n~a" usage))
            (options-paths-set! o (reverse (options-paths o)))
            o)
          (case (car args)
            [("--check") (options-check?-set! o #t) (loop (cdr args))]
            [("--diff") (options-diff?-set! o #t) (loop (cdr args))]
            [("--range") (options-range-set! o (parse-range (value))) (loop (cddr args))]
            [("--config") (options-config-set! o (value)) (loop (cddr args))]
            [("--help" "-h") (put-bytevector stdout (string->utf8 usage)) (flush-output-port stdout) (exit 0)]
            [else
             (when (and (fx> (string-length (car args)) 1) (char=? (string-ref (car args) 0) #\-))
               (fail "unknown option ~a\n~a" (car args) usage))
             (options-paths-set! o (cons (car args) (options-paths o)))
             (loop (cdr args))])))))

(define (config-for options path)
  (if (options-config options)
      (read-config-file (options-config options))
      (find-project-config path
        (let ([cwd (canonical-path (current-directory))] [p (canonical-path path)])
          (if (path-relative-to p cwd) cwd (path-parent p))))))

(define utf-8-bom #vu8(#xEF #xBB #xBF))

(define (starts-with-bom? bv)
  (and (fx>= (bytevector-length bv) 3)
       (fx= (bytevector-u8-ref bv 0) #xEF) (fx= (bytevector-u8-ref bv 1) #xBB) (fx= (bytevector-u8-ref bv 2) #xBF)))

(define-condition-type &unformattable &error make-unformattable unformattable?)

(define (unformattable fmt . args)
  (raise (condition (make-unformattable) (make-message-condition (apply format fmt args)))))

;; Text that is not UTF-8 decodes with replacement characters, which
;; writing back would put in place of the original bytes.
(define (decode-source bytes)
  (let* ([s (decode-source-bytes bytes)]
         [body (if (starts-with-bom? bytes)
                   (let ([bv (make-bytevector (fx- (bytevector-length bytes) 3))])
                     (bytevector-copy! bytes 3 bv 0 (bytevector-length bv))
                     bv)
                   bytes)])
    (unless (bytevector=? body (string->utf8 s)) (unformattable "the file is not UTF-8"))
    s))

(define (encode-source text bom?)
  (let ([body (string->utf8 text)])
    (if bom?
        (let ([bv (make-bytevector (fx+ 3 (bytevector-length body)))])
          (bytevector-copy! utf-8-bom 0 bv 0 3)
          (bytevector-copy! body 0 bv 3 (bytevector-length body))
          bv)
        body)))

(define (position t offset)
  (let ([line (text-offset-line t offset)])
    (values (fx+ line 1) (fx+ 1 (fx- offset (text-line-start t line))))))

(define (formatted config path s range)
  (let* ([t (make-text s)] [c (read-document config path t)] [style (config-format-style config)])
    (guard (e [(format-refused? e)
               (let ([d (format-refused-diagnostic e)])
                 (let-values ([(line column) (position t (diagnostic-start d))])
                   (unformattable "~a:~a: cannot format: ~a" line column (diagnostic-message d))))])
      (apply-edits s
        (if range
            (let ([last (fx- (text-line-count t) 1)])
              (when (fx> (car range) last)
                (unformattable "line ~a is past the end, line ~a" (fx+ (car range) 1) (fx+ last 1)))
              (format-lines c style (car range) (fxmin (cdr range) last)))
            (format-text c style))))))

;; Formatting keeps every line break, so the lines of the two texts pair
;; one to one, but for the line break a last line may gain.
(define (text-lines s)
  (let* ([t (make-text s)] [count (text-line-count t)])
    (let loop ([i (fx- count 1)] [acc '()])
      (if (fx< i 0)
          (list->vector acc)
          (let ([start (text-line-start t i)] [end (text-line-end t i)])
            (if (and (fx= i (fx- count 1)) (fx= start end))
                (loop (fx- i 1) acc)
                (loop (fx- i 1) (cons (cons (substring s start end) (fx< end (string-length s))) acc))))))))

(define context-lines 3)

(define (unified-diff name old new)
  (let* ([a (text-lines old)] [b (text-lines new)] [n (vector-length a)])
    (define (changed? i) (not (equal? (vector-ref a i) (vector-ref b i))))
    (define (put-line! out prefix line)
      (put-string out prefix)
      (put-string out (car line))
      (newline out)
      (unless (cdr line) (put-string out "\\ No newline at end of file\n")))
    (let-values ([(out get) (open-string-output-port)])
      (fprintf out "--- ~a\n+++ ~a\n" name name)
      (let next-hunk ([i 0])
        (let ([first (let find ([i i]) (cond [(fx= i n) #f] [(changed? i) i] [else (find (fx+ i 1))]))])
          (when first
            (let* ([start (fxmax 0 (fx- first context-lines))]
                   [end (let extend ([last first] [i (fx+ first 1)])
                          (cond
                            [(fx= i n) (fxmin n (fx+ last 1 context-lines))]
                            [(changed? i) (extend i (fx+ i 1))]
                            [(fx> (fx- i last) (fx* 2 context-lines)) (fxmin n (fx+ last 1 context-lines))]
                            [else (extend last (fx+ i 1))]))])
              (fprintf out "@@ -~a,~a +~a,~a @@\n" (fx+ start 1) (fx- end start) (fx+ start 1) (fx- end start))
              (let lines ([i start])
                (when (fx< i end)
                  (if (changed? i)
                      (let ([run-end (let run ([j i]) (if (and (fx< j end) (changed? j)) (run (fx+ j 1)) j))])
                        (do ([j i (fx+ j 1)]) ((fx= j run-end)) (put-line! out "-" (vector-ref a j)))
                        (do ([j i (fx+ j 1)]) ((fx= j run-end)) (put-line! out "+" (vector-ref b j)))
                        (lines run-end))
                      (begin (put-line! out " " (vector-ref a i)) (lines (fx+ i 1))))))
              (next-hunk end)))))
      (get))))

(define (put-text! s)
  (put-bytevector stdout (string->utf8 s)))

(define (replace-file! path bytes stamp)
  (let* ([real (real-path path)] [status (file-status real)] [mode (file-status-mode status)])
    (unless (file-writable? real) (unformattable "the file cannot be written, so it was left as it is"))
    (unless (= (file-status-links status) 1)
      (unformattable "the file has ~a hard links, which replacing it would separate, so it was left as it is"
        (file-status-links status)))
    (let-values ([(temporary fd) (create-temporary-file (string-append (path-parent real) "/.czfmt-" (path-last real) "-"))])
      (let ([open? #t])
        (guard (e [#t (when open? (close-fd fd)) (delete-file temporary) (raise e)])
          (set-fd-mode! fd mode)
          (write-fd fd bytes)
          (sync-fd! fd)
          (set! open? #f)
          (close-fd fd)
          (let ([now (file-stamp real)])
            (unless (and stamp now (file-stamp=? stamp now))
              (unformattable "the file changed while it was formatted, so it was left as it is")))
          (rename-path! temporary real))))))

(define (format-file! options path)
  (let* ([stamp (file-stamp path)]
         [bytes (call-with-port (open-file-input-port path) get-bytevector-all)]
         [bytes (if (eof-object? bytes) (make-bytevector 0) bytes)]
         [s (decode-source bytes)]
         [new (formatted (config-for options path) path s (options-range options))])
    (cond
      [(string=? s new) 0]
      [(or (options-check? options) (options-diff? options))
       (when (options-diff? options) (put-text! (unified-diff path s new)))
       (when (and (options-check? options) (not (options-diff? options))) (put-text! (string-append path "\n")))
       (if (options-check? options) 1 0)]
      [else (replace-file! path (encode-source new (starts-with-bom? bytes)) stamp) 0])))

(define (format-input! options)
  (let* ([bytes (get-bytevector-all (standard-input-port))]
         [bytes (if (eof-object? bytes) (make-bytevector 0) bytes)]
         [s (decode-source bytes)]
         [cwd (canonical-path (current-directory))]
         [config (if (options-config options) (read-config-file (options-config options)) (find-project-config (string-append cwd "/-") cwd))]
         [new (formatted config #f s (options-range options))])
    (cond
      [(options-diff? options) (unless (string=? s new) (put-text! (unified-diff "-" s new)))]
      [(not (options-check? options)) (put-bytevector stdout (encode-source new (starts-with-bom? bytes)))])
    (if (and (options-check? options) (not (string=? s new))) 1 0)))

(define (config-error-location c)
  (let ([path (config-error-path c)])
    (guard (e [(i/o-error? e) path])
      (let* ([bv (call-with-port (open-file-input-port path) get-bytevector-all)]
             [t (make-text (decode-source-bytes (if (eof-object? bv) (make-bytevector 0) bv)))])
        (let-values ([(line column) (position t (fxmin (config-error-start c) (text-length t)))])
          (format "~a:~a:~a" path line column))))))

(define (with-file-errors name thunk)
  (guard (e [(unformattable? e) (report "~a: ~a" name (condition-message e)) 2]
            [(config-error? e) (report "~a: error: ~a" (config-error-location e) (condition-message e)) 2]
            [(i/o-error? e)
             (report "~a: ~a" name (call-with-string-output-port (lambda (p) (display-condition e p))))
             2]
            [(posix-error? e) (report "~a: ~a" name (condition-message e)) 2])
    (thunk)))

(define (directory-files options p)
  (let ([dir (canonical-path p)])
    (map (lambda (file) (string-append p (substring file (string-length dir) (string-length file))))
         (config-source-files (config-for options (string-append dir "/.")) dir))))

(define (format-path! options p)
  (cond
    [(string=? p "-") (with-file-errors p (lambda () (format-input! options)))]
    [(file-directory? p)
     (let ([files (with-file-errors p (lambda () (directory-files options p)))])
       (if (list? files)
           (fold-left (lambda (status file) (fxmax status (with-file-errors file (lambda () (format-file! options file)))))
             0 files)
           files))]
    [(file-exists? p) (with-file-errors p (lambda () (format-file! options p)))]
    [else (report "~a: does not exist" p) 2]))

(define (main arguments)
  (let ([options (parse-options arguments)])
    (let ([status (fold-left (lambda (status p) (fxmax status (format-path! options p))) 0 (options-paths options))])
      (flush-output-port stdout)
      status)))

(unless (and (equal? (call-with-values scheme-version-number list) '(10 4 1)) (threaded?))
  (fail "czfmt needs threaded Chez Scheme 10.4.1, not ~a~a"
    (scheme-version) (if (threaded?) "" " (non-threaded)")))

;; Chez's own interrupt handler runs only once the main thread has spent
;; about a thousand timer ticks in Scheme code; at its default action
;; SIGINT ends the run at once.
(set-signal-default! signal-interrupt)

(exit
  (guard (c [#t (fail "czfmt: ~a" (call-with-string-output-port (lambda (p) (display-condition c p))))])
    (main (cdr (command-line)))))
