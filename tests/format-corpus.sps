(import (chezscheme) (reader-oracle) (czfmt) (analysis reader) (analysis text)
        (analysis diagnostic) (analysis config) (analysis document) (analysis path))

(random-seed 20261110)

(define failures 0)
(define shown 0)

(define (fail! fmt . args)
  (set! failures (+ failures 1))
  (when (< shown 40)
    (set! shown (+ shown 1))
    (apply printf (string-append "FAIL " fmt "\n") args)))

(define (split s c)
  (let loop ([i 0] [start 0] [acc '()])
    (cond
      [(= i (string-length s)) (reverse (cons (substring s start i) acc))]
      [(char=? (string-ref s i) c) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
      [else (loop (+ i 1) start acc)])))

(define config (default-project-config "/"))

(define (cst-for path s) (read-document config path (make-text s)))

(define (formatted path s style) (apply-edits s (format-text (cst-for path s) style)))

;; CZ_FORMAT_CORPORA is colon-separated NAME=DIR|GLOB,... entries,
;; the globs relative to DIR; NAME is one of tests/data/format-corpus-changes.ss.
(define named-corpora
  (let ([v (getenv "CZ_FORMAT_CORPORA")])
    (if (or (not v) (string=? v ""))
        '()
        (map (lambda (entry)
               (let* ([eq (let f ([i 0]) (if (char=? (string-ref entry i) #\=) i (f (+ i 1))))]
                      [rest (substring entry (+ eq 1) (string-length entry))]
                      [parts (split rest #\|)])
                 (list (string->symbol (substring entry 0 eq)) (car parts)
                       (if (null? (cdr parts)) '("**/*") (split (cadr parts) #\,)))))
             (filter (lambda (e) (not (string=? e ""))) (split v #\:))))))

;; Copies of one library for several Schemes hold the same text, which
;; would count once per copy.
(define (corpus-texts dir globs)
  (let ([compiled (map (lambda (g) (compile-glob g dir)) globs)] [seen (make-hashtable string-hash string=?)])
    (filter values
      (map (lambda (path)
             (and (exists (lambda (g) (glob-match? g path)) compiled)
                  (let ([s (read-source-file path)])
                    (and (not (hashtable-ref seen s #f))
                         (begin (hashtable-set! seen s #t) (cons path s))))))
           (corpus-files dir)))))

(define (chez-data s)
  (guard (e [#t #f])
    (let ([in (open-string-input-port s)])
      (let loop ([acc '()])
        (let ([x (read in)])
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

(define (mixed formatted-text original a b)
  (let ([f (make-text formatted-text)] [o (make-text original)])
    (string-append (substring formatted-text 0 (text-line-start f a))
                   (substring original (text-line-start o a) (text-line-end o b))
                   (substring formatted-text (text-line-end f b) (string-length formatted-text)))))

(define (line-text t i) (substring (text-string t) (text-line-start t i) (text-line-end t i)))

(define on-type-lines 0)
(define on-type-agreeing 0)

(define (leading-column s start end)
  (let loop ([i start] [col 0])
    (if (and (< i end) (memv (string-ref s i) '(#\space #\tab)))
        (loop (+ i 1) (if (char=? (string-ref s i) #\tab) (* 8 (+ 1 (quotient col 8))) (+ col 1)))
        col)))

(define (check-file! path s)
  (let ([data (chez-data s)])
    (if (not (and data (equal? data (chez-data s))))
        'unreadable
        (guard (e [(format-refused? e)
                   (fail! "~a: refused: ~a" path (diagnostic-message (format-refused-diagnostic e)))
                   'failed])
          (let* ([out (formatted path s default-format-style)] [t (make-text s)] [w (make-text out)])
            (unless (equal? data (chez-data out)) (fail! "~a: Chez reads other data after formatting" path))
            (unless (null? (format-text (cst-for path out) default-format-style)) (fail! "~a: a second pass changes it" path))
            (do ([k 0 (+ k 1)]) ((= k 3))
              (let* ([lines (text-line-count t)] [a (random lines)] [b (min (- lines 1) (+ a (random 40)))]
                     [m (mixed out s a b)] [mt (make-text m)] [whole (formatted path m default-format-style)] [wt (make-text whole)])
                (unless (exists (lambda (i) (and (or (< i a) (> i b)) (< i (text-line-count mt))
                                                 (not (string=? (line-text mt i) (line-text wt i)))))
                                (iota (text-line-count mt)))
                  (unless (string=? whole (apply-edits m (format-lines (cst-for path m) default-format-style a b)))
                    (fail! "~a: lines ~a to ~a formatted alone differ from the whole file" path (+ a 1) (+ b 1))))))
            (let ([c (cst-for path out)])
              (do ([k 0 (+ k 1)]) ((= k 10))
                (let* ([line (random (text-line-count w))] [ls (text-line-start w line)] [le (text-line-end w line)])
                  (when (and (< ls le) (not (char=? (string-ref out (+ ls (let f ([i 0]) (if (and (< (+ ls i) le) (char-whitespace? (string-ref out (+ ls i)))) (f (+ i 1)) i)))) #\;))
                             (let ([tok (cst-token-at c ls)]) (or (not tok) (eq? (token-kind tok) 'whitespace) (= (token-start tok) ls))))
                    (set! on-type-lines (+ on-type-lines 1))
                    (when (null? (format-line-indentation c default-format-style line))
                      (set! on-type-agreeing (+ on-type-agreeing 1)))))))
            'formatted)))))

;; This excerpt tells the styles apart: a call whose arguments hang, a
;; nested cond, and a parameter written with 3-space indentation.
(define (seven-excerpt dir)
  (let* ([lines (list->vector (split (read-source-file (string-append dir "/s/7.ss")) #\newline))]
         [line (lambda (n) (vector-ref lines (- n 1)))]
         [range (lambda (from to) (map (lambda (i) (line (+ from i))) (iota (+ (- to from) 1))))]
         [middle (map (lambda (l) (if (>= (string-length l) 6) (substring l 6 (string-length l)) l)) (range 1263 1297))]
         [last-middle (car (last-pair middle))])
    (unless (and (string=? (line 713) "(define $format-scheme-version")
                 (string=? (line 1263) "      (if-feature pthreads")
                 (string=? (line 1299) "(define collect-request-handler"))
      (errorf 'seven-excerpt "s/7.ss is not the expected version"))
    (apply string-append
      (map (lambda (l) (string-append l "\n"))
           (append (range 713 718) '("")
                   (reverse (cons (substring last-middle 0 (- (string-length last-middle) 3)) (cdr (reverse middle))))
                   '("") (range 1299 1305))))))

(define (changed-lines before after)
  (let ([a (make-text before)] [b (make-text after)])
    (filter (lambda (i) (not (string=? (line-text a i) (line-text b i)))) (iota (text-line-count a)))))

(define (style-with-rules style rules)
  (let ([table (hashtable-copy (format-style-rules style) #t)])
    (for-each (lambda (r) (hashtable-set! table (car r) (cdr r))) rules)
    (make-format-style table (format-style-call-layout style) (format-style-head-alone style)
      (format-style-comment-lines style) (format-style-column-zero-heads style))))

(define (check-seven! dir)
  (let* ([s (seven-excerpt dir)]
         [rules '(("if-feature" . 1) ("with-tc-mutex" . 0))]
         [s3 (style-with-rules default-format-style rules)]
         [s1 (style-with-rules (make-format-style (format-style-rules default-format-style) 'align 2 'with-code
                                 (format-style-column-zero-heads default-format-style))
               rules)]
         [handler-block (map (lambda (i) (+ 6 1 35 1 i)) '(1 2 3 4 5 6))]
         [s3-changed (changed-lines s (formatted "7.ss" s s3))]
         [s1-changed (changed-lines s (formatted "7.ss" s s1))])
    (printf "s/7.ss excerpt: S3 changes lines ~s, S1 changes ~a lines\n" (map add1 s3-changed) (length s1-changed))
    (unless (equal? s3-changed handler-block)
      (fail! "S3 should change only the six lines of collect-request-handler, ~s, not ~s" handler-block s3-changed))
    (unless (= (length s1-changed) 9)
      (fail! "S1 should change 9 lines of the excerpt, not ~a" (length s1-changed)))))

(define (change-shares texts)
  (let ([total 0] [raw 0] [decision 0])
    (for-each
      (lambda (entry)
        (let* ([path (car entry)] [s (cdr entry)] [t (make-text s)] [c (cst-for path s)])
          (guard (e [(format-refused? e) (void)])
            (let* ([w (make-text (apply-edits s (format-text c default-format-style)))]
                   [n (text-line-count t)] [deltas (make-vector n #f)])
              (do ([i 0 (+ i 1)]) ((= i n))
                (let ([ls (text-line-start t i)] [le (text-line-end t i)])
                  (unless (for-all char-whitespace? (string->list (substring s ls le)))
                    (vector-set! deltas i (- (leading-column (text-string w) (text-line-start w i) (text-line-end w i))
                                             (leading-column s ls le))))))
              (do ([i 0 (+ i 1)]) ((= i n))
                (let ([d (vector-ref deltas i)])
                  (when d
                    (set! total (+ total 1))
                    (unless (= d 0) (set! raw (+ raw 1)))
                    (let* ([ls (text-line-start t i)]
                           [p (let f ([k ls]) (if (char-whitespace? (string-ref s k)) (f (+ k 1)) k))]
                           [opener (find (lambda (x) (and (node? x) (eq? (node-kind x) 'list) (< (node-start x) p)))
                                         (reverse (cst-path-at c p)))]
                           [opener-delta (or (and opener (vector-ref deltas (text-offset-line t (node-start opener)))) 0)])
                      (unless (= d opener-delta) (set! decision (+ decision 1)))))))))))
      texts)
    (values total (if (= total 0) 0 (* 100. (/ decision total))) (if (= total 0) 0 (* 100. (/ raw total))))))

(define recorded
  (call-with-input-file "tests/data/format-corpus-changes.ss"
    (lambda (in) (let loop ([acc '()]) (let ([x (read in)]) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

(let ([dirs (corpus-directories)])
  (if (and (null? dirs) (null? named-corpora))
      (printf "formatter corpus: skipped, neither CZ_CORPORA nor CZ_FORMAT_CORPORA is set\n")
      (let ([texts (append (apply append (map (lambda (dir) (corpus-texts dir '("**/*"))) dirs))
                           (apply append (map (lambda (c) (corpus-texts (cadr c) (caddr c))) named-corpora)))]
            [outcomes (make-eq-hashtable)])
        (for-each (lambda (entry) (hashtable-update! outcomes (check-file! (car entry) (cdr entry)) add1 0)) texts)
        (printf "formatter gates: ~a files formatted, ~a not read the same twice by Chez, ~a failed\n"
          (hashtable-ref outcomes 'formatted 0) (hashtable-ref outcomes 'unreadable 0) (hashtable-ref outcomes 'failed 0))
        (printf "on-type indentation agrees with whole-file formatting on ~a of ~a sampled code lines (~,1f%)\n"
          on-type-agreeing on-type-lines (if (= on-type-lines 0) 0. (* 100. (/ on-type-agreeing on-type-lines))))
        (let ([seven (find (lambda (dir) (file-exists? (string-append dir "/s/7.ss"))) dirs)])
          (if seven
              (check-seven! seven)
              (printf "s/7.ss excerpt: skipped, no corpus in CZ_CORPORA holds Chez's s/7.ss\n")))
        (for-each
          (lambda (c)
            (let-values ([(lines decision raw) (change-shares (corpus-texts (cadr c) (caddr c)))])
              (let ([expected (assq (car c) recorded)])
                (if expected
                    (printf "~a: ~a lines, decision ~,1f% (recorded ~a), raw ~,1f% (recorded ~a)~a\n"
                      (car c) lines decision (cadr expected) raw (caddr expected)
                      (if (and (<= (abs (- decision (cadr expected))) 1.0) (<= (abs (- raw (caddr expected))) 1.0))
                          ""
                          ", more than one point from the record"))
                    (printf "~a: ~a lines, decision ~,1f%, raw ~,1f%\n" (car c) lines decision raw)))))
          named-corpora)
        (printf "formatter corpus: ~a failures\n" failures)
        (exit (if (= failures 0) 0 1)))))
