(import (chezscheme) (check) (programs) (analysis posix) (analysis threads))

(define formatter (string-append (current-directory) "/build/bin/czfmt.so"))

(define root
  (let ([dir (format "/tmp/cz-fmt-test-~a" (process-id))])
    (system (format "rm -rf ~a; mkdir -p ~a" dir dir))
    dir))

(define (fresh-directory name)
  (let ([dir (string-append root "/" name)])
    (system (format "rm -rf ~a; mkdir -p ~a" dir dir))
    dir))

(define (write-file! path s)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (out) (put-bytevector out (if (string? s) (string->utf8 s) s)))))

(define (read-file path)
  (utf8->string (call-with-port (open-file-input-port path) get-bytevector-all)))

(define (run arguments directory)
  (run-program formatter arguments directory 60))

(define (status r) (exit-status-code (program-status r)))
(define (output r) (utf8->string (program-output r)))
(define (errors r) (utf8->string (program-error-output r)))

(define unformatted "(define (f x)\n(g x))\n")
(define formatted "(define (f x)\n  (g x))\n")

(define (directory-names dir) (list-sort string<? (directory-list dir)))

(define (string-search needle s)
  (let ([n (string-length needle)])
    (let loop ([i 0])
      (and (fx<= (fx+ i n) (string-length s))
           (or (string=? (substring s i (fx+ i n)) needle) (loop (fx+ i 1)))))))

(define (bytevector-append-all . bvs)
  (let ([out (make-bytevector (apply + (map bytevector-length bvs)))])
    (let loop ([bvs bvs] [at 0])
      (unless (null? bvs)
        (bytevector-copy! (car bvs) 0 out at (bytevector-length (car bvs)))
        (loop (cdr bvs) (+ at (bytevector-length (car bvs))))))
    out))


(test "a file is rewritten in place and keeps its mode"
  (let* ([dir (fresh-directory "write")] [file (string-append dir "/a.ss")])
    (write-file! file unformatted)
    (system (format "chmod 640 ~a" file))
    (let ([r (run (list "a.ss") dir)])
      (check-equal? 0 (status r))
      (check-equal? "" (output r))
      (check-equal? formatted (read-file file))
      (check-equal? #o640 (file-status-mode (file-status file)))
      (check-equal? '("a.ss") (directory-names dir)))))

(test "--check writes nothing, lists the files that would change and exits 1"
  (let ([dir (fresh-directory "check")])
    (write-file! (string-append dir "/a.ss") unformatted)
    (write-file! (string-append dir "/b.ss") formatted)
    (let ([r (run (list "--check" ".") dir)])
      (check-equal? 1 (status r))
      (check-equal? "./a.ss\n" (output r))
      (check-equal? unformatted (read-file (string-append dir "/a.ss"))))
    (check-equal? 0 (status (run (list "--check" "b.ss") dir)))))

(test "--diff prints a unified diff and writes nothing"
  (let ([dir (fresh-directory "diff")])
    (write-file! (string-append dir "/a.ss") "(a\nb)")
    (let ([r (run (list "--diff" "a.ss") dir)])
      (check-equal? 0 (status r))
      (check-equal? "--- a.ss\n+++ a.ss\n@@ -1,2 +1,2 @@\n (a\n-b)\n\\ No newline at end of file\n+  b)\n" (output r))
      (check-equal? "(a\nb)" (read-file (string-append dir "/a.ss"))))
    (check-equal? 1 (status (run (list "--diff" "--check" "a.ss") dir)))))

(test "- reads standard input and writes standard output"
  (let ([dir (fresh-directory "stdin")])
    (write-file! (string-append dir "/in") unformatted)
    (check-equal? 0
      (system (format "cd ~a && ~a -q --program ~a - < in > out" dir (self-executable-path) formatter)))
    (check-equal? formatted (read-file (string-append dir "/out")))
    (check-equal? 1
      (system (format "cd ~a && ~a -q --program ~a --check - < in > out" dir (self-executable-path) formatter)))))

(test "a file that cannot be formatted is reported, left alone, and the others are still formatted"
  (let ([dir (fresh-directory "refuse")])
    (write-file! (string-append dir "/a.ss") unformatted)
    (write-file! (string-append dir "/bad.ss") "(a))\n(b\nc)\n")
    (write-file! (string-append dir "/latin.ss") (bytevector 40 97 10 233 41 10))
    (let ([r (run (list "bad.ss" "latin.ss" "a.ss" "missing.ss") dir)])
      (check-equal? 2 (status r))
      (check-equal? "bad.ss: 1:4: cannot format: unexpected close parenthesis\nlatin.ss: the file is not UTF-8\nmissing.ss: does not exist\n"
        (errors r))
      (check-equal? formatted (read-file (string-append dir "/a.ss")))
      (check-equal? "(a))\n(b\nc)\n" (read-file (string-append dir "/bad.ss")))
      (check-equal? (bytevector 40 97 10 233 41 10)
        (call-with-port (open-file-input-port (string-append dir "/latin.ss")) get-bytevector-all)))))

(test "a byte-order mark and CRLF line endings are kept"
  (let ([dir (fresh-directory "bom")])
    (write-file! (string-append dir "/a.ss") (bytevector-append-all (bytevector #xEF #xBB #xBF) (string->utf8 "(a\r\nb)")))
    (check-equal? 0 (status (run (list "a.ss") dir)))
    (check-equal? (bytevector-append-all (bytevector #xEF #xBB #xBF) (string->utf8 "(a\r\n  b)\r\n"))
      (call-with-port (open-file-input-port (string-append dir "/a.ss")) get-bytevector-all))))

(test "--range formats only its lines, counted from 1"
  (let ([dir (fresh-directory "range")])
    (write-file! (string-append dir "/a.ss") "(f\nx\ny)\n")
    (check-equal? 0 (status (run (list "--range" "2:2" "a.ss") dir)))
    (check-equal? "(f\n  x\ny)\n" (read-file (string-append dir "/a.ss")))
    (check-equal? 2 (status (run (list "--range" "9:9" "a.ss") dir)))
    (check-equal? 2 (status (run (list "--range" "3:2" "a.ss") dir)))))

(test "the style comes from the project's configuration, or the file --config names"
  (let ([dir (fresh-directory "config")])
    (write-file! (string-append dir "/.cz") "(root)\n(format (indent (my-define 1)))\n")
    (write-file! (string-append dir "/other") "(format (head-alone 1))\n")
    (write-file! (string-append dir "/a.ss") "(my-define x\ny)\n(foo\ny)\n")
    (check-equal? 0 (status (run (list "--diff" "--config" "other" "a.ss") dir)))
    (check-equal? 0 (status (run (list "--config" "other" "a.ss") dir)))
    (check-equal? "(my-define x\n  y)\n(foo\n y)\n" (read-file (string-append dir "/a.ss")))
    (write-file! (string-append dir "/a.ss") "(my-define x\ny)\n(foo\ny)\n")
    (check-equal? 0 (status (run (list "a.ss") dir)))
    (check-equal? "(my-define x\n  y)\n(foo\n  y)\n" (read-file (string-append dir "/a.ss")))
    (write-file! (string-append dir "/.cz") "(format (head-alone 3))\n")
    (let ([r (run (list "a.ss") dir)])
      (check-equal? 2 (status r))
      (check (string-search "head-alone" (errors r))))))

(test "exclusions in a directory walk match whole path components"
  (let ([dir (fresh-directory "exclude")])
    (write-file! (string-append dir "/.cz") "(root)\n(exclude \"**/skip.ss\")\n")
    (write-file! (string-append dir "/skip.ss") unformatted)
    (write-file! (string-append dir "/noskip.ss") unformatted)
    (let ([r (run (list "--check" ".") dir)])
      (check-equal? 1 (status r))
      (check-equal? "./noskip.ss\n" (output r)))))

(test "an existing file with the temporary prefix is never touched, and no temporary file is left"
  (let ([dir (fresh-directory "temporary")])
    (write-file! (string-append dir "/a.ss") unformatted)
    (for-each (lambda (name) (write-file! (string-append dir "/" name) "keep me"))
      (list ".czfmt-a.ss-" ".czfmt-a.ss-XXXXXX" (format ".czfmt-~a-a.ss" (process-id)) ".a.ss.czfmt-tmp"))
    (check-equal? 0 (status (run (list "a.ss") dir)))
    (check-equal? formatted (read-file (string-append dir "/a.ss")))
    (for-each (lambda (name) (check-equal? "keep me" (read-file (string-append dir "/" name))))
      (list ".czfmt-a.ss-" ".czfmt-a.ss-XXXXXX" (format ".czfmt-~a-a.ss" (process-id)) ".a.ss.czfmt-tmp"))
    (check-equal? 5 (length (directory-list dir)))))

(define (error-lines r)
  (let loop ([s (errors r)] [i 0] [start 0] [acc '()])
    (cond
      [(fx= i (string-length s)) (reverse acc)]
      [(char=? (string-ref s i) #\newline) (loop s (fx+ i 1) (fx+ i 1) (cons (substring s start i) acc))]
      [else (loop s (fx+ i 1) start acc)])))

(test "concurrent runs on the same files do not collide, and a file another run replaced is left to it"
  (let ([dir (fresh-directory "concurrent")] [names (map (lambda (i) (format "f~a.ss" i)) (iota 20))])
    (for-each (lambda (name) (write-file! (string-append dir "/" name) unformatted)) names)
    (let ([results (make-mailbox)])
      (do ([i 0 (+ i 1)]) ((= i 4))
        (fork-thread (lambda () (mailbox-post! results (run names dir)))))
      (for-each
        (lambda (r)
          (check (memv (status r) '(0 2)))
          (check-equal? '()
            (remp (lambda (line)
                    (member line (map (lambda (name) (string-append name ": the file changed while it was formatted, so it was left as it is")) names)))
                  (error-lines r))))
        (map (lambda (i) (mailbox-take! results)) (iota 4))))
    (for-each (lambda (name) (check-equal? formatted (read-file (string-append dir "/" name)))) names)
    (check-equal? (list-sort string<? names) (directory-names dir))))

(test "a symbolic link is followed, and the link kept"
  (let ([dir (fresh-directory "link")])
    (write-file! (string-append dir "/target.ss") unformatted)
    (system (format "ln -s target.ss ~a/link.ss" dir))
    (check-equal? 0 (status (run (list "link.ss") dir)))
    (check-equal? formatted (read-file (string-append dir "/target.ss")))
    (check (file-symbolic-link? (string-append dir "/link.ss")))))

(test "a file that cannot be written, or has another hard link, is reported and left as it is"
  (let* ([dir (fresh-directory "unwritable")] [read-only (string-append dir "/read-only.ss")]
         [linked (string-append dir "/linked.ss")] [other (string-append dir "/other.ss")])
    (write-file! read-only unformatted)
    (system (format "chmod 444 ~a" read-only))
    (write-file! linked unformatted)
    (system (format "ln ~a ~a" linked other))
    (let ([r (run (list "read-only.ss" "linked.ss") dir)])
      (check-equal? 2 (status r))
      (check-equal?
        (string-append
          "read-only.ss: the file cannot be written, so it was left as it is\n"
          "linked.ss: the file has 2 hard links, which replacing it would separate, so it was left as it is\n")
        (errors r)))
    (check-equal? unformatted (read-file read-only))
    (check-equal? unformatted (read-file linked))
    (check-equal? unformatted (read-file other))
    (check-equal? '("linked.ss" "other.ss" "read-only.ss") (directory-names dir))
    (check-equal? 1 (status (run (list "--check" "read-only.ss" "linked.ss") dir)))))

(load-shared-object "libc.so.6")
(define inotify-init1 (foreign-procedure "inotify_init1" (int) int))
(define inotify-add-watch (foreign-procedure "inotify_add_watch" (int string unsigned-32) int))
(define read-fd (foreign-procedure "read" (int u8* size_t) ssize_t))
(define IN_CLOSE_NOWRITE #x10)
(define IN_NONBLOCK #x800)

;; Formatting a long file gives the test time to change it once czfmt
;; has closed it after reading, which inotify reports.
(test "a file changed while it is formatted is reported and keeps the change"
  (let* ([dir (fresh-directory "changed")] [file (string-append dir "/big.ss")]
         [fd (inotify-init1 IN_NONBLOCK)] [results (make-mailbox)])
    (write-file! file (apply string-append (make-list 50000 unformatted)))
    (check (fx>= (inotify-add-watch fd file IN_CLOSE_NOWRITE) 0))
    (fork-thread (lambda () (mailbox-post! results (run (list "big.ss") dir))))
    (let ([deadline (+ (time-second (current-time 'time-monotonic)) 60)] [event (make-bytevector 4096)])
      (let wait ()
        (when (and (fx<= (read-fd fd event 4096) 0) (< (time-second (current-time 'time-monotonic)) deadline))
          (sleep (make-time 'time-duration 1000000 0))
          (wait))))
    (write-file! file "(edited)\n")
    (let ([r (mailbox-take! results)])
      (check-equal? 2 (status r))
      (check-equal? "big.ss: the file changed while it was formatted, so it was left as it is\n" (errors r)))
    (check (string=? "(edited)\n" (read-file file)))
    (check-equal? '("big.ss") (directory-names dir))
    (close-fd fd)))

(test "a directory walk follows no symbolic link below the named directory, out of the tree or round a cycle"
  (let* ([dir (fresh-directory "walk-links")] [project (string-append dir "/project")] [outside (string-append dir "/outside")])
    (system (format "mkdir -p ~a ~a" project outside))
    (write-file! (string-append project "/a.ss") unformatted)
    (write-file! (string-append outside "/o.ss") unformatted)
    (system (format "cd ~a && ln -s ../outside ext && ln -s ../outside/o.ss o.ss && ln -s . l1 && ln -s . l2" project))
    (let ([r (run (list "--check" ".") project)])
      (check-equal? 1 (status r))
      (check-equal? "./a.ss\n" (output r)))
    (check-equal? 0 (status (run (list ".") project)))
    (check-equal? formatted (read-file (string-append project "/a.ss")))
    (check-equal? unformatted (read-file (string-append outside "/o.ss")))))

(test "usage errors exit 2"
  (check-equal? 2 (status (run '() root)))
  (check-equal? 2 (status (run '("--bogus" "a.ss") root)))
  (check-equal? 2 (status (run '("--range") root))))

(system (format "rm -rf ~a" root))
(test-exit)
