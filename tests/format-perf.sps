(import (chezscheme) (reader-oracle) (programs) (analysis reader) (analysis text) (czfmt) (analysis path)
        (analysis posix))

(define failures 0)

(define (gate! name measured bound)
  (printf "~a: ~,1f ms (bound ~a ms)~a\n" name measured bound (if (<= measured bound) "" " FAIL"))
  (unless (<= measured bound) (set! failures (+ failures 1))))

(define (milliseconds thunk)
  (collect (collect-maximum-generation))
  (let ([start (current-time 'time-monotonic)])
    (thunk)
    (let ([d (time-difference (current-time 'time-monotonic) start)])
      (+ (* 1000 (time-second d)) (/ (time-nanosecond d) 1e6)))))

(define (best-of n thunk) (apply min (map (lambda (i) (milliseconds thunk)) (iota n))))

(define (s-directory)
  (find (lambda (dir) (file-exists? (string-append dir "/s/read.ss"))) (corpus-directories)))

;; The 50 lines lose their indentation, so the range has edits to check.
(define (dedented s t first last)
  (let ([a (text-line-start t first)] [b (text-line-end t last)])
    (string-append (substring s 0 a)
      (let loop ([i a] [acc '()] [line-start? #t])
        (cond
          [(= i b) (list->string (reverse acc))]
          [(and line-start? (memv (string-ref s i) '(#\space #\tab))) (loop (+ i 1) acc #t)]
          [else (loop (+ i 1) (cons (string-ref s i) acc) (char=? (string-ref s i) #\newline))]))
      (substring s b (string-length s)))))

(let* ([s (string-append "(define (f)\n" (apply string-append (make-list 5000 "  ;; a commented-out line\n")) "  (g\n    x))\n")]
       [c (read-cst (make-text s) (make-reader-mode 'chezscheme 'no-fold))])
  (gate! "format-text, a 5,000-line comment block inside a definition (best of 5)"
    (best-of 5 (lambda () (format-text c default-format-style))) 50))

(let ([dir (s-directory)])
  (if (not dir)
      (begin
        (printf "formatter performance: skipped the corpus gates, no corpus in CZ_CORPORA holds Chez's s/read.ss\n")
        (exit (if (= failures 0) 0 1)))
      (let* ([files (filter (lambda (p) (equal? (path-extension p) "ss")) (corpus-files (string-append dir "/s")))]
             [texts (map (lambda (p) (cons p (read-source-file p))) files)]
             [largest (fold-left (lambda (best x) (if (> (text-line-count (make-text (cdr x))) (text-line-count (make-text (cdr best)))) x best))
                        (car texts) (cdr texts))]
             [s (cdr largest)] [t (make-text s)] [lines (text-line-count t)]
             [mode (make-reader-mode 'chezscheme 'no-fold)]
             [c (read-cst t mode)]
             [middle (quotient lines 2)]
             [ranged (read-cst (make-text (dedented s t middle (+ middle 49))) mode)]
             [last-line (- lines 2)])
        (unless (= (length (format-lines ranged default-format-style middle (+ middle 49))) 50)
          (errorf 'format-perf "the range has no edits to check"))
        (gate! (format "format-text, ~a (~a lines; best of 5)" (path-last (car largest)) lines)
          (best-of 5 (lambda () (format-text c default-format-style))) 100)
        (gate! "format-lines, 50 lines of it, each with an edit (best of 5)"
          (best-of 5 (lambda () (format-lines ranged default-format-style middle (+ middle 49)))) 10)
        (gate! "format-line-indentation of its last line (best of 5)"
          (best-of 5 (lambda () (format-line-indentation c default-format-style last-line))) 50)
        (let ([r (run-program (string-append (current-directory) "/build/bin/czfmt.so")
                   (list "--check" (string-append dir "/s")) (current-directory) 60)])
          (unless (memv (exit-status-code (program-status r)) '(0 1)) (errorf 'format-perf "czfmt failed"))
          (gate! (format "czfmt --check over s/, ~a files, from process start" (length files))
            (* 1000 (program-seconds r)) 2000))
        (printf "formatter performance: ~a failures\n" failures)
        (exit (if (= failures 0) 0 1)))))
