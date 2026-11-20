(import (chezscheme) (reader-oracle) (scheme-text-generator) (analysis text) (analysis reader)
        (analysis diagnostic) (chez-fmt))

(define case-count
  (let ([v (getenv "CHEZ_TOOLS_MUTANTS")]) (if v (string->number v) 100000)))

(random-seed 20261110)

(define failures 0)
(define shown 0)

(define (fail! text fmt . args)
  (set! failures (+ failures 1))
  (when (< shown 40)
    (set! shown (+ shown 1))
    (printf "FAIL ~s: ~?\n" text fmt args)))

(define (condition-text e)
  (if (condition? e) (call-with-string-output-port (lambda (p) (display-condition e p))) (format "~s" e)))

;; A refusal of a text that reads whole is the reading check firing on the
;; formatter's own edits.
(define (formatting s c style thunk)
  (guard (e [(format-refused? e)
             (when (eq? 'internal-error (diagnostic-rule (format-refused-diagnostic e)))
               (fail! s "the reading check fired: ~a" (diagnostic-message (format-refused-diagnostic e))))
             'refused]
            [#t (fail! s "raised ~a" (condition-text e)) 'refused])
    (thunk)))

(define (only-whitespace-changes? s edits)
  (for-all (lambda (e)
             (and (for-all (lambda (ch) (memv ch '(#\space #\tab))) (string->list (substring s (text-edit-start e) (text-edit-end e))))
                  (or (for-all (lambda (ch) (char=? ch #\space)) (string->list (text-edit-replacement e)))
                      (= (text-edit-start e) (string-length s)))))
           edits))

(define (check-case! s mode)
  (let* ([c (read-cst (make-text s) mode)] [lines (text-line-count (cst-text c))])
    (let ([edits (formatting s c default-format-style (lambda () (format-text c default-format-style)))])
      (unless (eq? edits 'refused)
        (unless (only-whitespace-changes? s edits) (fail! s "an edit changes more than spaces and tabs"))
        (let* ([out (apply-edits s edits)] [c2 (read-cst (make-text out) mode)])
          (let ([again (formatting out c2 default-format-style (lambda () (format-text c2 default-format-style)))])
            (unless (null? again) (fail! s "a second pass changes ~s" out))))))
    (let* ([a (random lines)] [b (+ a (random (- lines a)))])
      (formatting s c default-format-style (lambda () (format-lines c default-format-style a b))))
    (let ([line (random lines)])
      (let ([edits (guard (e [#t (fail! s "format-line-indentation raised ~a" (condition-text e)) '()])
                     (format-line-indentation c default-format-style line))])
        (unless (and (list? edits) (< (length edits) 2))
          (fail! s "format-line-indentation gave ~s for line ~a" edits line))))))

(define modes
  (list (make-reader-mode 'chezscheme 'no-fold) (make-reader-mode 'r6rs 'no-fold) (make-reader-mode 'chezscheme 'fold)))

(define (random-mode) (list-ref modes (random (length modes))))

(let ([dirs (corpus-directories)])
  (do ([k 0 (+ k 1)]) ((= k (quotient case-count 2)))
    (check-case! (random-scheme-text) (random-mode)))
  (if (null? dirs)
      (printf "formatter fuzzing: ~a generated texts; corpus mutants skipped, CHEZ_TOOLS_CORPORA is not set\n" (quotient case-count 2))
      (let ([sources (list->vector
                       (map (lambda (p) (let ([s (read-source-file p)]) (cons s (line-starts s))))
                            (apply append (map corpus-files dirs))))])
        (do ([k 0 (+ k 1)]) ((= k (- case-count (quotient case-count 2))))
          (let ([source (vector-ref sources (random (vector-length sources)))])
            (check-case! (random-mutant (car source) (cdr source)) (random-mode))))
        (printf "formatter fuzzing: ~a generated texts and ~a corpus mutants\n"
          (quotient case-count 2) (- case-count (quotient case-count 2)))))
  (printf "formatter fuzzing: ~a failures\n" failures)
  (exit (if (= failures 0) 0 1)))
