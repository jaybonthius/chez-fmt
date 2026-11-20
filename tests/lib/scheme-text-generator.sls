(library (scheme-text-generator)
  (export random-scheme-text)
  (import (chezscheme))

  (define heads
    '("define" "let" "let*" "cond" "lambda" "if" "case" "syntax-case" "module" "begin" "when" "do"
      "library" "define-record-type" "foo" "f" "list" "format" "|a b|" "a\\x41;b" "#%car" "#2%car"))

  (define atoms
    '("x" "y" "z" "42" "-1.5" "#\\(" "#\\x41" "#\\space" "#t" "\"s\"" "\"two\nlines\"" "\"tab\there\""
      "|p q|" "#{g h}" "..." "a.b" "+" "#!eof"))

  (define prefixes '("'" "`" "," ",@" "#'" "#`" "#," "#&" "#;"))

  (define (pick items) (list-ref items (random (length items))))

  (define (indentation)
    (case (random 6)
      [(0) "\t"]
      [(1) (string-append (make-string (random 4) #\space) "\t")]
      [else (make-string (random 14) #\space)]))

  ;; Tabs after a non-blank character, U+2028 and U+0085 inside a line, and
  ;; a form feed in leading whitespace are what the corpora rarely hold.
  (define (separator)
    (case (random 22)
      [(0 1 2 3 4 5 6) " "]
      [(7) "  "]
      [(8) "\t"]
      [(9 10 11 12) (string-append "\n" (indentation))]
      [(13) (string-append "\n\n" (indentation))]
      [(14) (string-append " ; trailing\n" (indentation))]
      [(15) (string-append "\n" (indentation) ";; comment\n" (indentation))]
      [(16) " #| block\n  comment |# "]
      [(17) " ;c\x2028;"]
      [(18) "\x2028;"]
      [(19) (string-append "\n" (indentation) "; czfmt: skip\n" (indentation))]
      [(20) (string-append "\n\f" (indentation))]
      [else " \x85; "]))

  (define (elements depth n)
    (let loop ([i 0] [acc '()])
      (if (= i n)
          (apply string-append (reverse acc))
          (loop (+ i 1) (cons* (random-datum (- depth 1)) (separator) acc)))))

  (define (random-datum depth)
    (if (or (<= depth 0) (< (random 10) 4))
        (pick atoms)
        (case (random 9)
          [(0) (string-append (pick prefixes) (random-datum (- depth 1)))]
          [(1) (string-append "[" (random-datum (- depth 1)) (separator) (elements depth (random 3)) "]")]
          [(2) (string-append "#(" (elements depth (random 4)) ")")]
          [(3) (string-append "(" (separator) (pick heads) (elements depth (random 4)) (separator) ")")]
          [else (string-append "(" (pick heads) (elements depth (random 5)) ")")])))

  (define (random-scheme-text)
    (let ([body (let loop ([i (random 5)] [acc '()])
                  (if (< i 0)
                      (apply string-append (reverse acc))
                      (loop (- i 1)
                            (cons* (case (random 8)
                                     [(0) "\n;;; heading\n"]
                                     [(1) "\n; czfmt: off\n"]
                                     [(2) "\n; czfmt: on\n"]
                                     [(3) (string-append "\n" (indentation))]
                                     [else "\n"])
                                   (random-datum 5)
                                   acc))))])
      (if (= (random 3) 0) body (string-append body "\n")))))
