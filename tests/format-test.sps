(import (chezscheme) (check) (scheme-text-generator) (chez-fmt) (analysis reader) (analysis text)
        (analysis diagnostic) (analysis config) (analysis posix))

(define chez-mode (make-reader-mode 'chezscheme 'no-fold))

(define (cst-of s) (read-cst (make-text s) chez-mode))

(define formatted
  (case-lambda
    [(s) (formatted s default-format-style)]
    [(s style) (apply-edits s (format-text (cst-of s) style))]))

(define (refused? thunk)
  (guard (e [(format-refused? e) #t]) (thunk) #f))

(define (lines . ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))

(define (style-with . changes)
  (let ([rules (hashtable-copy (format-style-rules default-format-style) #t)])
    (for-each (lambda (c) (hashtable-set! rules (car c) (cdr c))) changes)
    (make-format-style rules (format-style-call-layout default-format-style) (format-style-head-alone default-format-style)
      (format-style-comment-lines default-format-style) (format-style-column-zero-heads default-format-style))))

;; Checked against the input's own tokens, so that a fault in the
;; formatter's own check cannot hide one in its edits.
(define (edit-violations s edits)
  (let* ([c (cst-of s)] [tokens (vector->list (cst-tokens c))] [n (string-length s)])
    (define (inside-whitespace? e)
      (exists (lambda (tok)
                (and (eq? (token-kind tok) 'whitespace)
                     (fx<= (token-start tok) (text-edit-start e)) (fx<= (text-edit-end e) (token-end tok))))
              tokens))
    (define (at-boundary? offset)
      (or (fx= offset 0) (fx= offset n) (exists (lambda (tok) (fx= (token-start tok) offset)) tokens)))
    (filter values
      (map (lambda (e)
             (let ([old (substring s (text-edit-start e) (text-edit-end e))] [new (text-edit-replacement e)])
               (cond
                 [(and (fx= (text-edit-start e) n) (fx= (text-edit-end e) n) (member new '("\n" "\r\n" "\r"))) #f]
                 [(not (for-all (lambda (ch) (memv ch '(#\space #\tab))) (string->list old))) (list 'removes old)]
                 [(not (for-all (lambda (ch) (char=? ch #\space)) (string->list new))) (list 'inserts new)]
                 [(fx= (text-edit-start e) (text-edit-end e)) (if (at-boundary? (text-edit-start e)) #f (list 'splits e))]
                 [(inside-whitespace? e) #f]
                 [else (list 'outside-whitespace e)])))
           edits))))

(define (line-break-gaps s)
  (let ([c (cst-of s)])
    (let loop ([tokens (vector->list (cst-tokens c))] [count 0] [acc '()])
      (cond
        [(null? tokens) (reverse (cons count acc))]
        [(eq? (token-kind (car tokens)) 'whitespace)
         (loop (cdr tokens)
               (+ count (let ([t (token-text c (car tokens))])
                          (let walk ([i 0] [k 0])
                            (cond
                              [(= i (string-length t)) k]
                              [(char=? (string-ref t i) #\newline) (walk (+ i 1) (+ k 1))]
                              [(char=? (string-ref t i) #\return)
                               (walk (if (and (< (+ i 1) (string-length t)) (char=? (string-ref t (+ i 1)) #\newline)) (+ i 2) (+ i 1))
                                     (+ k 1))]
                              [else (walk (+ i 1) k)]))))
               acc)]
        [else (loop (cdr tokens) 0 (cons count acc))]))))

(define (check-guarantees s)
  (let ([c (cst-of s)])
    (guard (e [(format-refused? e) 'refused])
      (let* ([edits (format-text c default-format-style)] [out (apply-edits s edits)])
        (check-equal? '() (edit-violations s edits))
        (let ([before (line-break-gaps s)] [after (line-break-gaps out)])
          (check (or (equal? before after)
                     (and (equal? (reverse (cdr (reverse before))) (reverse (cdr (reverse after))))
                          (= (car (last-pair before)) 0) (= (car (last-pair after)) 1)))))
        (check-equal? out (formatted out))
        out))))

(test "the default style's rules are the built-in table"
  (check-equal? 0 (indent-rule-for default-format-style "cond"))
  (check-equal? 1 (indent-rule-for default-format-style "define"))
  (check-equal? 2 (indent-rule-for default-format-style "syntax-case"))
  (check-equal? 'let (indent-rule-for default-format-style "let"))
  (check-equal? 'module (indent-rule-for default-format-style "module"))
  (check-equal? 'call (indent-rule-for default-format-style "format"))
  (check-equal? 'author (format-style-call-layout default-format-style))
  (check-equal? 2 (format-style-head-alone default-format-style))
  (check-equal? 'with-code (format-style-comment-lines default-format-style))
  (check-raises assertion-violation?
    (make-format-style (format-style-rules default-format-style) 'author 3 'with-code '())))

(test "bodies are at +2 and distinguished forms align or go at +4"
  (check-equal?
    (lines "(define (f x)"
           "  (let loop ([i 0]"
           "             [acc '()])"
           "    (if (null? x)"
           "        acc"
           "        (loop (+ i 1) acc))))")
    (formatted
      (lines "(define (f x)"
             "(let loop ([i 0]"
             "[acc '()])"
             "(if (null? x)"
             "    acc"
             "(loop (+ i 1) acc))))")))
  (check-equal?
    (lines "(define-record-type point"
           "                    (fields x y)"
           "  (protocol p))"
           "(define-record-type"
           "    point"
           "    (fields x))")
    (formatted (lines "(define-record-type point" "(fields x y)" "(protocol p))" "(define-record-type" "point" "(fields x))")
      (style-with '("define-record-type" . 2))))
  (check-equal?
    (lines "(cond" "  [a b]" "  [c d])" "(cond [a b]" "      [c d])")
    (formatted (lines "(cond" " [a b]" " [c d])" "(cond [a b]" " [c d])"))))

(test "a call keeps the author's layout, read from its first argument line"
  (check-equal?
    (lines "(format \"~d\"" "  a" "  b)" "(foo a" "     b" "     c)")
    (formatted (lines "(format \"~d\"" "      a" "    b)" "(foo a" "     b" "  c)")))
  (check-equal?
    (lines "(foo" " a" " b)" "(foo" "  a" "  b)" "(foo" "  a" "  b)")
    (formatted (lines "(foo" " a" "  b)" "(foo" "  a" " b)" "(foo" "      a" " b)")))
  (check-equal?
    (lines "(foo" "  a)" "(foo a" "     b)")
    (formatted (lines "(foo" "a)" "(foo a" "     b)")
      (make-format-style (format-style-rules default-format-style) 'align 2 'with-code '())))
  (check-equal?
    (lines "(foo a" "     b)" "(foo" " a)")
    (formatted (lines "(foo a" "  b)" "(foo" "    a)")
      (make-format-style (format-style-rules default-format-style) 'align 1 'with-code '()))))

(test "brackets, vectors and lists whose head is not a symbol align under the head"
  (check-equal?
    (lines "(define (f)" "  (let ([x" "         1]" "        [y 2])" "    #(1" "      2)" "    ((f x)" "     y)))")
    (formatted (lines "(define (f)" "(let ([x" "1]" "[y 2])" "#(1" "2)" "((f x)" "y)))"))))

(test "a head that starts its line is at the opener + 1, and closers go where the next element would"
  (check-equal?
    (lines "(" " foo a" "  b" "  )" "(f x" "   )")
    (formatted (lines "(" "foo a" "b" ")" "(f x" ")"))))

(test "named let and named module take two distinguished forms"
  (check-equal?
    (lines "(f (let loop" "        ([i 0])" "     i)" "  (module m" "          (x)" "    (define x 1))" "  (module (x)" "    (define x 1))" "  (let" "      loop" "      ()" "    1))")
    (formatted (lines "(f (let loop" "([i 0])" "i)" "(module m" "(x)" "(define x 1))" "(module (x)" "(define x 1))" "(let" "loop" "()" "1))"))))

(test "rules apply by head name inside quoted data and templates"
  (check-equal? (lines "'(define x" "   1)") (formatted (lines "'(define x" "1)"))))

(test "a top-level form's body written at column 0 stays there"
  (check-equal?
    (lines "(let ()" "(define x 1)" "  (define y 2)" "(define z" "  3)" ")")
    (formatted (lines "(let ()" "(define x 1)" "    (define y 2)" "(define z" "3)" ")")))
  (check-equal?
    (lines "(foo" "  (define x 1))")
    (formatted (lines "(foo" "(define x 1))")))
  (check-equal?
    (lines "(library (a)" "  (export)" "  (import (chezscheme))" "(define x 1))")
    (formatted (lines "(library (a)" "  (export)" "  (import (chezscheme))" "(define x 1))"))))

(test "comment lines move with the code below them"
  (check-equal?
    (lines "(define (f)" " ; outdented" "  (g)" ";;; heading" "  ;; follows the opener" "  )")
    (formatted (lines "(define (f)" "   ; outdented" "    (g)" ";;; heading" "  ;; follows the opener" "  )")))
  (check-equal?
    (lines "(define (f)" " ; one left" "  (g))")
    (formatted (lines "(define (f)" "   ; one left" "    (g))")))
  (check-equal?
    (lines "(f (g x" "      ;; c" "      ))" "(h" ";; c" "  y)")
    (formatted (lines "(f (g x" "       ;; c" "       ))" "(h" ";; c" "      y)")))
  (check-equal?
    (lines "(define (f)" "  ; as code" "  (g))")
    (formatted (lines "(define (f)" "      ; as code" "    (g))")
      (make-format-style (format-style-rules default-format-style) 'author 2 'as-code '()))))

(test "interior spaces: runs kept, inside delimiters removed, touching data separated"
  (check-equal? (lines "(a  b)" "(define (f x) (g x))" "( ; c" " a" "  b)" "('a 'b)" "#;(x)")
                (formatted (lines "( a  b )" "(define(f x)(g x))" "( ; c" " a" "b)" "('a'b)" "#;(x)")))
  (check-equal? (lines "(list 'a" "      \"s\" #\\x)") (formatted (lines "(list 'a" "      \"s\"#\\x)"))))

(test "a datum comment keeps its spelling and its datum is indented as code"
  (check-equal? (lines "#;(k" "    1)") (formatted (lines "#;(k" "1)")))
  (check-equal? (lines "(f a" "   #;b" "   c)") (formatted (lines "(f a" " #;b" "   c)"))))

(test "trailing whitespace and whitespace-only lines are removed, other whitespace kept"
  (check-equal? (lines "(a b)" "" "(c)") (formatted (lines "(a b)  \t" "   \t" "(c)")))
  (check-equal? (lines "(a b)\x3000;" "\x3000; " "(c)") (formatted (lines "(a b) \x3000;" "\x3000; " "(c)")))
  (check-equal? (lines "(a" " \f b)") (formatted (lines "(a" " \f b)")))
  (check-equal? (lines "(f \"a  " "   b\"  " "  c)") (formatted (lines "(f \"a  " "   b\"  " "c)"))))

(test "a final line break is added in the file's style, and trailing blank lines are kept"
  (check-equal? "(a)\n" (formatted "(a)"))
  (check-equal? "(a\r\n  b)\r\n" (formatted "(a\r\nb)"))
  (check-equal? "(a)\n\n\n" (formatted "(a)\n\n\n"))
  (check-equal? "(a)\n" (formatted "(a)   "))
  (check-equal? "(a)\n" (formatted "(a)\n   "))
  (check-equal? "; c\n" (formatted "; c"))
  (check-equal? "" (formatted ""))
  (check-equal? "" (formatted "   "))
  (check-equal? "(a)\n#!eof\n  junk  " (formatted "(a)\n  #!eof\n  junk  ")))

(test "tabs count to the next multiple of eight and stay where the line is not moved"
  (check-equal? (lines "(abcdef a" "\tb)") (formatted (lines "(abcdef a" "\tb)")))
  (check-equal? (lines "(foo\tx" "\ty)") (formatted (lines "(foo\tx" "\ty)")))
  (check-equal? (lines "(let ()" "  (foo\tx" "        y))") (formatted (lines "(let ()" "\t(foo\tx" "\t\ty))")))
  (check-equal? (lines "(let ()" "  (foo\tx" "    y))") (formatted (lines "(let ()" "\t(foo\tx" "\t y))"))))

(test "strings, block comments, |…| symbols and their continuation lines are untouched"
  (let ([s (lines "(f \"a" "      b\"" "   #| x" "  y |#" " |p" "    q|" "  z)")])
    (check-equal? (lines "(f \"a" "      b\"" "   #| x" "  y |#" "  |p" "    q|" "  z)") (formatted s))))

(test "a line Chez ends a comment on is one line, with U+2028 kept as whitespace"
  (check-equal? (lines "(a ;c\x2028;b" "      d)") (formatted (lines "(a ;c\x2028;b" "      d)")))
  (check-equal? (lines "(a ;c\x2028;b" "  d)") (formatted (lines "(a ;c\x2028;b" "d)"))))

(test "a space or line break that would change a token is left out, not the whole text refused"
  (check-equal? "a\\" (formatted "a\\"))
  (check-equal? "(n #{#lib x })\n" (formatted "(n #{#lib x})"))
  (check-equal? "(a\n  b\\\n  c)\n" (formatted "(a\n   b\\\n  c)\n"))
  (check-equal? "(a b\\\n c\n  d)\n" (formatted "(a b\\\n c\nd)\n")))

(test "only spaces and tabs change, every line break stays, and a second pass changes nothing"
  (for-each check-guarantees
    (list (lines "(define (f x)" "(let ([y 1]" "[z 2])" "(if x" "y" "z)))")
          (lines "( a  b )   " "(define(f x)(g x))" "" "   " "'a'b")
          "(a\tb\n\t\tc)\n(d ;c\x2028;e\n  f)"
          "(a)")))

(test "an edit that changes how the text reads is refused, whole or within its lines"
  (let* ([s (lines "(define (f x)" "  (g x \"a b\" |c d|)) ; e")] [c (cst-of s)])
    (define (at text) (let loop ([i 0]) (if (string=? (substring s i (+ i (string-length text))) text) i (loop (+ i 1)))))
    (define (refuses? edits)
      (define (refused? thunk)
        (guard (e [(format-refused? e) (eq? 'internal-error (diagnostic-rule (format-refused-diagnostic e)))])
          (thunk)
          #f))
      (let ([whole (refused? (lambda () (check-format-edits c edits)))]
            [window (or (exists (lambda (e) (= (text-edit-start e) (string-length s))) edits)
                        (let* ([t (make-text s)]
                               [lines (map (lambda (e) (text-offset-line t (text-edit-start e))) edits)])
                          (if (null? lines)
                              #f
                              (refused? (lambda () (check-format-edits c edits (apply min lines) (apply max lines)))))))])
        (check-equal? whole window)
        whole))
    (check (refuses? (list (make-text-edit (+ (at "(g x") 2) (+ (at "(g x") 3) ""))))
    (check (refuses? (list (make-text-edit (+ (at "\"a b") 2) (+ (at "\"a b") 3) "  "))))
    (check (refuses? (list (make-text-edit (+ (at "|c d") 2) (+ (at "|c d") 3) ""))))
    (check (refuses? (list (make-text-edit (+ (at "(f x") 2) (+ (at "(f x") 3) "\n"))))
    (check (refuses? (list (make-text-edit (at "(g x") (at "(g x") ";"))))
    (check (refuses? (list (make-text-edit (+ (at "; e") 1) (+ (at "; e") 2) ""))))
    (check (refuses? (list (make-text-edit (at "x)") (at "x)") "#|"))))
    (check (refuses? (list (make-text-edit (string-length s) (string-length s) "\n"))))
    (check (not (refuses? (list (make-text-edit (+ (at "(g x") 2) (+ (at "(g x") 3) "   ")))))
    (check (not (refuses? '())))))

(test "the result depends only on the text and the style"
  (let ([s (lines "(define (f x)" "(g x" "y))")])
    (define (edit-list s)
      (map (lambda (e) (list (text-edit-start e) (text-edit-end e) (text-edit-replacement e)))
           (format-text (cst-of s) default-format-style)))
    (check-equal? (edit-list s) (edit-list (string-copy s)))
    (check-equal? (formatted s)
      (parameterize ([current-directory "/"]) (formatted s)))))

(test "broken input is refused as a whole, with the reader's first structural diagnostic"
  (define (refusal s)
    (guard (e [(format-refused? e)
               (let ([d (format-refused-diagnostic e)]) (list (diagnostic-start d) (diagnostic-message d)))])
      (format-text (cst-of s) default-format-style)
      'formatted))
  (check-equal? '(0 "unexpected end-of-file reading list") (refusal (lines "(let ([y 1]" "(define (h)" "2)")))
  (check-equal? '(3 "unexpected close parenthesis") (refusal (lines "(a))" "(b)")))
  (check-equal? '(0 "unexpected end-of-file reading list") (refusal (lines "(a \"bc" "(d)")))
  (check-equal? '(4 "unexpected end-of-file reading list") (refusal (lines "(a)" "(b \"bc")))
  (check-equal? '(4 "unexpected end-of-file reading string") (refusal (lines "(a)" "\"bc")))
  (check-equal? '(4 "unexpected end-of-file reading block comment") (refusal (lines "(a)" "#| x" "(d)")))
  (check-equal? '(4 "unexpected end-of-file reading symbol") (refusal (lines "(a)" "|bc")))
  (check-equal? '(0 "parenthesized list terminated by bracket") (refusal (lines "(a]")))
  (check-equal? '(8 "non-octet found in bytevector") (refusal (lines "#vu8(1 2]")))
  (check-equal? 'formatted (refusal (lines "#;(k" "1)"))))

(test "a range is cut above the first structural problem and refused at or after it"
  (let* ([s (lines "(define (f)" "1)" "(g" "2)" ")" "(h" "3)")] [c (cst-of s)])
    (check-equal? (lines "(define (f)" "  1)" "(g" "  2)" ")" "(h" "3)")
      (apply-edits s (format-lines c default-format-style 0 6)))
    (check-raises format-refused? (format-lines c default-format-style 4 6))
    (check-equal? (lines "(define (f)" "1)" "(g" "  2)" ")" "(h" "3)")
      (apply-edits s (format-lines c default-format-style 3 3))))
  (check-equal? (lines "(a" "  b)" "(c")
    (let ([s (lines "(a" "b)" "(c")]) (apply-edits s (format-lines (cst-of s) default-format-style 0 1)))))

(test "range formatting changes only its lines, from the layout above it"
  (let* ([s (lines "(define (f)" "     (g" "x" "y)" "z)")] [c (cst-of s)])
    (check-equal? (lines "(define (f)" "     (g" "       x" "       y)" "z)")
      (apply-edits s (format-lines c default-format-style 2 3)))
    (check-raises assertion-violation? (format-lines c default-format-style 3 2))
    (check-raises assertion-violation? (format-lines c default-format-style 0 6))))

(test "disabled regions and the skip marker leave their lines alone but keep their frames"
  (check-equal?
    (lines "(define (f)" "  ;; chez-fmt: off" "     (g   x" "  y)" "  ;; chez-fmt: on" "  (h" "   x))")
    (formatted (lines "(define (f)" "  ;; chez-fmt: off" "     (g   x" "  y)" "  ;; chez-fmt: on" "(h" " x))")))
  (check-equal?
    (lines "(define (f)" "  ;; chez-fmt: off" "     (g   x" "  y))" "    " "  (h)")
    (formatted (lines "(define (f)" "  ;; chez-fmt: off" "     (g   x" "  y))" "    " "  (h)")))
  (check-equal?
    (lines "(define t" "    ; chez-fmt: skip" "  '([a    1]" "       [bb 2])" "  (g" "   1))")
    (formatted (lines "(define t" "    ; chez-fmt: skip" "  '([a    1]" "       [bb 2])" "(g" " 1))")))
  (check-equal?
    (lines "(f" "    ; chez-fmt: skip" "  )")
    (formatted (lines "(f" "  ; chez-fmt: skip" ")")))
  (check-equal?
    (lines "(f" "    ; chez-fmt: skipped" "  x)")
    (formatted (lines "(f" "  ; chez-fmt: skipped" "x)"))))

(test "a marker followed by spaces or tabs is still a marker"
  (for-each
    (lambda (trailing)
      (check-equal?
        (lines (string-append ";; chez-fmt: off" trailing) "(f" "     x)" (string-append ";; chez-fmt: on" trailing) "(g" "  y)")
        (formatted (lines (string-append ";; chez-fmt: off" trailing) "(f" "     x)" (string-append ";; chez-fmt: on" trailing) "(g" "y)")))
      (check-equal?
        (lines (string-append ";; chez-fmt: skip" trailing) "(f" "     x)" "(g" "  y)")
        (formatted (lines (string-append ";; chez-fmt: skip" trailing) "(f" "     x)" "(g" "y)"))))
    '(" " "\t" " \t ")))

(test "format-line-indentation indents a new line inside unclosed forms as whole-file formatting would"
  (define (indentation-of s line)
    (let ([edits (format-line-indentation (cst-of s) default-format-style line)])
      (if (null? edits)
          'none
          (string-length (text-edit-replacement (car edits))))))
  (define (stable? before element after)
    (let* ([target (indentation-of (string-append before "\n") (+ 1 (length (filter (lambda (c) (char=? c #\newline)) (string->list before)))))]
           [completed (string-append before "\n" (make-string target #\space) element after "\n")])
      (and (fixnum? target) (string=? completed (formatted completed)))))
  (check (stable? "(define (f x)" "(g x)" ")"))
  (check (stable? "(let ([x 1])" "x" ")"))
  (check (stable? "(cond\n  [(f x)" "(g x)" "])"))
  (check (stable? "(foo a" "b" ")"))
  (check (stable? "(foo" "a" ")"))
  (check (stable? "(define (f)\n  (h a\n     b)" "(g)" ")"))
  (check-equal? 2 (indentation-of "(define (f x)\n" 1))
  (check-equal? 5 (indentation-of "(foo a\n" 1))
  (check-equal? 2 (indentation-of "(foo\n" 1))
  (check-equal? 2 (indentation-of "(define (f x)\n     " 1))
  (check-equal? 'none (indentation-of "(define (f x)\n  " 1))
  (check-equal? 5 (indentation-of "(define (f x)\n  (g x\n  " 2)))

(test "format-line-indentation reads only the lines above, and gives nothing where it cannot know"
  (define (edits s line) (format-line-indentation (cst-of s) default-format-style line))
  (check-equal? '() (edits (lines "(a))" "" "(b") 1))
  (check-equal? '() (edits (lines "(f \"a" "  b\")") 1))
  (check-equal? '() (edits (lines "#| a" "  b |#") 1))
  (check-equal? '() (edits (lines ";; chez-fmt: off" "(f" "    x)") 2))
  (check-equal? '() (edits (lines "  #!r6rs" "(f)") 0))
  (check-equal? 1 (length (edits (lines "(foo a" "  b" "  c)") 1)))
  (check-equal? 5 (string-length (text-edit-replacement (car (edits (lines "(foo a" "  b" "  c)") 1)))))
  (check-equal? 2 (string-length (text-edit-replacement (car (edits (lines "(foo a" "  b" "c)") 2)))))
  (check-equal? 2 (string-length (text-edit-replacement (car (edits (lines "(define (f)" "  (g x)" ")") 2)))))
  (check-equal? 0 (string-length (text-edit-replacement (car (edits (lines "(let ()" "(define x 1)" "    ") 2)))))
  (check-equal? 2 (string-length (text-edit-replacement (car (edits (lines "(foo" "    ; c" "  x)") 1))))))

(test "a project's format section overrides the defaults, nearer files winning"
  (let ([root (format "/tmp/chez-tools-format-test-~a" (process-id))])
    (system (format "rm -rf ~a; mkdir -p ~a/a/b" root root))
    (call-with-output-file (string-append root "/a/.chez-tools")
      (lambda (out)
        (put-string out "(root)\n(format (call-layout align) (head-alone 1) (indent (mat 1) ((with-x with-y) 0) (2 nanopass-case) (define #f) (my-let let)))\n"))
      'truncate)
    (call-with-output-file (string-append root "/a/b/.chez-tools")
      (lambda (out) (put-string out "(format (comment-lines as-code) (column-zero-heads begin) (indent (mat 2) (with-y -1)))\n"))
      'truncate)
    (let ([style (config-format-style (find-project-config (string-append root "/a/b/x.ss") root))])
      (check-equal? 'align (format-style-call-layout style))
      (check-equal? 1 (format-style-head-alone style))
      (check-equal? 'as-code (format-style-comment-lines style))
      (check-equal? '("begin") (format-style-column-zero-heads style))
      (check-equal? 2 (indent-rule-for style "mat"))
      (check-equal? 0 (indent-rule-for style "with-x"))
      (check-equal? 'call (indent-rule-for style "with-y"))
      (check-equal? 2 (indent-rule-for style "nanopass-case"))
      (check-equal? 'call (indent-rule-for style "define"))
      (check-equal? 'let (indent-rule-for style "my-let"))
      (check-equal? 0 (indent-rule-for style "cond")))
    (check-equal? default-format-style default-format-style)
    (let ([style (config-format-style (default-project-config root))])
      (check-equal? 'author (format-style-call-layout style))
      (check-equal? 1 (indent-rule-for style "define")))
    (system (format "rm -rf ~a" root))))

;; Where whole-file formatting of a text changes only lines a to b, range
;; formatting of those lines must give the same text.
(define (range-agrees? s a b)
  (let* ([whole (formatted s)] [t (make-text s)] [w (make-text whole)])
    (define (line-text t i) (substring (text-string t) (text-line-start t i) (text-line-end t i)))
    (or (exists (lambda (i) (and (or (< i a) (> i b)) (< i (text-line-count t)) (not (string=? (line-text t i) (line-text w i)))))
                (iota (text-line-count t)))
        (string=? whole (apply-edits s (format-lines (cst-of s) default-format-style a b))))))

(define (mixed formatted-text original a b)
  (let ([f (make-text formatted-text)] [o (make-text original)])
    (string-append (substring formatted-text 0 (text-line-start f a))
                   (substring original (text-line-start o a) (text-line-end o b))
                   (substring formatted-text (text-line-end f b) (string-length formatted-text)))))

(test "on generated texts: the guarantees hold, ranges agree with whole-file formatting, and on-type indentation never raises"
  (random-seed 20261110)
  (let loop ([i 0] [refused 0])
    (if (= i 500)
        (check (< refused 100))
        (let* ([s (random-scheme-text)] [out (check-guarantees s)])
          (if (eq? out 'refused)
              (loop (+ i 1) (+ refused 1))
              (let* ([lines (text-line-count (make-text s))] [a (random lines)] [b (+ a (random (- lines a)))])
                (check-equal? '() (format-lines (cst-of out) default-format-style a (min b (- (text-line-count (make-text out)) 1))))
                (check (range-agrees? (mixed out s a b) a b))
                (let ([c (cst-of s)])
                  (do ([line 0 (+ line 1)]) ((= line lines))
                    (check (let ([edits (format-line-indentation c default-format-style line)])
                             (and (list? edits) (< (length edits) 2))))))
                (loop (+ i 1) refused)))))))

(test-exit)
