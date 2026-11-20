(library (czfmt)
  (export
    ;; styles
    make-format-style format-style? format-style-rules format-style-call-layout
    format-style-head-alone format-style-comment-lines format-style-column-zero-heads
    call-layout call-layout-set comment-lines comment-lines-set
    default-format-style config-format-style
    ;; rules
    indent-rule-for
    ;; formatting
    format-text format-lines format-line-indentation check-format-edits
    &format-refused make-format-refused format-refused? format-refused-diagnostic)
  (import (chezscheme) (analysis reader) (analysis text) (analysis config)
          (analysis diagnostic))

  (define-enumeration call-layout (author align) call-layout-set)
  (define-enumeration comment-lines (with-code as-code) comment-lines-set)

  (define-condition-type &format-refused &error make-format-refused format-refused?
    (diagnostic format-refused-diagnostic))

  (define (indent-rule? x)
    (or (memq x '(call let module)) (and (fixnum? x) (fx>= x 0))))

  (define-record-type format-style
    (nongenerative) (sealed #t)
    (fields rules call-layout head-alone comment-lines column-zero-heads)
    (protocol
      (lambda (new)
        (lambda (rules call-layout head-alone comment-lines column-zero-heads)
          (unless (and (hashtable? rules)
                       (let-values ([(names values) (hashtable-entries rules)])
                         (and (for-all string? (vector->list names))
                              (for-all indent-rule? (vector->list values)))))
            (assertion-violationf 'make-format-style "~s is not a table from head names to indent rules" rules))
          (unless (memq call-layout '(author align))
            (assertion-violationf 'make-format-style "~s is not author or align" call-layout))
          (unless (memv head-alone '(1 2))
            (assertion-violationf 'make-format-style "~s is not 1 or 2" head-alone))
          (unless (memq comment-lines '(with-code as-code))
            (assertion-violationf 'make-format-style "~s is not with-code or as-code" comment-lines))
          (unless (and (list? column-zero-heads) (for-all string? column-zero-heads))
            (assertion-violationf 'make-format-style "~s is not a list of head names" column-zero-heads))
          (new (hashtable-copy rules #f) call-layout head-alone comment-lines column-zero-heads)))))

  (define built-in-rules
    '((0 "case-lambda" "cond" "exclusive-cond" "meta-cond" "begin" "identifier-syntax" "top-level-program"
         "critical-section" "with-interrupts-disabled" "delay" "dynamic-wind" "call-with-values")
      (1 "lambda" "let*" "letrec" "letrec*" "let-values" "let*-values" "let-syntax" "letrec-syntax"
         "fluid-let" "fluid-let-syntax" "parameterize" "with-syntax" "with-implicit" "rec" "define"
         "define-syntax" "define-values" "define-record-type" "define-condition-type" "define-ftype"
         "define-enumeration" "define-record" "define-structure" "set!" "when" "unless" "guard" "case"
         "eval-when" "record-case" "with-mutex" "syntax-rules" "library")
      (2 "trace-lambda" "define-property" "do" "syntax-case" "with-continuation-mark")
      (let "let" "trace-let")
      (module "module")))

  (define (rule-table entries)
    (let ([rules (make-hashtable string-hash string=?)])
      (for-each (lambda (entry) (for-each (lambda (name) (hashtable-set! rules name (car entry))) (cdr entry)))
        entries)
      rules))

  (define default-format-style
    (make-format-style (rule-table built-in-rules) 'author 2 'with-code
      '("library" "module" "begin" "let" "let*" "eval-when" "top-level-program")))

  ;; scmindent's procedure style, written #f or a negative count, is the
  ;; call layout.
  (define (scmindent-rule x)
    (if (or (not x) (and (fixnum? x) (fx< x 0))) 'call x))

  ;; The configuration lists the nearest file's entries first, so indent
  ;; entries are applied from the farthest file on for a nearer file's rule
  ;; to replace a farther one's.
  (define (config-format-style config)
    (unless (config? config) (assertion-violationf 'config-format-style "~s is not a configuration" config))
    (let ([entries (config-format config)] [rules (hashtable-copy (format-style-rules default-format-style) #t)])
      (define (setting key default) (cond [(assq key entries) => cdr] [else default]))
      (for-each
        (lambda (entry)
          (let ([rule (scmindent-rule (cdr entry))])
            (for-each (lambda (name) (hashtable-set! rules name rule)) (car entry))))
        (apply append (reverse (map cdr (filter (lambda (e) (eq? (car e) 'indent)) entries)))))
      (make-format-style rules
        (setting 'call-layout (format-style-call-layout default-format-style))
        (setting 'head-alone (format-style-head-alone default-format-style))
        (setting 'comment-lines (format-style-comment-lines default-format-style))
        (setting 'column-zero-heads (format-style-column-zero-heads default-format-style)))))

  (define (indent-rule-for style name)
    (unless (format-style? style) (assertion-violationf 'indent-rule-for "~s is not a format style" style))
    (unless (string? name) (assertion-violationf 'indent-rule-for "~s is not a head name" name))
    (hashtable-ref (format-style-rules style) name 'call))

  (define (datum-start x) (if (token? x) (token-start x) (node-start x)))
  (define (datum-end x) (if (token? x) (token-end x) (node-end x)))

  (define (closed? x)
    (let* ([children (node-children x)] [last (vector-ref children (fx- (vector-length children) 1))])
      (and (fx> (vector-length children) 1) (token? last) (eq? (token-kind last) 'close))))

  (define (spaces-and-tabs? s start end)
    (let loop ([i start])
      (or (fx= i end)
          (and (memv (string-ref s i) '(#\space #\tab)) (loop (fx+ i 1))))))

  (define (advance-column col s start end)
    (let loop ([i start] [col col])
      (if (fx= i end)
          col
          (loop (fx+ i 1) (if (char=? (string-ref s i) #\tab) (fx* 8 (fx+ (fxquotient col 8) 1)) (fx+ col 1))))))

  (define (line-break-count s start end)
    (let loop ([i start] [count 0])
      (if (fx= i end)
          count
          (let ([c (string-ref s i)])
            (cond
              [(char=? c #\newline) (loop (fx+ i 1) (fx+ count 1))]
              [(char=? c #\return)
               (loop (if (and (fx< (fx+ i 1) end) (char=? (string-ref s (fx+ i 1)) #\newline)) (fx+ i 2) (fx+ i 1))
                     (fx+ count 1))]
              [else (loop (fx+ i 1) count)])))))

  (define-record-type run
    (nongenerative) (sealed #t)
    (fields cst text string tokens style layouts out-columns (mutable root) (mutable disabled)))

  (define-record-type layout
    (nongenerative) (sealed #t)
    (fields node parent data indices kind rule (mutable decision)))

  (define (new-run c style out-columns?)
    (let* ([tokens (cst-tokens c)]
           [r (make-run c (cst-text c) (text-string (cst-text c)) tokens style (make-eq-hashtable)
                (and out-columns? (make-fxvector (vector-length tokens) -1)) #f #f)])
      (run-root-set! r (layout-of r (cst-root c) #f))
      r))

  (define (line-of r offset) (text-offset-line (run-text r) offset))

  (define (input-column r offset)
    (let ([start (text-line-start (run-text r) (line-of r offset))])
      (advance-column 0 (run-string r) start offset)))

  (define (leads-line? r offset)
    (let ([s (run-string r)] [start (text-line-start (run-text r) (line-of r offset))])
      (let loop ([i (fx- offset 1)])
        (or (fx< i start) (and (char-whitespace? (string-ref s i)) (loop (fx- i 1)))))))

  (define (token-index r offset)
    (let ([v (run-tokens r)])
      (let loop ([lo 0] [hi (vector-length v)])
        (if (fx= (fx+ lo 1) hi)
            lo
            (let ([mid (fxsrl (fx+ lo hi) 1)])
              (if (fx<= (token-start (vector-ref v mid)) offset) (loop mid hi) (loop lo mid)))))))

  (define (output-column r offset)
    (let ([cols (run-out-columns r)])
      (if cols
          (let ([col (fxvector-ref cols (token-index r offset))])
            (if (fx>= col 0) col (input-column r offset)))
          (input-column r offset))))

  (define (symbol-head? x) (and (token? x) (memq (token-kind x) '(symbol gensym primitive))))

  (define (layout-of r x parent)
    (or (hashtable-ref (run-layouts r) x #f)
        (let* ([children (node-children x)]
               [data (node-data x)]
               [indices (make-fxvector (vector-length children))]
               [style (run-style r)])
          (let loop ([i 0] [k 0])
            (when (fx< i (vector-length children))
              (let ([child (vector-ref children i)])
                (fxvector-set! indices i k)
                (loop (fx+ i 1)
                      (if (and (fx< k (vector-length data)) (eq? child (vector-ref data k))) (fx+ k 1) k)))))
          (let-values ([(kind rule)
                        (cond
                          [(eq? (node-kind x) 'root) (values 'root #f)]
                          [(and (eq? (token-subkind (vector-ref children 0)) 'paren)
                                (fx> (vector-length data) 0)
                                (symbol-head? (vector-ref data 0)))
                           (let ([rule (indent-rule-for style (symbol-token-name (run-cst r) (vector-ref data 0)))])
                             (values 'symbol-head
                               (if (memq rule '(let module))
                                   (if (and (fx> (vector-length data) 1) (token? (vector-ref data 1))
                                            (eq? (token-kind (vector-ref data 1)) 'symbol))
                                       2
                                       1)
                                   rule)))]
                          [else (values 'under-head #f)])])
            (let ([l (make-layout x parent data indices kind rule 'unknown)])
              (hashtable-set! (run-layouts r) x l)
              l)))))

  (define (layout-start l) (node-start (layout-node l)))
  (define (layout-root? l) (eq? (layout-kind l) 'root))

  ;; An unclosed list also holds the offset at its end, where text typed
  ;; into it goes.
  (define (child-at children offset)
    (let loop ([lo -1] [hi (vector-length children)])
      (if (fx= (fx+ lo 1) hi)
          (and (fx>= lo 0)
               (let ([child (vector-ref children lo)])
                 (and (or (fx< offset (datum-end child))
                          (and (fx= offset (datum-end child)) (node? child) (not (closed? child))))
                      lo)))
          (let ([mid (fxsrl (fx+ lo hi) 1)])
            (if (fx<= (datum-start (vector-ref children mid)) offset) (loop mid hi) (loop lo mid))))))

  (define (context-at r offset)
    (let loop ([l (run-root r)])
      (let* ([children (node-children (layout-node l))] [i (child-at children offset)])
        (if (not i)
            (values l (vector-length (layout-data l)))
            (let ([child (vector-ref children i)] [k (fxvector-ref (layout-indices l) i)])
              (cond
                [(or (token? child) (fx= offset (node-start child))) (values l k)]
                [(eq? (node-kind child) 'list) (loop (layout-of r child l))]
                [else
                 (let inner ([x child])
                   (let* ([grand (node-children x)] [j (child-at grand offset)] [y (and j (vector-ref grand j))])
                     (cond
                       [(or (not y) (token? y) (fx= offset (node-start y))) (values l k)]
                       [(eq? (node-kind y) 'list) (loop (layout-of r y l))]
                       [else (inner y)])))]))))))

  (define (element-start l i) (datum-start (vector-ref (layout-data l) i)))

  (define (on-head-line? r l i)
    (and (fx< i (vector-length (layout-data l)))
         (fx= (line-of r (element-start l i)) (line-of r (element-start l 0)))))

  (define (author-decision r l limit)
    (let ([data (layout-data l)])
      (let ([head-line (line-of r (element-start l 0))])
        (let loop ([i 1])
          (and (fx< i (vector-length data))
               (let* ([start (element-start l i)] [line (line-of r start)])
                 (cond
                   [(and limit (fx>= line limit)) #f]
                   [(and (fx> line head-line) (leads-line? r start))
                    (let ([col (input-column r start)] [column-of (if (fixed-line? r line start) output-column input-column)])
                      (if (on-head-line? r l 1)
                          (if (fx= col (column-of r (element-start l 1))) 'align 'hang)
                          (case (fx- col (column-of r (layout-start l)))
                            [(1) 'plus1]
                            [(2) 'plus2]
                            [else 'other])))]
                   [else (loop (fx+ i 1))])))))))

  ;; A first argument line that formatting leaves where it is keeps its
  ;; column while its opener may move, so it is read against the output,
  ;; or formatting the output again would decide otherwise.
  (define (fixed-line? r line start)
    (let ([disabled (run-disabled r)])
      (and disabled
           (or (fx= (bytevector-u8-ref disabled line) 1)
               (not (spaces-and-tabs? (run-string r) (text-line-start (run-text r) line) start))))))

  (define (call-decision r l limit)
    (if limit
        (author-decision r l limit)
        (let ([d (layout-decision l)])
          (if (eq? d 'unknown)
              (let ([d (author-decision r l #f)]) (layout-decision-set! l d) d)
              d))))

  (define (element-column r l k column-of limit)
    (let ([opener (column-of (layout-start l))] [style (run-style r)])
      (define (under i) (column-of (element-start l i)))
      (cond
        [(fx= k 0) (fx+ opener 1)]
        [(eq? (layout-kind l) 'under-head) (under 0)]
        [(eq? (layout-rule l) 'call)
         (let ([decision (and (eq? (format-style-call-layout style) 'author) (call-decision r l limit))])
           (if (on-head-line? r l 1)
               (if (memq decision '(#f align)) (under 1) (fx+ opener 2))
               (case decision
                 [(plus1) (fx+ opener 1)]
                 [(plus2) (fx+ opener 2)]
                 [else (fx+ opener (format-style-head-alone style))])))]
        [else
         (let ([n (layout-rule l)])
           (if (fx<= k n)
               (if (on-head-line? r l 1) (under 1) (fx+ opener 4))
               (if (on-head-line? r l (fx+ n 1)) (under (fx+ n 1)) (fx+ opener 2))))])))

  (define (body-index? l k)
    (and (eq? (layout-kind l) 'symbol-head)
         (let ([rule (layout-rule l)]) (if (eq? rule 'call) (fx>= k 1) (fx> k rule)))))

  (define (column-zero-body? r l k)
    (let ([parent (layout-parent l)])
      (and parent (layout-root? parent) (body-index? l k)
           (member (symbol-token-name (run-cst r) (vector-ref (layout-data l) 0))
                   (format-style-column-zero-heads (run-style r)))
           #t)))

  (define (code-target r l k input-col column-of limit)
    (cond
      [(layout-root? l) 0]
      [(and (fx= input-col 0) (column-zero-body? r l k)) 0]
      [else (element-column r l k column-of limit)]))

  ;; A line comment's token holds the spaces and tabs at the end of its
  ;; line, which formatting keeps.
  (define (marker-text r tok)
    (let* ([s (run-string r)]
           [end (let trim ([end (token-end tok)])
                  (if (and (fx> end (token-start tok)) (memv (string-ref s (fx- end 1)) '(#\space #\tab)))
                      (trim (fx- end 1))
                      end))])
      (let loop ([i (token-start tok)])
        (if (and (fx< i end) (char=? (string-ref s i) #\;))
            (loop (fx+ i 1))
            (let skip ([i i])
              (if (and (fx< i end) (char=? (string-ref s i) #\space))
                  (skip (fx+ i 1))
                  (substring s i end)))))))

  ;; Chez also ends a line comment at U+2028 or U+0085, which do not end an
  ;; LSP line, so code can follow a comment on its line.
  (define (alone-on-line? r tok)
    (let* ([t (run-text r)] [s (run-string r)] [end (text-line-end t (line-of r (token-start tok)))])
      (and (leads-line? r (token-start tok))
           (let loop ([i (token-end tok)])
             (or (fx>= i end) (and (char-whitespace? (string-ref s i)) (loop (fx+ i 1))))))))

  (define (disabled-lines r limit)
    (let* ([t (run-text r)] [flags (make-bytevector (text-line-count t) 0)] [tokens (run-tokens r)]
           [end (if (and limit (fx< limit (text-line-count t))) (text-line-start t limit) (fx+ (text-length t) 1))])
      (define (mark! from to)
        (do ([j from (fx+ j 1)]) ((fx> j to)) (bytevector-u8-set! flags j 1)))
      (let loop ([i 0] [off-line #f])
        (if (or (fx= i (vector-length tokens)) (fx>= (token-start (vector-ref tokens i)) end))
            (when off-line (mark! off-line (fx- (text-line-count t) 1)))
            (let ([tok (vector-ref tokens i)])
              (if (and (eq? (token-kind tok) 'line-comment) (alone-on-line? r tok))
                  (let ([text (marker-text r tok)] [line (line-of r (token-start tok))])
                    (cond
                      [(and (not off-line) (string=? text "czfmt: off")) (loop (fx+ i 1) line)]
                      [(and off-line (string=? text "czfmt: on")) (mark! off-line line) (loop (fx+ i 1) #f)]
                      [(and (not off-line) (string=? text "czfmt: skip"))
                       (let-values ([(l k) (context-at r (token-start tok))])
                         (when (fx< k (vector-length (layout-data l)))
                           (mark! line (line-of r (fx- (datum-end (vector-ref (layout-data l) k)) 1)))))
                       (loop (fx+ i 1) off-line)]
                      [else (loop (fx+ i 1) off-line)]))
                  (loop (fx+ i 1) off-line)))))
      flags))

  ;; The reader reports a wrong closer at its list or at the closer itself,
  ;; so both places are kept.
  (define-record-type problem
    (nongenerative) (sealed #t)
    (fields kind start reported-at))

  (define (structural-problems c)
    (let ([problems '()])
      (define (add! kind start reported-at) (set! problems (cons (make-problem kind start reported-at) problems)))
      (vector-for-each
        (lambda (tok)
          (cond
            [(enum-set-member? 'unterminated (token-flags tok)) (add! 'unterminated (token-start tok) (list (token-start tok)))]
            [(enum-set-member? 'unmatched (token-flags tok)) (add! 'unmatched (token-start tok) (list (token-start tok)))]))
        (cst-tokens c))
      (let walk ([x (cst-root c)])
        (when (node? x)
          (when (eq? (node-kind x) 'list)
            (let* ([children (node-children x)] [opener (vector-ref children 0)])
              (if (closed? x)
                  (let ([closer (vector-ref children (fx- (vector-length children) 1))])
                    (unless (eq? (token-subkind closer) (if (memq (token-subkind opener) '(bracket record)) 'bracket 'paren))
                      (add! 'wrong-closer (node-start x) (list (node-start x) (token-start closer)))))
                  (add! 'unclosed (node-start x) (list (node-start x))))))
          (vector-for-each walk (node-children x))))
      problems))

  (define (first-problem c)
    (let ([problems (structural-problems c)])
      (and (pair? problems)
           (fold-left (lambda (best p) (if (fx< (problem-start p) (problem-start best)) p best))
             (car problems) (cdr problems)))))

  (define (problem-diagnostic c p)
    (or (find (lambda (d) (and (eq? (diagnostic-severity d) (severity error)) (memv (diagnostic-start d) (problem-reported-at p))))
              (vector->list (cst-diagnostics c)))
        (make-diagnostic 'read-error (severity error) (problem-start p) (problem-start p)
          "the text does not read as complete data" '() (diagnostic-tag-set))))

  (define (refuse diagnostic)
    (raise (condition (make-format-refused diagnostic) (make-message-condition (diagnostic-message diagnostic)))))

  (define (internal-refusal message)
    (refuse (make-diagnostic 'internal-error (severity error) 0 0 message '() (diagnostic-tag-set))))

  (define (final-line-break s)
    (let loop ([i 0])
      (cond
        [(fx= i (string-length s)) "\n"]
        [(char=? (string-ref s i) #\newline) "\n"]
        [(char=? (string-ref s i) #\return)
         (if (and (fx< (fx+ i 1) (string-length s)) (char=? (string-ref s (fx+ i 1)) #\newline)) "\r\n" "\r")]
        [else (loop (fx+ i 1))])))

  (define (line-kind r line)
    (let* ([t (run-text r)] [tokens (run-tokens r)] [ls (text-line-start t line)] [le (text-line-end t line)])
      (if (fx= ls (string-length (run-string r)))
          (values 'blank #f)
          (let* ([i (token-index r ls)] [tok (vector-ref tokens i)])
            (cond
              [(and (fx< (token-start tok) ls) (not (eq? (token-kind tok) 'whitespace))) (values 'continuation #f)]
              ;; A token can end with the line break, as a symbol ending in an
              ;; escaped one does; the indentation after it is then all that
              ;; separates it from the next token.
              [(and (fx= (token-start tok) ls) (fx> i 0) (not (eq? (token-kind (vector-ref tokens (fx- i 1))) 'whitespace)))
               (values 'continuation #f)]
              [(and (eq? (token-kind tok) 'whitespace) (fx>= (token-end tok) le)) (values 'blank #f)]
              [else
               (let* ([i (if (eq? (token-kind tok) 'whitespace) (fx+ i 1) i)] [first-token (vector-ref tokens i)])
                 (case (token-kind first-token)
                   [(block-comment directive script-header unread-tail) (values 'fixed i)]
                   [(line-comment) (values (if (alone-on-line? r first-token) 'comment 'fixed) i)]
                   [else (values 'code i)]))])))))

  (define-record-type comment-line
    (nongenerative) (sealed #t)
    (fields line token layout index))

  (define (pass-edits r first last)
    (let* ([t (run-text r)] [s (run-string r)] [n (string-length s)] [tokens (run-tokens r)]
           [token-count (vector-length tokens)] [style (run-style r)] [cols (run-out-columns r)]
           [line-count (text-line-count t)]
           [disabled (let ([flags (disabled-lines r (fx+ last 1))]) (run-disabled-set! r flags) flags)]
           [deltas (make-fxvector line-count 0)]
           [owners (make-vector line-count #f)]
           [edits (make-vector line-count '())]
           [column-of (lambda (offset) (output-column r offset))])
      (define (edit! line start end replacement)
        (vector-set! edits line (cons (make-text-edit start end replacement) (vector-ref edits line))))
      (define (enabled? line) (fx= (bytevector-u8-ref disabled line) 0))
      (define (touching-sibling? offset)
        (let-values ([(l k) (context-at r offset)])
          (and (not (layout-root? l)) (fx> k 0)
               (fx< k (vector-length (layout-data l)))
               (fx= (element-start l k) offset)
               (fx= (datum-end (vector-ref (layout-data l) (fx- k 1))) offset))))
      ;; Where the reader stopped at an unexpected character, a space can
      ;; change what it reads, so the two tokens are read again with it.
      (define (separable? previous tok)
        (let ([c2 (read-cst (make-text (string-append (substring s (token-start previous) (token-end previous)) " "
                                         (substring s (token-start tok) (token-end tok))))
                    (cst-mode-at (run-cst r) (token-start previous)))])
          (equal? (map (lambda (x) (list (token-kind x) (token-subkind x) (token-text c2 x))) (vector->list (cst-tokens c2)))
                  (list (list (token-kind previous) (token-subkind previous) (token-text (run-cst r) previous))
                        (list 'whitespace #f " ")
                        (list (token-kind tok) (token-subkind tok) (token-text (run-cst r) tok))))))
      (define (format-code-line! line ls le i p)
        (let-values ([(l k) (context-at r p)])
          (let* ([in-col (advance-column 0 s ls p)]
                 [target (code-target r l k in-col column-of #f)]
                 [col (if (and (not (fx= target in-col)) (spaces-and-tabs? s ls p))
                          (begin (edit! line ls p (make-string target #\space)) target)
                          in-col)])
            (vector-set! owners line l)
            (fxvector-set! deltas line (fx- col in-col))
            (let loop ([i i] [col col] [previous #f])
              (when (fx< i token-count)
                (let* ([tok (vector-ref tokens i)] [ts (token-start tok)] [te (token-end tok)])
                  (when (fx< ts le)
                    (if (eq? (token-kind tok) 'whitespace)
                        (if (fx>= te le)
                            (let ([kept (let skip ([q ts])
                                          (if (and (fx< q le) (memv (string-ref s q) '(#\space #\tab))) (skip (fx+ q 1)) q))])
                              (when (fx< ts kept) (edit! line ts kept "")))
                            (let ([next (vector-ref tokens (fx+ i 1))])
                              (if (and (spaces-and-tabs? s ts te)
                                       (or (and previous (eq? (token-kind previous) 'open)
                                                (not (eq? (token-kind next) 'line-comment)))
                                           (eq? (token-kind next) 'close)))
                                  (begin (edit! line ts te "") (loop (fx+ i 1) col previous))
                                  (loop (fx+ i 1) (advance-column col s ts te) previous))))
                        (let ([col (if (and previous (fx= (token-end previous) ts)
                                            (not (memq (token-kind previous) '(open prefix)))
                                            (not (memq (token-kind tok) '(close dot)))
                                            (touching-sibling? ts)
                                            (separable? previous tok))
                                       (begin (edit! line ts ts " ") (fx+ col 1))
                                       col)])
                          (fxvector-set! cols i col)
                          (when (fx<= te le)
                            (loop (fx+ i 1) (advance-column col s ts te) tok)))))))))))
      (define (code-line-owner line i)
        (or (vector-ref owners line)
            (let-values ([(owner k) (context-at r (token-start (vector-ref tokens i)))]) owner)))
      ;; A comment line at the end of a range moves with a code line after
      ;; it, which the range leaves where it is.
      (define (note-code-after-range! below comments)
        (let ([pending (make-eq-hashtable)])
          (for-each (lambda (c) (hashtable-set! pending (comment-line-layout c) #t)) comments)
          (let ([end (fold-left (lambda (end c) (fxmax end (node-end (layout-node (comment-line-layout c))))) 0 comments)])
            (let loop ([j (fx+ last 1)])
              (when (and (fx< j line-count) (fx< (text-line-start t j) end) (fx> (hashtable-size pending) 0))
                (let-values ([(kind i) (line-kind r j)])
                  (when (eq? kind 'code)
                    (let ([owner (code-line-owner j i)])
                      (when (hashtable-contains? pending owner)
                        (hashtable-delete! pending owner)
                        (hashtable-set! below owner (fxvector-ref deltas j))))))
                (loop (fx+ j 1)))))))
      (define (format-comment-line! c below)
        (let* ([line (comment-line-line c)] [tok (comment-line-token c)] [l (comment-line-layout c)]
               [ls (text-line-start t line)] [p (token-start tok)] [in-col (advance-column 0 s ls p)])
          (let ([target
                 (if (eq? (format-style-comment-lines style) 'as-code)
                     (code-target r l (comment-line-index c) in-col column-of #f)
                     (let ([text (token-text (run-cst r) tok)])
                       (if (and (fx= in-col 0) (fx>= (string-length text) 3) (string=? (substring text 0 3) ";;;"))
                           0
                           (fxmax 0
                             (fx+ in-col
                                  (hashtable-ref below l
                                    (if (layout-root? l) 0 (fxvector-ref deltas (line-of r (layout-start l))))))))))])
            (when (and (not (fx= target in-col)) (spaces-and-tabs? s ls p))
              (edit! line ls p (make-string target #\space))))))
      ;; A comment line moves with the nearest code line below it in its
      ;; list. One walk up the lines finds that line for every comment line;
      ;; a search down from each one is quadratic in a run of comment lines.
      (define (place-comment-lines! comments)
        (let ([below (make-eq-hashtable)])
          (note-code-after-range! below comments)
          (let loop ([line last] [comments comments])
            (unless (null? comments)
              (if (fx= line (comment-line-line (car comments)))
                  (begin (format-comment-line! (car comments) below) (loop (fx- line 1) (cdr comments)))
                  (let-values ([(kind i) (line-kind r line)])
                    (when (eq? kind 'code) (hashtable-set! below (code-line-owner line i) (fxvector-ref deltas line)))
                    (loop (fx- line 1) comments)))))))
      (let ([comments '()])
        (do ([line first (fx+ line 1)]) ((fx> line last))
          (when (enabled? line)
            (let ([ls (text-line-start t line)] [le (text-line-end t line)])
              (let-values ([(kind i) (line-kind r line)])
                (case kind
                  [(blank) (when (and (fx< ls le) (spaces-and-tabs? s ls le)) (edit! line ls le ""))]
                  [(comment)
                   (let ([tok (vector-ref tokens i)])
                     (let-values ([(l k) (context-at r (token-start tok))])
                       (set! comments (cons (make-comment-line line tok l k) comments))))]
                  [(code) (format-code-line! line ls le i (token-start (vector-ref tokens i)))]
                  [else (void)])))))
        (place-comment-lines! comments))
      (let ([body (apply append (map reverse (vector->list edits)))]
            [final (let loop ([i (fx- token-count 1)])
                     (cond
                       [(fx< i 0) #f]
                       [(eq? (token-kind (vector-ref tokens i)) 'whitespace) (loop (fx- i 1))]
                       [else (vector-ref tokens i)]))])
        ;; A token can take the line break in, as a symbol ending in a
        ;; backslash does, so the line break is read before it is added.
        (let ([line-break (make-text-edit n n (final-line-break s))])
          (if (and final
                   (fx>= last (fx- line-count 1))
                   (not (eq? (token-kind final) 'unread-tail))
                   (fx= (line-break-count s (token-end final) n) 0)
                   (window-reads-same? (run-cst r) (list line-break) (fx- line-count 1) (fx- line-count 1)))
              (append body (list line-break))
              body)))))

  ;; The final line break formatting adds is the one line break a text may
  ;; gain.
  (define (reads-same? sa a i0 i1 a-start a-end sb b j0 j1 b-start b-end may-add-break?)
    (define (next-solid v i limit)
      (if (and (fx< i limit) (eq? (token-kind (vector-ref v i)) 'whitespace)) (next-solid v (fx+ i 1) limit) i))
    (define (same-text? x y)
      (and (fx= (fx- (token-end x) (token-start x)) (fx- (token-end y) (token-start y)))
           (let loop ([i (token-start x)] [j (token-start y)])
             (or (fx= i (token-end x))
                 (and (char=? (string-ref sa i) (string-ref sb j)) (loop (fx+ i 1) (fx+ j 1)))))))
    (let loop ([i i0] [j j0] [gap-a a-start] [gap-b b-start])
      (let* ([i2 (next-solid a i i1)] [j2 (next-solid b j j1)]
             [breaks-a (line-break-count sa gap-a (if (fx< i2 i1) (token-start (vector-ref a i2)) a-end))]
             [breaks-b (line-break-count sb gap-b (if (fx< j2 j1) (token-start (vector-ref b j2)) b-end))])
        (cond
          [(and (fx= i2 i1) (fx= j2 j1))
           (or (fx= breaks-a breaks-b) (and may-add-break? (fx= breaks-a 0) (fx= breaks-b 1)))]
          [(or (fx= i2 i1) (fx= j2 j1)) #f]
          [else
           (let ([x (vector-ref a i2)] [y (vector-ref b j2)])
             (and (fx= breaks-a breaks-b)
                  (eq? (token-kind x) (token-kind y))
                  (eq? (token-subkind x) (token-subkind y))
                  (same-text? x y)
                  (loop (fx+ i2 1) (fx+ j2 1) (token-end x) (token-end y))))]))))

  (define (whole-text-reads-same? c edits)
    (let* ([sa (text-string (cst-text c))] [a (cst-tokens c)]
           [sb (apply-edits sa 0 (string-length sa) edits)] [b (cst-tokens (read-cst (make-text sb) (cst-mode-at c 0)))])
      (reads-same? sa a 0 (vector-length a) 0 (string-length sa) sb b 0 (vector-length b) 0 (string-length sb) #t)))

  ;; Whitespace edits within some lines can change only the tokens from
  ;; just before those lines to just after them, so only that window is
  ;; read again. It starts a token early, so that its first token is never
  ;; at offset 0, where Chez reads a script header. An end of file datum
  ;; ends the reading only at top level, which a window cannot tell.
  (define (window-reads-same? c edits first last)
    (let* ([t (cst-text c)] [sa (text-string t)] [n (string-length sa)] [a (cst-tokens c)]
           [index-at (lambda (offset)
                       (let loop ([lo 0] [hi (vector-length a)])
                         (if (fx= (fx+ lo 1) hi)
                             lo
                             (let ([mid (fxsrl (fx+ lo hi) 1)])
                               (if (fx<= (token-start (vector-ref a mid)) offset) (loop mid hi) (loop lo mid))))))]
           [i0 (fxmax 0 (fx- (index-at (text-line-start t first)) 1))]
           [le (text-line-end t last)]
           [i1 (if (fx= le n) (vector-length a) (fx+ (index-at le) 1))]
           [ws (token-start (vector-ref a i0))]
           [we (if (fx= i1 (vector-length a)) n (token-end (vector-ref a (fx- i1 1))))])
      (if (let loop ([i i0])
            (and (fx< i i1)
                 (or (eq? (token-subkind (vector-ref a i)) 'eof) (loop (fx+ i 1)))))
          (whole-text-reads-same? c edits)
          (let* ([sb (apply-edits sa ws we edits)] [b (cst-tokens (read-cst (make-text sb) (cst-mode-at c ws)))])
            (reads-same? sa a i0 i1 ws we sb b 0 (vector-length b) 0 (string-length sb) (fx= we n))))))

  (define (check-edits-shape who c edits)
    (unless (cst? c) (assertion-violationf who "~s is not a CST" c))
    (unless (and (list? edits) (for-all text-edit? edits)
                 (let loop ([edits edits] [at 0])
                   (or (null? edits)
                       (and (fx<= at (text-edit-start (car edits)))
                            (fx<= (text-edit-end (car edits)) (text-length (cst-text c)))
                            (loop (cdr edits) (text-edit-end (car edits)))))))
      (assertion-violationf who "~s is not a list of ordered edits within the text" edits)))

  (define changed-reading "formatting would change how the text reads, so no edits were made")

  (define check-format-edits
    (case-lambda
      [(c edits)
       (check-edits-shape 'check-format-edits c edits)
       (unless (or (null? edits) (whole-text-reads-same? c edits))
         (internal-refusal changed-reading))]
      [(c edits first-line last-line)
       (check-edits-shape 'check-format-edits c edits)
       (let ([t (cst-text c)])
         (unless (and (fixnum? first-line) (fixnum? last-line) (fx<= 0 first-line last-line) (fx< last-line (text-line-count t))
                      (for-all (lambda (e) (and (fx<= (text-line-start t first-line) (text-edit-start e))
                                                (fx<= (text-edit-end e) (text-line-end t last-line))))
                               edits))
           (assertion-violationf 'check-format-edits "~s are not edits within lines ~s to ~s" edits first-line last-line)))
       (unless (or (null? edits) (window-reads-same? c edits first-line last-line))
         (internal-refusal changed-reading))]))

  (define (checked-edits c style first last)
    (let ([edits (pass-edits (new-run c style #t) first last)])
      (unless (or (null? edits)
                  (if (and (fx= first 0) (fx= last (fx- (text-line-count (cst-text c)) 1)))
                      (whole-text-reads-same? c edits)
                      (window-reads-same? c edits first last)))
        (internal-refusal changed-reading))
      edits))

  ;; Only &format-refused may leave: a fault in the formatter becomes a
  ;; refusal, so a caller formatting on save never sees anything else.
  (define (refusing-faults thunk)
    (guard (e [(format-refused? e) (raise e)]
              [#t (internal-refusal
                    (format "the formatter failed: ~a"
                      (if (condition? e) (call-with-string-output-port (lambda (p) (display-condition e p))) e)))])
      (thunk)))

  (define (check-arguments who c style)
    (unless (cst? c) (assertion-violationf who "~s is not a CST" c))
    (unless (format-style? style) (assertion-violationf who "~s is not a format style" style)))

  (define (format-text c style)
    (check-arguments 'format-text c style)
    (refusing-faults
      (lambda ()
        (let ([problem (first-problem c)])
          (when problem (refuse (problem-diagnostic c problem)))
          (checked-edits c style 0 (fx- (text-line-count (cst-text c)) 1))))))

  (define (format-lines c style first-line last-line)
    (check-arguments 'format-lines c style)
    (let ([count (text-line-count (cst-text c))])
      (unless (and (fixnum? first-line) (fixnum? last-line) (fx<= 0 first-line last-line) (fx< last-line count))
        (assertion-violationf 'format-lines "~s to ~s are not lines of a text with ~s lines" first-line last-line count)))
    (refusing-faults
      (lambda ()
        (let* ([problem (first-problem c)]
               [problem-line (and problem (text-offset-line (cst-text c) (problem-start problem)))])
          (when (and problem (fx>= first-line problem-line))
            (refuse (problem-diagnostic c problem)))
          (checked-edits c style first-line (if problem (fxmin last-line (fx- problem-line 1)) last-line))))))

  ;; Lists left open above the line are what typing leaves; any other
  ;; problem above it means the lines above no longer say where it goes.
  (define (format-line-indentation c style line)
    (check-arguments 'format-line-indentation c style)
    (unless (and (fixnum? line) (fx<= 0 line) (fx< line (text-line-count (cst-text c))))
      (assertion-violationf 'format-line-indentation "~s is not a line of the text" line))
    (guard (e [#t '()])
      (let* ([r (new-run c style #f)] [t (run-text r)] [s (run-string r)]
             [ls (text-line-start t line)] [le (text-line-end t line)]
             [column-of (lambda (offset) (input-column r offset))])
        (define (edit p target)
          (if (or (fx= target (advance-column 0 s ls p)) (not (spaces-and-tabs? s ls p)))
              '()
              (let ([edits (list (make-text-edit ls p (make-string target #\space)))])
                (if (window-reads-same? c edits line line) edits '()))))
        (if (or (exists (lambda (p) (and (fx< (problem-start p) ls) (not (eq? (problem-kind p) 'unclosed))))
                        (structural-problems c))
                (fx= (bytevector-u8-ref (disabled-lines r (fx+ line 1)) line) 1))
            '()
            (let-values ([(kind i) (line-kind r line)])
              (case kind
                [(blank)
                 (if (spaces-and-tabs? s ls le)
                     (let-values ([(l k) (context-at r ls)])
                       (edit le (blank-line-target r l k column-of line)))
                     '())]
                [(code comment)
                 (let ([p (token-start (vector-ref (run-tokens r) i))])
                   (let-values ([(l k) (context-at r p)])
                     (edit p (code-target r l k (advance-column 0 s ls p) column-of line))))]
                [else '()]))))))

  ;; A blank line has no column of its own to keep, so a body written at
  ;; column 0 is recognised by the element before it.
  (define (blank-line-target r l k column-of line)
    (cond
      [(layout-root? l) 0]
      [(and (fx> k 0) (column-zero-body? r l k) (column-zero-body? r l (fx- k 1))
            (let ([start (element-start l (fx- k 1))])
              (and (leads-line? r start) (fx= (input-column r start) 0))))
       0]
      [else (element-column r l k column-of line)])))
