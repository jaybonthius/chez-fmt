(library (reader-oracle)
  (export compare-with-chez
          comparison? comparison-failures comparison-chez-read? comparison-skipped
          comparison-exact-message?
          raised-by-record-construction?
          round-trip-failure
          corpus-directories corpus-files read-source-file
          line-starts random-mutant)
  (import (chezscheme) (analysis text) (analysis reader) (analysis diagnostic))

  (define-record-type comparison
    (nongenerative) (sealed #t)
    (fields failures chez-read? skipped exact-message?))

  ;; Chez's message depends on record types defined in the reading process,
  ;; which the reader never consults.
  (define process-dependent-messages
    '("unrecognized record name ~s"
      "too few fields supplied for record ~s"
      "too many fields supplied for record ~s"
      "unresolvable cycle constructing record of type ~s"))

  (define (datum-start x) (if (token? x) (token-start x) (node-start x)))
  (define (datum-end x) (if (token? x) (token-end x) (node-end x)))

  (define (round-trip-failure c s)
    (let ([tokens (cst-tokens c)] [n (string-length s)])
      (call/cc
        (lambda (k)
          (define (fail fmt . args) (k (apply format fmt args)))
          (let loop ([i 0] [p 0])
            (if (fx= i (vector-length tokens))
                (unless (fx= p n) (fail "tokens end at ~a of ~a" p n))
                (let ([t (vector-ref tokens i)])
                  (unless (fx= (token-start t) p) (fail "token ~a starts at ~a, not ~a" i (token-start t) p))
                  (unless (fx< (token-start t) (token-end t)) (fail "empty token at ~a" p))
                  (loop (fx+ i 1) (token-end t)))))
          (let walk ([x (cst-root c)])
            (when (node? x)
              (let ([children (node-children x)])
                (if (fx= (vector-length children) 0)
                    (unless (and (eq? (node-kind x) 'root) (fx= n 0)) (fail "empty node at ~a" (node-start x)))
                    (begin
                      (unless (fx= (datum-start (vector-ref children 0)) (node-start x))
                        (fail "node at ~a does not start with its first child" (node-start x)))
                      (unless (fx= (datum-end (vector-ref children (fx- (vector-length children) 1))) (node-end x))
                        (fail "node at ~a does not end with its last child" (node-start x)))
                      (do ([i 1 (fx+ i 1)]) ((fx= i (vector-length children)))
                        (unless (fx= (datum-end (vector-ref children (fx- i 1))) (datum-start (vector-ref children i)))
                          (fail "children of node at ~a are not contiguous" (node-start x))))
                      (vector-for-each walk children))))))
          (unless (and (fx= (node-start (cst-root c)) 0) (fx= (node-end (cst-root c)) n))
            (fail "root does not span the text"))
          #f))))

  ;; Chez reads a script header's line as nothing; spaces keep the offsets.
  (define (chez-text c s)
    (let ([tokens (cst-tokens c)])
      (if (and (fx> (vector-length tokens) 0) (eq? (token-kind (vector-ref tokens 0)) 'script-header))
          (let ([copy (string-copy s)])
            (do ([i 0 (fx+ i 1)]) ((fx= i (token-end (vector-ref tokens 0))) copy)
              (unless (char=? (string-ref copy i) #\newline) (string-set! copy i #\space))))
          s)))

  (define (mode-prefix syntax fold)
    (string-append (if (eq? syntax 'r6rs) "#!r6rs" "") (if (eq? fold 'fold) "#!fold-case" "")))

  (define (condition-span c)
    (let ([span #f])
      (parameterize ([current-locate-source-object-source
                      (lambda (src start? cache?)
                        (unless span (set! span (cons (source-object-bfp src) (source-object-efp src))))
                        (values))])
        (display-condition c (open-output-string)))
      span))

  (define (condition-text c)
    (if (and (message-condition? c) (irritants-condition? c))
        (apply format (condition-message c) (condition-irritants c))
        (condition-message c)))

  (define (chez-read text shift)
    (let* ([ip (open-string-input-port text)]
           [sfd (make-source-file-descriptor "text" (open-bytevector-input-port (string->utf8 text)))])
      (guard (c [(and (message-condition? c) (condition-span c))
                 => (lambda (span)
                      (list 'error (car span) (cdr span) (condition-message c) (condition-text c)))]
                [else (list 'unlocated c)])
        (let loop ([fp 0] [acc '()])
          (let-values ([(x end) (get-datum/annotations ip sfd fp)])
            (if (eof-object? x)
                (cons 'ok (reverse acc))
                (loop end (cons x acc))))))))

  (define (same-condition? a b)
    (define (field ok? get c) (and (ok? c) (get c)))
    (and (condition? a) (condition? b)
         (equal? (map record-rtd (simple-conditions a)) (map record-rtd (simple-conditions b)))
         (equal? (field who-condition? condition-who a) (field who-condition? condition-who b))
         (equal? (field message-condition? condition-message a) (field message-condition? condition-message b))
         (equal? (field irritants-condition? condition-irritants a) (field irritants-condition? condition-irritants b))))

  (define (condition->string c)
    (call-with-string-output-port (lambda (p) (display-condition c p))))

  ;; Chez's reader gives every error of its own a span. A condition without
  ;; one comes from code the reader calls: the constructor of the record type
  ;; that #[name field ...] names, defined in the reading process. Requiring
  ;; that constructing one of the text's records raise the same condition
  ;; keeps any other condition without a span a failure.
  (define (raised-by-record-construction? c condition)
    (let ([s (text-string (cst-text c))])
      (define (read-alone x)
        (let ([start (datum-start x)])
          (guard (e [#t (values #f #f)])
            (let ([v (read (open-string-input-port
                             (string-append (mode-directives (cst-mode-at c start))
                                            (substring s start (datum-end x)))))])
              (if (eof-object? v) (values #f #f) (values #t v))))))
      (define (process-record-type name)
        (and (symbol? name)
             (or (record-reader (symbol->string name))
                 (let ([x (($primitive $sgetprop) name '*rtd* #f)])
                   (and (record-type-descriptor? x) x)))))
      (define (raises-condition? x)
        (let ([data (vector->list (node-data x))])
          (and (pair? data)
               (let-values ([(ok? name) (read-alone (car data))])
                 (let ([rtd (and ok? (process-record-type name))])
                   (and rtd
                        (let loop ([fields (cdr data)] [vals '()])
                          (if (null? fields)
                              (same-condition?
                                (guard (e [#t e]) (apply (record-constructor rtd) (reverse vals)) #f)
                                condition)
                              (let-values ([(ok? v) (read-alone (car fields))])
                                (and ok? (loop (cdr fields) (cons v vals))))))))))))
      (let walk ([x (cst-root c)])
        (and (node? x)
             (or (and (eq? (node-kind x) 'list)
                      (eq? (token-subkind (vector-ref (node-children x) 0)) 'record)
                      (raises-condition? x))
                 (exists walk (vector->list (node-children x))))))))

  (define (chez-tokens text)
    (let ([ip (open-string-input-port text)])
      (let loop ([acc '()])
        (let-values ([(type value start end)
                      (guard (c [#t (values 'error #f #f #f)]) (read-token ip))])
          (case type
            [(eof) (cons 'ok (reverse acc))]
            [(error) (cons 'error (reverse acc))]
            [else (loop (cons (list type value start end) acc))])))))

  (define (our-token-type c t)
    (case (token-kind t)
      [(open)
       (case (token-subkind t)
         [(paren) '(lparen)] [(bracket) '(lbrack)] [(vector) '(vparen)] [(sized-vector) '(vnparen)]
         [(fxvector) '(vfxparen)] [(sized-fxvector) '(vfxnparen)]
         [(flvector) '(vflparen)] [(sized-flvector) '(vflnparen)]
         [(bytevector) '(vu8paren)] [(sized-bytevector) '(vu8nparen)]
         [(stencil-vector) '(vsparen)] [(sized-stencil-vector) '(vsnparen)]
         [(record) '(record-brack)])]
      [(close) (if (eq? (token-subkind t) 'paren) '(rparen) '(rbrack))]
      [(dot) '(dot)]
      [(prefix)
       (case (token-subkind t)
         [(box) '(box)]
         [(mark) '(mark)]
         [else (list 'quote (token-subkind t))])]
      [(reference) '(insert)]
      [(error) (if (eq? (token-subkind t) 'fasl) '(fasl) '(error))]
      [else (list 'atomic (token-kind t))]))

  (define (chez-token-type type value)
    (case type
      [(quote) (list 'quote value)]
      [(atomic)
       (list 'atomic
         (cond
           [(gensym? value) 'gensym]
           [(symbol? value) 'symbol]
           [(number? value) 'number]
           [(string? value) 'string]
           [(char? value) 'character]
           [(boolean? value) 'boolean]
           [(pair? value) 'primitive]
           [else 'special]))]
      [else (list type)]))

  (define (token-failures c text shift limit)
    (let* ([chez (chez-tokens text)]
           [complete? (eq? (car chez) 'ok)]
           [ours (filter (lambda (t) (not (memq (token-kind t) '(whitespace line-comment block-comment directive script-header unread-tail))))
                         (vector->list (cst-tokens c)))]
           [theirs (filter (lambda (t) (fx< (fx- (caddr t) shift) limit)) (cdr chez))])
      (let loop ([ours ours] [theirs theirs])
        (cond
          [(and (null? theirs) (or (null? ours) (not complete?))) '()]
          [(or (null? ours) (null? theirs))
           (list (cons 'tokens (format "token counts differ at ~s / ~s"
                                 (if (null? ours) 'end (token-start (car ours)))
                                 (if (null? theirs) 'end (fx- (caddr (car theirs)) shift)))))]
          [else
           (let* ([t (car ours)] [th (car theirs)]
                  [start (fx- (caddr th) shift)]
                  [end (fxmin (fx- (cadddr th) shift) (string-length (text-string (cst-text c))))])
             (if (and (fx= (token-start t) start) (fx= (token-end t) end)
                      (equal? (our-token-type c t) (chez-token-type (car th) (cadr th))))
                 (loop (cdr ours) (cdr theirs))
                 (list (cons 'tokens (format "ours ~s ~s ~s-~s, Chez's ~s ~s ~s-~s"
                                       (token-kind t) (token-subkind t) (token-start t) (token-end t)
                                       (car th) (cadr th) start end)))))]))))

  (define (chez-spans annotations shift n)
    (let ([seen (make-eq-hashtable)] [acc '()])
      (define (walk-annotation a)
        (unless (hashtable-ref seen a #f)
          (hashtable-set! seen a #t)
          (let ([src (annotation-source a)] [e (annotation-expression a)])
            (set! acc (cons (list (fxmax 0 (fx- (source-object-bfp src) shift))
                                  (fxmin n (fx- (source-object-efp src) shift)))
                            acc))
            (walk e))))
      (define (walk x)
        (cond
          [(annotation? x) (walk-annotation x)]
          [(hashtable-ref seen x #f) (void)]
          [(pair? x) (hashtable-set! seen x #t) (walk (car x)) (walk (cdr x))]
          [(vector? x) (hashtable-set! seen x #t) (vector-for-each walk x)]
          [(box? x) (hashtable-set! seen x #t) (walk (unbox x))]
          [(stencil-vector? x)
           (hashtable-set! seen x #t)
           (do ([i 0 (fx+ i 1)]) ((fx= i (stencil-vector-length x))) (walk (stencil-vector-ref x i)))]
          [else (void)]))
      (for-each walk-annotation annotations)
      (sort (lambda (a b) (or (fx< (car a) (car b)) (and (fx= (car a) (car b)) (fx< (cadr a) (cadr b))))) acc)))

  (define (element-list? x)
    (memq (token-subkind (vector-ref (node-children x) 0))
          '(fxvector sized-fxvector flvector sized-flvector bytevector sized-bytevector)))

  ;; Spans of our data that Chez annotates: a mark's datum is not annotated
  ;; (its annotation moves to the mark), nor the elements of fixnum, flonum
  ;; and byte vectors, nor datum comments. Whether a record's fields are
  ;; annotated depends on the record type, so neither side counts them.
  (define (our-spans c)
    (let ([acc '()])
      (define (note! x) (set! acc (cons (list (datum-start x) (datum-end x)) acc)))
      (define (walk x)
        (note! x)
        (walk-contents x))
      (define (walk-contents x)
        (when (node? x)
          (case (node-kind x)
            [(list)
             (unless (or (element-list? x) (eq? (token-subkind (vector-ref (node-children x) 0)) 'record))
               (vector-for-each walk (node-data x)))]
            [(abbreviation)
             (let ([data (node-data x)])
               (when (fx> (vector-length data) 0)
                 (if (eq? (token-subkind (vector-ref (node-children x) 0)) 'mark)
                     (under-mark (vector-ref data 0))
                     (walk (vector-ref data 0)))))])))
      (define (under-mark x)
        (if (and (node? x) (eq? (node-kind x) 'abbreviation)
                 (eq? (token-subkind (vector-ref (node-children x) 0)) 'mark))
            (let ([data (node-data x)])
              (when (fx> (vector-length data) 0) (under-mark (vector-ref data 0))))
            (walk-contents x)))
      (vector-for-each (lambda (f) (walk (top-level-form-node f))) (cst-top-level-forms c))
      (sort (lambda (a b) (or (fx< (car a) (car b)) (and (fx= (car a) (car b)) (fx< (cadr a) (cadr b))))) acc)))

  (define (annotation-at annotations start end shift)
    (let ([found #f] [seen (make-eq-hashtable)])
      (define (walk x)
        (cond
          [found (void)]
          [(annotation? x)
           (unless (hashtable-ref seen x #f)
             (hashtable-set! seen x #t)
             (let ([src (annotation-source x)])
               (if (and (fx= (fx- (source-object-bfp src) shift) start) (fx= (fx- (source-object-efp src) shift) end))
                   (set! found x)
                   (when (and (fx<= (fx- (source-object-bfp src) shift) start) (fx<= end (fx- (source-object-efp src) shift)))
                     (walk (annotation-expression x))))))]
          [(hashtable-ref seen x #f) (void)]
          [(pair? x) (hashtable-set! seen x #t) (walk (car x)) (walk (cdr x))]
          [(vector? x) (hashtable-set! seen x #t) (vector-for-each walk x)]
          [(box? x) (hashtable-set! seen x #t) (walk (unbox x))]
          [(stencil-vector? x)
           (hashtable-set! seen x #t)
           (do ([i 0 (fx+ i 1)]) ((fx= i (stencil-vector-length x))) (walk (stencil-vector-ref x i)))]
          [else (void)]))
      (for-each walk annotations)
      found))

  (define (same-value? a b)
    (cond
      [(and (gensym? a) (gensym? b))
       (string=? (symbol->string a) (symbol->string b))]
      [(and (pair? a) (pair? b)) (and (same-value? (car a) (car b)) (same-value? (cdr a) (cdr b)))]
      [else (equal? a b)]))

  (define (mode-directives mode)
    (string-append
      (if (eq? (reader-mode-syntax mode) 'r6rs) "#!r6rs " "")
      (if (eq? (reader-mode-fold mode) 'fold) "#!fold-case " "#!no-fold-case ")))

  (define (value-failures c annotations shift)
    (let ([failures '()] [s (text-string (cst-text c))])
      (vector-for-each
        (lambda (t)
          (when (memq (token-kind t) '(symbol number string character boolean gensym primitive special))
            (let ([a (annotation-at annotations (token-start t) (token-end t) shift)])
              (when a
                (let* ([expected (annotation-stripped a)]
                       [text (substring s (token-start t) (token-end t))]
                       [alone (guard (e [#t e])
                                (read (open-string-input-port
                                        (string-append (mode-directives (cst-mode-at c (token-start t))) text))))]
                       [name (symbol-token-name c t)]
                       [expected-name (cond
                                        [(symbol? expected) (symbol->string expected)]
                                        [(and (pair? expected) (eq? (car expected) '$primitive))
                                         (symbol->string (car (last-pair expected)))]
                                        [else #f])])
                  (unless (same-value? alone expected)
                    (set! failures (cons (cons 'values (format "~s at ~a reads alone as ~s, in place as ~s"
                                                         text (token-start t) alone expected))
                                         failures)))
                  ;; Chez interns #{pretty unique} by its unique name, so a
                  ;; gensym the process already holds keeps its own pretty name.
                  (unless (or (not (memq (token-kind t) '(symbol gensym primitive)))
                              (and (eq? (token-kind t) 'gensym) (char=? (string-ref text 1) #\{))
                              (equal? name expected-name))
                    (set! failures (cons (cons 'values (format "name of ~s at ~a is ~s, not ~s"
                                                         text (token-start t) name expected-name))
                                         failures))))))))
        (cst-tokens c))
      (reverse failures)))

  ;; An error's category is Chez's format string; its directives, a
  ;; conditional included, stand for any text.
  (define (template-pieces template)
    (let ([n (string-length template)])
      (let loop ([i 0] [literal '()] [pieces '()])
        (define (flush) (cons (list->string (reverse literal)) pieces))
        (cond
          [(fx= i n) (reverse (flush))]
          [(char=? (string-ref template i) #\~)
           (let skip ([j (fx+ i 1)] [depth 0])
             (let ([ch (string-ref template j)])
               (cond
                 [(memv ch '(#\: #\@ #\, #\' #\v #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                  (if (char=? ch #\') (skip (fx+ j 2) depth) (skip (fx+ j 1) depth))]
                 [(char=? ch #\[)
                  (let inner ([k (fx+ j 1)] [d (fx+ depth 1)])
                    (cond
                      [(and (char=? (string-ref template k) #\~) (char=? (string-ref template (fx+ k 1)) #\]))
                       (if (fx= d 1) (loop (fx+ k 2) '() (cons 'any (flush))) (inner (fx+ k 2) (fx- d 1)))]
                      [(and (char=? (string-ref template k) #\~) (memv (string-ref template (fx+ k 1)) '(#\[)))
                       (inner (fx+ k 2) (fx+ d 1))]
                      [else (inner (fx+ k 1) d)]))]
                 [else (loop (fx+ j 1) '() (cons 'any (flush)))])))]
          [else (loop (fx+ i 1) (cons (string-ref template i) literal) pieces)]))))

  (define (matches-template? message template)
    (let ([n (string-length message)])
      (let match ([pieces (template-pieces template)] [i 0])
        (cond
          [(null? pieces) (fx= i n)]
          [(eq? (car pieces) 'any)
           (let try ([j i])
             (and (fx<= j n)
                  (or (match (cdr pieces) j) (try (fx+ j 1)))))]
          [else
           (let ([lit (car pieces)])
             (and (fx<= (fx+ i (string-length lit)) n)
                  (string=? lit (substring message i (fx+ i (string-length lit))))
                  (match (cdr pieces) (fx+ i (string-length lit)))))]))))

  (define (compare-with-chez s syntax fold)
    (let* ([c (guard (e [#t e]) (read-cst (make-text s) (make-reader-mode syntax fold)))])
      (if (not (cst? c))
          (make-comparison (list (cons 'exception (call-with-string-output-port (lambda (p) (display-condition c p)))))
            #f #f #f)
          (let* ([n (string-length s)]
                 [prefix (mode-prefix syntax fold)]
                 [shift (string-length prefix)]
                 [text (string-append prefix (chez-text c s))]
                 [round-trip (round-trip-failure c s)]
                 [failures (if round-trip (list (cons 'round-trip round-trip)) '())]
                 [chez (chez-read text shift)]
                 [diagnostics (cst-diagnostics c)]
                 [limit (let ([tokens (cst-tokens c)])
                          (if (and (fx> (vector-length tokens) 0)
                                   (eq? (token-kind (vector-ref tokens (fx- (vector-length tokens) 1))) 'unread-tail))
                              (token-start (vector-ref tokens (fx- (vector-length tokens) 1)))
                              n))])
            (set! failures (append failures (token-failures c text shift limit)))
            (case (car chez)
              [(ok)
                (let ([annotations (cdr chez)])
                  (unless (fx= (vector-length diagnostics) 0)
                    (let ([d (vector-ref diagnostics 0)])
                      (set! failures (append failures
                                       (list (cons 'first-error
                                               (format "Chez reads it, we report ~s at ~a-~a"
                                                 (diagnostic-message d) (diagnostic-start d) (diagnostic-end d))))))))
                  (unless (fx= (length annotations) (vector-length (cst-top-level-forms c)))
                    (set! failures (append failures
                                     (list (cons 'data (format "~a top-level data, Chez reads ~a"
                                                         (vector-length (cst-top-level-forms c)) (length annotations)))))))
                  (let ([theirs (chez-spans annotations shift n)] [ours (our-spans c)])
                    (unless (equal? theirs ours)
                      (let loop ([a theirs] [b ours])
                        (cond
                          [(and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b))]
                          [else
                           (set! failures (append failures
                                            (list (cons 'data (format "first difference: Chez ~s, ours ~s"
                                                                (if (pair? a) (car a) 'none)
                                                                (if (pair? b) (car b) 'none))))))]))))
                  (set! failures (append failures (value-failures c annotations shift)))
                  (make-comparison failures #t #f #f))]
              [(unlocated)
               (let ([condition (cadr chez)])
                 (if (raised-by-record-construction? c condition)
                     (make-comparison failures #f (condition->string condition) #f)
                     (make-comparison
                       (append failures
                         (list (cons 'first-error
                                 (format "Chez raises ~s with no source span" (condition->string condition)))))
                       #f #f #f)))]
              [else
                (let ([start (fxmax 0 (fx- (list-ref chez 1) shift))]
                      [end (fxmin n (fxmax 0 (fx- (list-ref chez 2) shift)))]
                      [template (list-ref chez 3)]
                      [message (list-ref chez 4)])
                  (cond
                    [(member template process-dependent-messages)
                     (make-comparison failures #f template #f)]
                    [(fx= (vector-length diagnostics) 0)
                     (make-comparison
                       (append failures (list (cons 'first-error (format "we read it, Chez reports ~s at ~a-~a" message start end))))
                       #f #f #f)]
                    [else
                     (let ([d (vector-ref diagnostics 0)])
                       (make-comparison
                         (if (and (fx= (diagnostic-start d) start) (fx= (diagnostic-end d) end)
                                  (matches-template? (diagnostic-message d) template))
                             failures
                             (append failures
                               (list (cons 'first-error
                                       (format "ours ~s at ~a-~a, Chez's ~s at ~a-~a"
                                         (diagnostic-message d) (diagnostic-start d) (diagnostic-end d)
                                         message start end)))))
                         #f #f
                         (string=? (diagnostic-message d) message)))]))])))))

  (define (corpus-directories)
    (let ([v (getenv "CHEZ_TOOLS_CORPORA")])
      (if (or (not v) (string=? v ""))
          '()
          (let loop ([chars (string->list v)] [current '()] [acc '()])
            (cond
              [(null? chars) (reverse (if (null? current) acc (cons (list->string (reverse current)) acc)))]
              [(char=? (car chars) #\:) (loop (cdr chars) '() (if (null? current) acc (cons (list->string (reverse current)) acc)))]
              [else (loop (cdr chars) (cons (car chars) current) acc)])))))

  (define source-extensions '("ss" "sls" "sps" "ms" "scm"))

  (define (corpus-files dir)
    (let walk ([dir dir])
      (sort string<?
        (apply append
          (map (lambda (f)
                 (let ([p (string-append dir "/" f)])
                   (cond
                     [(and (file-directory? p) (not (file-symbolic-link? p)) (not (string=? f ".git"))) (walk p)]
                     [(and (file-regular? p) (member (path-extension p) source-extensions)) (list p)]
                     [else '()])))
               (directory-list dir))))))

  (define (read-source-file path)
    (let ([bv (call-with-port (open-file-input-port path) get-bytevector-all)])
      (if (eof-object? bv) "" (decode-source-bytes bv))))

  (define special-characters (string->list "()[]\"|#;\\"))
  (define other-characters (string->list "{}'`,@.+-0179aexr!=%: \n\tλ"))

  (define (random-element items) (list-ref items (random (length items))))

  (define (random-character)
    (if (fx< (random 10) 7) (random-element special-characters) (random-element other-characters)))

  (define (random-position s)
    (let ([n (string-length s)])
      (if (and (fx> n 0) (fx= (random 2) 0))
          (let ([candidates (let loop ([i 0] [acc '()])
                              (if (fx= i n)
                                  acc
                                  (loop (fx+ i 1) (if (memv (string-ref s i) special-characters) (cons i acc) acc))))])
            (if (null? candidates) (random n) (random-element candidates)))
          (random (fx+ n 1)))))

  (define (mutate s)
    (let ([n (string-length s)] [p (random-position s)])
      (case (random 4)
        [(0) (if (fx< p n) (string-append (substring s 0 p) (substring s (fx+ p 1) n)) s)]
        [(1) (string-append (substring s 0 p) (string (random-character)) (substring s p n))]
        [(2) (if (fx< p n) (string-append (substring s 0 (fx+ p 1)) (substring s p n)) s)]
        [else (if (fx< (fx+ p 1) n)
                  (string-append (substring s 0 p) (string (string-ref s (fx+ p 1)) (string-ref s p))
                                 (substring s (fx+ p 2) n))
                  s)])))

  (define (line-starts s)
    (let loop ([i 0] [acc '(0)])
      (cond
        [(fx= i (string-length s)) (list->vector (reverse acc))]
        [(char=? (string-ref s i) #\newline) (loop (fx+ i 1) (cons (fx+ i 1) acc))]
        [else (loop (fx+ i 1) acc)])))

  (define (random-window s starts)
    (let* ([lines (vector-length starts)]
           [first (random lines)]
           [last (fxmin lines (fx+ first 1 (random 60)))])
      (substring s (vector-ref starts first)
        (if (fx= last lines) (string-length s) (vector-ref starts last)))))

  (define (random-mutant s starts)
    (let loop ([s (random-window s starts)] [edits (fx+ 1 (random 3))])
      (if (fx= edits 0) s (loop (mutate s) (fx- edits 1))))))
