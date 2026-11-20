(library (programs)
  (export run-program run-scheme program-status program-output program-error-output program-seconds)
  (import (chezscheme) (analysis posix) (analysis threads))

  (define-record-type program-run
    (nongenerative) (sealed #t)
    (fields status output error-output seconds))

  (define (program-status r) (program-run-status r))
  (define (program-output r) (program-run-output r))
  (define (program-error-output r) (program-run-error-output r))
  (define (program-seconds r) (program-run-seconds r))

  (define (read-all-later fd)
    (let ([result (make-mailbox)])
      (fork-thread
        (lambda ()
          (let* ([port (open-fd-input-port fd)] [bv (get-bytevector-all port)])
            (close-port port)
            (mailbox-post! result (if (eof-object? bv) (make-bytevector 0) bv)))))
      result))

  (define (run-program program arguments directory seconds)
    (run-scheme (append (list "--program" program) arguments) directory seconds))

  (define (run-scheme arguments directory seconds)
    (let-values ([(out-ours out-theirs) (make-socket-pair)] [(err-ours err-theirs) (make-socket-pair)])
      (let* ([start (current-time 'time-monotonic)]
             [pid (spawn-process (self-executable-path)
                    (append (list "scheme" "-q") arguments)
                    (list (cons 1 out-theirs) (cons 2 err-theirs)) directory)]
             [status (make-mailbox)])
        (close-fd out-theirs)
        (close-fd err-theirs)
        (let ([out (read-all-later out-ours)] [err (read-all-later err-ours)])
          (fork-thread (lambda () (mailbox-post! status (process-wait-exit pid))))
          (let ([s (mailbox-take! status (deadline-after seconds))])
            (when (mailbox-timeout? s) (process-group-signal! pid signal-kill))
            (let ([s (if (mailbox-timeout? s) (mailbox-take! status) s)])
              (let ([d (time-difference (current-time 'time-monotonic) start)])
                (process-reap! pid)
                (make-program-run s (mailbox-take! out) (mailbox-take! err)
                  (+ (time-second d) (/ (time-nanosecond d) 1e9)))))))))))
