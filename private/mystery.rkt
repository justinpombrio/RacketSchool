#lang racket

(require redex)
(require test-engine/racket-tests)

(provide
 run
 run-all
 stuck?)

(define-syntax-rule (run syntax reduction e)
  (if (not (redex-match? syntax p (term e)))
      'syntax-error
      (let ((x (apply-reduction-relation* reduction (term e))))
        (cond
          [(empty? x)
           ; Infinite loop detected!
           ; The programmer asked for an infinite loop, so let's give them one.
           (let loop () (loop))]
          [(empty? (rest x)) (set! x (car x))]
          [else (displayln `(warning: ambiguous outcome))
                (for ([i x]) (displayln `(--> ,i)))
                (set! x (car x))])
        (if (and (pair? x) (eq? (car x) 'prog))
            'stuck
            x))))

(define-syntax-rule (run-all syntax reduction ... e)
  (values (run syntax reduction e) ...))

(define (stuck? e)
  (eq? e 'stuck))
