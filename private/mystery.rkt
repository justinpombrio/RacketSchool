#lang racket

(require redex)
(require test-engine/racket-tests)

(provide
 run
 run-all
 stuck?)

(define-syntax-rule (run lang e)
  (let ((x (apply-reduction-relation* lang (term e))))
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
        x)))

(define-syntax-rule (run-all lang1 lang2 lang3 e)
  (let ((answer1 (run lang1 e))
        (answer2 (run lang2 e))
        (answer3 (run lang3 e)))
    (values answer1 answer2 answer3)))

(define (stuck? e)
  (eq? e 'stuck))
