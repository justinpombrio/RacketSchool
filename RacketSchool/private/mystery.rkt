#lang racket

(require redex)
(require test-engine/racket-tests)
(require "mystery-functions.rkt")
(require "mystery-variables.rkt")
(require "mystery-records.rkt")

(provide
 run
 stuck?
 (rename-out
  [record->1 records1]
  [record->2 records2]
  [record->3 records3]
  [func->1 functions1]
  [func->2 functions2]
  [func->3 functions3]
  [var->1 variables1]
  [var->2 variables2]
  [var->3 variables3]))

(define-syntax-rule (run lang e)
  (begin
    (define x (apply-reduction-relation* lang (term e)))
    (cond
      [(empty? x) (error 'run "can't happen: ~e produced '()" (term e))]
      [(empty? (rest x)) (set! x (car x))]
      [else (displayln `(warning: ambiguous outcome))
            (for ([i x]) (displayln `(--> ,i)))
            (set! x (car x))])
    (if (stuck? x)
        'stuck
        x)))

(define (stuck? e)
  (and (pair? e) (eq? (car e) 'prog)))