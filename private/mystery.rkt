#lang racket

(require redex)
(require test-engine/racket-tests)
(require "mystery-functions.rkt")
(require "mystery-variables.rkt")
(require "mystery-records.rkt")

(provide
 run
 run-all
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
  [var->3 variables3])
 func-lang-1 func-lang-2 func-lang-3
 record-lang-1 record-lang-2 record-lang-3
 var-lang)

(define-syntax-rule (run lang e)
  (let ((x (apply-reduction-relation* lang (term e))))
    (cond
      [(empty? x) (error 'run "can't happen: ~e produced '()" (term e))]
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