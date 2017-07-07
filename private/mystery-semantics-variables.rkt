#lang racket

(require redex)
(require "basic.rkt")
(require "testing.rkt")
(require "mystery-variables.rkt")

;; ---------------------------------------------------------------------------------------------------
;; **Mystery semantics**
;; How does this language differ from Variables1?
;; Find a program that will demonstrate the difference.

(define var->4
  (extend-reduction-relation var->1 var-lang
   ;; let
   ;; (This is like the `let` rule in Variables1, except that it does not create a fresh name.)
   (--> (prog f ...
              (in-hole E (let ((x v)) e)))
        (prog f ... (defvar x v)
              (in-hole E e))
        e-let2)))

;; ---------------------------------------------------------------------------------------------------
;; tests (SPOILERS!)

(module+ test
  (run-standard-tests var->4)
  (test-->> var->4
            (term (prog (let ((x 1)) (let ((y x)) (begin (set! y 3) x)))))
            1))
