#lang racket

(require redex)
(require "testing.rkt")

(provide (all-defined-out))

;; A basic language that all of the mystery languages will build upon.

;; ---------------------------------------------------------------------------------------------------
;; syntax

(define-language event-loop-syntax
  (l ::= (event-loop (event ...) ...))
  (event ::=
         (key s)
         (resume e))
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (zero? e)
     (+ e e)
     ;; strings
     s
     (empty? e)
     (++ e e)
     ;; functions & let
     (e e)
     x
     (let ((x e)) e)
     ;; yield!
     (yield e))
  (x ::= variable-not-otherwise-mentioned)
  (b ::= true false)
  (n ::= number)
  (s ::= string)
  (v ::=
     b
     n
     s)
  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))
  

;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define-extended-language event-loop-lang event-loop-syntax
  (L ::=
     (event-loop [(resume E) event ...] [event ...] ...))
  (E ::=
     hole
     ;; booleans
     (if E e e)
     ;; numbers
     (zero? E)
     (+ E e)
     (+ v E)
     ;; strings
     (empty? E)
     (++ E e)
     (++ v E)
     ;; functions & let
     (E e)
     (v E)
     (let ((x E)) e)
     ;; yield
     (yield E)))


(define event-loop->
  (reduction-relation event-loop-lang
   #:domain l
   ;; booleans
   (--> (in-hole L (if true e_then e_else))
        (in-hole L e_then)
        e-if-true)
   (--> (in-hole L (if false e_then e_else))
        (in-hole L e_else)
        e-if-false)
   ;; numbers
   (--> (in-hole L (zero? 0))
        (in-hole L true)
        e-zero-yes)
   (--> (in-hole L (zero? n))
        (in-hole L false)
        (side-condition (not (equal? (term n) 0)))
        e-zero-no)
   (--> (in-hole L (+ n_1 n_2))
        (in-hole L ,(+ (term n_1) (term n_2)))
        e-plus)
   ;; strings
   (--> (in-hole L (empty? ""))
        (in-hole L true)
        e-empty-yes)
   (--> (in-hole L (empty? s))
        (in-hole L false)
        (side-condition (not (equal? (term s) "")))
        e-empty-no)
   (--> (in-hole L (++ s_1 s_2))
        (in-hole L ,(string-append (term s_1) (term s_2)))
        e-append)
   ;; let
   (--> (in-hole L (let ((x v)) e))
        (in-hole L (substitute e x v))
        e-let)
    ;; key event
    (--> (event-loop [(key s) event_1 ...]
                     [event_2 ...]
                     ...)
         ,(begin (println (string-append "key:" (term s)))
                 (term (event-loop [event_1 ...]
                                   [event_2 ...]
                                   ...)))
         e-event)
    ;; yield
    (--> (event-loop [(resume (in-hole E (yield v))) event_1 ...]
                     [event_2 ...]
                     ...)
         (event-loop [event_1 ... (resume (in-hole E v))]
                     [event_2 ...]
                     ...)
         e-yield)
    ;; done
    (--> (event-loop [(resume v) event_1 ...] [event_2 ...] ...)
         (event-loop [event_1 ...]
                     [event_2 ...]
                     ...)
         e-done-1)
    (--> (event-loop []
                     [event_2 ...]
                     ...)
         (event-loop [event_2 ...]
                     ...)
         e-done-2)))



;; ---------------------------------------------------------------------------------------------------
;; tests

(define-test ex-bool-1
  (event-loop [(resume (if true "yes" "no"))])
  (event-loop [(resume "yes")]))

(define-test ex-bool-2
  (event-loop [(resume (if false "yes" "no"))])
  (event-loop [(resume "no")]))

(define-test ex-str-1
  (event-loop [(resume (empty? ""))])
  (event-loop [(resume true)]))

(define-test ex-str-2
  (event-loop [(resume (empty? " "))])
  (event-loop [(resume false)]))

(define-test ex-str-3
  (event-loop [(resume (++ "abc" "def"))])
  (event-loop [(resume "abcdef")]))

(define-test ex-num-1
  (event-loop [(resume (zero? 0))])
  (event-loop [(resume true)]))

(define-test ex-num-2
  (event-loop [(resume (zero? -1))])
  (event-loop [(resume false)]))

(define-test ex-num-3
  (event-loop [(resume (+ 1 2))])
  (event-loop [(resume 3)]))

(define-test ex-let-1
  (event-loop [(resume (let ((x 3)) x))])
  (event-loop [(resume 3)]))

(define-test ex-let-2
  (event-loop [(resume (let ((x 1)) (let ((x (+ x 1))) (+ x 1))))])
  (event-loop [(resume 3)]))

(define-test ex-let-3
  (event-loop [(resume (+ 1 (let ((x 1)) (+ x 1))))])
  (event-loop [(resume 3)]))

(define t-event-1
  (term (event-loop [(resume (+ 1 2))
                      (key "a")])))

(define t-event-2
  (term (event-loop [(resume (+ 1 2)) (key "a") (key "b")]
                    [(key "c")])))

(define t-event-3
  (term (event-loop [(resume (+ (yield 1) (yield 2)))
                     (key "a")
                     (key "b")]
                    [(key "c")])))

(define t-event-4
  (term (event-loop [(resume (let ((x (yield 1)))
                               (yield (+ x x))))
                     (key "a")
                     (key "b")])))

(traces event-loop-> t-event-3)

(define-syntax-rule (run-bool-tests lang) (run-tests lang ex-boolt ex-bool-2))
(define-syntax-rule (run-str-tests lang)  (run-tests lang ex-str-1 ex-str-2 ex-str-3))
(define-syntax-rule (run-num-tests lang)  (run-tests lang ex-num-1 ex-num-2 ex-num-3))
;(define-syntax-rule (run-func-tests lang) (run-tests lang ex-defun-1 ex-defun-2 ex-defun-3 ex-defun-4 ex-defun-5 ex-defun-6))
(define-syntax-rule (run-let-tests lang)  (run-tests lang ex-let-1 ex-let-2 ex-let-3))

(define-syntax-rule
  (run-standard-tests lang)
  (begin (run-bool-tests lang)
         (run-str-tests lang)
         (run-num-tests lang)
         (run-let-tests lang)))

#;(module+ test (run-standard-tests event-loop->))
