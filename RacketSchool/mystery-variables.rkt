#lang racket

(require redex)
(require "base.rkt")
(require "testing.rkt")

;; ---------------------------------------------------------------------------------------------------
;; syntax

(define-extended-language var-lang basic-lang
  (f ::= ....
     (define x v))
  (e ::= ....
     (set! x e)
     (begin e ...))
  (E ::= ....
     (set! x E)
     (begin E e ...)))

;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define var->
  (extend-reduction-relation arith-> var-lang
   ;; termination
   (--> (prog f ... v)
        v)
   ;; function id
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E x_fun))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E (function x_fun))))
   ;; begin
   (--> (in-hole P (begin v))
        (in-hole P v))
   (--> (in-hole P (begin v e_1 e_2 ...))
        (in-hole P (begin e_1 e_2 ...)))
   ;; set!
   (--> (prog f_1 ... (define x v) f_2 ...
              (in-hole E (set! x v_2)))
        (prog f_1 ... (define x v_2) f_2 ...
              (in-hole E v_2)))
   ;; let
   (--> (prog f ...
              (in-hole E (let ((x v)) e)))
        (prog f ... (define x_fresh v)
              (in-hole E (substitute e x x_fresh)))
        (fresh x_fresh))))

(define var->1
  (extend-reduction-relation var-> var-lang
   ;; id
   (--> (prog f_1 ... (define x v) f_2 ... (in-hole E x))
        (prog f_1 ... (define x v) f_2 ... (in-hole E v)))
   ;; apply
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_fresh v)
              (in-hole E (substitute e_body x_param x_fresh)))
        (fresh x_fresh))))

(define var->2
  (extend-reduction-relation var-> var-lang
   ;; id (except in func argument position)
   (--> (prog f_1 ... (define x v) f_2 ... (in-hole E x))
        (prog f_1 ... (define x v) f_2 ... (in-hole E v))
        (side-condition (not (redex-match? var-lang
                                           (in-hole E_any ((function x_any) x_any2))
                                           (term (in-hole E x))))))
   ;; apply (aliasing)
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_arg v) f_3 ...
              (in-hole E ((function x_fun) x_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_arg v) f_3 ...
              (in-hole E (substitute e_body x_param x_arg))))
   (--> (prog f_1 ... (define x_arg v) f_2 ... (defun (x_fun x_param) e_body) f_3 ...
              (in-hole E ((function x_fun) x_arg)))
        (prog f_1 ... (define x_arg v) f_2 ... (defun (x_fun x_param) e_body) f_3 ...
              (in-hole E (substitute e_body x_param x_arg))))
   ;; apply (no aliasing)
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_fresh v)
              (in-hole E (substitute e_body x_param x_fresh)))
        (fresh x_fresh))))

(define var->3
  (extend-reduction-relation var-> var-lang
   ;; id (except in func argument position)
   (--> (prog f_1 ... (define x v) f_2 ... (in-hole E x))
        (prog f_1 ... (define x v) f_2 ... (in-hole E v))
        (side-condition (not (redex-match? var-lang
                                           (in-hole E_any ((function x_any) x_any2))
                                           (term (in-hole E x))))))
   ;; apply (aliasing)
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_arg v) f_3 ...
              (in-hole E ((function x_fun) x_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_arg v) f_3 ... (define x_new v)
              (in-hole E (let ((x_ans (substitute e_body x_param x_new))) (begin (set! x_arg x_new) x_ans))))
        (fresh x_new))
   (--> (prog f_1 ... (define x_arg v) f_2 ... (defun (x_fun x_param) e_body) f_3 ...
              (in-hole E ((function x_fun) x_arg)))
        (prog f_1 ... (define x_arg v) f_2 ... (defun (x_fun x_param) e_body) f_3 ... (define x_new v)
              (in-hole E (let ((x_ans (substitute e_body x_param x_new))) (begin (set! x_arg x_new) x_ans))))
        (fresh x_new))
   ;; apply (no aliasing)
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ... (define x_fresh v)
              (in-hole E (substitute e_body x_param x_fresh)))
        (fresh x_fresh))))

;; ---------------------------------------------------------------------------------------------------
;; tests

(define ex-0
  (term (prog (define x 1)
              (defun (f y) (set! x (x + y)))
              (begin (f x)
                     (f 1)
                     x))))

(define ex-1
  (term (prog (define x 1)
              (defun (f y) (set! y (y + 1)))
              (begin (f x)
                     x))))

(define ex-2
  (term (prog (define x 1)
              (defun (f y) (begin (set! y (x + 1))
                                  (set! y (x + 1))))
              (begin (f x)
                     x))))

(module+ test
  (run-standard-tests var->1)
  (test-->> var->1 ex-0 3)
  (test-->> var->1 ex-1 1)
  (test-->> var->1 ex-2 1))

(module+ test
  (run-standard-tests var->2)
  (test-->> var->2 ex-0 3)
  (test-->> var->2 ex-1 2)
  (test-->> var->2 ex-2 3))

(module+ test
  (run-standard-tests var->3)
  (test-->> var->3 ex-0 2)
  (test-->> var->3 ex-1 2)
  (test-->> var->3 ex-2 2))

