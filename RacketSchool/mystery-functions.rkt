#lang racket

(require redex)
(require "base.rkt")
(require "testing.rkt")

(provide func->1 func->2 func->3)

;; Language 1: standard
;; Language 2: COBOL
;; Language 3: goto



;; ---------------------------------------------------------------------------------------------------
;; syntax

(define-extended-language func-lang-1 basic-lang)

(define-extended-language func-lang-2 arith-lang
  (p ::= (prog cf ... f ... e))
  (f ::= (defun (x x) e))
  (cf ::= (defun e (x x) e))
  (e ::= ....
     x
     (function x)
     (e e)
     (cobol-return x e)
     (let ((x e)) e))
  (P ::= (prog cf ... E))
  (E ::= ....
     (E e)
     (v E)
     (cobol-return x E)
     (let ((x E)) e))
  (v ::= ....
     (function x))
  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))

(define-extended-language func-lang-3 basic-lang)

;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define func->1 (extend-reduction-relation basic-> func-lang-1))

(define func->2
  (extend-reduction-relation arith-> func-lang-2
   ;; init (NEW)
   (--> (prog (defun (x_f1 x_p1) e_1) (defun (x_f2 x_p2) e_2) ... e)
        (prog (defun 0 (x_f1 x_p1) e_1) (defun 0 (x_f2 x_p2) e_2) ... e))
   ;; termination
   (--> (prog cf ... v)
        v)
   ;; id
   (--> (prog cf_1 ... (defun e_1 (x_fun x_param) e_body) cf_2 ...
              (in-hole E x_fun))
        (prog cf_1 ... (defun e_1 (x_fun x_param) e_body) cf_2 ...
              (in-hole E (function x_fun))))
   ;; let
   (--> (in-hole P (let ((x v)) e))
        (in-hole P (substitute e x v)))
   ;; call (NEW)
   (--> (prog cf_1 ... (defun e_1 (x_fun x_param) e_body) cf_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog cf_1 ... (defun (in-hole E continue) (x_fun x_param) e_body) cf_2 ...
              (cobol-return x_fun (substitute e_body x_param v_arg))))
   ;; return (NEW)
   (--> (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (cobol-return x_fun v)))
        (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (substitute e_cont continue v))))))

(define func->3
  (extend-reduction-relation arith-> func-lang-3
   ;; termination
   (--> (prog f ... v)
        v)
   ;; id
   (--> (prog f ... (in-hole E x_fun))
        (prog f ... (in-hole E (function x_fun)))
        (where (defun (x_fun x_param) e_body) (lookup-fun x_fun (f ...))))
   ;; let
   (--> (in-hole P (let ((x v)) e))
        (in-hole P (substitute e x v)))
   ;; call (NEW)
   (--> (prog f ... (in-hole E ((function x_fun) v_arg)))
        (prog f ... (substitute e_body x_param v_arg))
        (where (defun (x_fun x_param) e_body) (lookup-fun x_fun (f ...))))))

;; ---------------------------------------------------------------------------------------------------
;; tests

;; triangular numbers
(define ex-tri (term (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))))

;; passing functions
(define ex-twice
  (term (prog (defun (twice f) (f (f 1)))
              (defun (inc x) (x + 1))
              (twice inc))))

(module+ test
  (run-standard-tests func->1)
  (test-->> func->1 ex-tri 15)
  (test-->> func->1 ex-twice 3))

(module+ test
  (run-bool-tests func->2)
  (run-str-tests func->2)
  (run-num-tests func->2)
  (run-let-tests func->2)
  (test-->> func->2 ex-twice 3))
;(traces func->2 ex-tri)

(module+ test
  (run-bool-tests func->3)
  (run-str-tests func->3)
  (run-num-tests func->3)
  (run-let-tests func->3)
  (test-->> func->3 ex-tri 0)
  (test-->> func->3 ex-twice 2))


