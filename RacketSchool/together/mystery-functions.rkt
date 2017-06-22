#lang racket

(require redex)
(require "base.rkt")
(require "testing.rkt")

;; Language 1: standard
;; Language 2: COBOL
;; Language 3: goto

;; ---------------------------------------------------------------------------------------------------
;; syntax

(define-extended-language func-lang-1 basic-lang)

(define-extended-language func-lang-2 basic-lang
  (p ::=
     (prog cf ... f ... e))
  (cf ::=
     (defun e (x x) e))
  (e ::= ....
     (return x e))
  (E ::= ....
     (return x E)))

(define-extended-language func-lang-3 basic-lang)

;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define func->1 (extend-reduction-relation basic-> func-lang-1))

(define func->2
  (extend-reduction-relation basic-> func-lang-2
   ;; init
   (--> (prog (defun (x_f1 x_p1) e_1) (defun (x_f2 x_p2) e_2) ... e)
        (prog (defun 0 (x_f1 x_p1) e_1) (defun 0 (x_f2 x_p2) e_2) ... e))
   ;; termination
   (--> (prog cf ... v)
        v)
   ;; call
   (--> (prog cf_1 ... (defun e_1 (x_fun x_param) e_body) cf_2 ...
              (in-hole E (x_fun v_arg)))
        (prog cf_1 ... (defun (in-hole E continue) (x_fun x_param) e_body) cf_2 ...
              (return x_fun (substitute e_body x_param v_arg))))
   ;; return
   (--> (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (return x_fun v)))
        (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (substitute e_cont continue v))))))

(define func->3
  (extend-reduction-relation basic-> func-lang-2
   ;; call
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E (x_fun v_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (substitute e_body x_param v_arg)))))


;; ---------------------------------------------------------------------------------------------------
;; tests

;; triangular numbers
(define ex-tri (term (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))))

(define-test ex-tri-std
  (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))
  15)

(define-test ex-tri-goto
  (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))
  0)

;; passing functions
(define ex-twice (term (prog (defun (twice f) (f (f 1))) (defun (inc x) (x + 1)) (twice inc))))


(module+ test
  (run-standard-tests func->1)
  (run-test func->1 ex-tri-std))

(module+ test
  (run-bool-tests func->2)
  (run-str-tests func->2)
  (run-num-tests func->2)
  (run-let-tests func->2))

(module+ test
  (run-bool-tests func->3)
  (run-str-tests func->3)
  (run-num-tests func->3)
  (run-let-tests func->3)
  (run-test func->3 ex-tri-goto))
 