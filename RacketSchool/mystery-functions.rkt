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

#;(define-extended-language func-lang-lambda basic-lang
  (e ::= ....
     (λ (x) e)
     (closure e (x) e))
  (v ::= ....
     (closure e (x) e))
  #:binding-forms
  (λ (x) e #:refers-to x))


;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define func->1 (extend-reduction-relation basic-> func-lang-1))

(define func->2
  (extend-reduction-relation arith-> func-lang-2
   ;; init (NEW)
   (--> (prog (defun (x_f1 x_p1) e_1) (defun (x_f2 x_p2) e_2) ... e)
        (prog (defun 0 (x_f1 x_p1) e_1) (defun 0 (x_f2 x_p2) e_2) ... e)
        e-init)
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
              (cobol-return x_fun (substitute e_body x_param v_arg)))
        e-call)
   ;; return (NEW)
   (--> (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (cobol-return x_fun v)))
        (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (substitute e_cont continue v))) ;?
        e-return)))

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

(define-extended-language func-lang-4 func-lang-2
  (e ::= ....
     (return e) ; for use by users
     (call x e))   ; internal use only
  (E ::= ....
     (return E)
     (call x E)))

(define func->4
  (extend-reduction-relation func->2 func-lang-4
   ;; call
   (--> (prog cf_1 ... (defun 0 (x_fun x_param) e_body) cf_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog cf_1 ... (defun 0 (x_fun x_param) e_body) cf_2 ...
              (in-hole E (call x_fun (substitute e_body x_param v_arg))))
        e-call)
   (--> (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E (call x_fun (substitute e_cont continue v_arg))))
        (side-condition (not (equal? (term e_cont) 0))))
   ;; normal return
   (--> (prog cf ... (in-hole E (call x_fun v)))
        (prog cf ... (in-hole E v)))
   ;; return
   (--> (prog cf_1 ... (defun e_cont (x_fun x_param) e_body) cf_2 ...
              (in-hole E_1 (call x_fun (in-hole E_2 (return v)))))
        (prog cf_1 ... (defun (in-hole E_2 continue) (x_fun x_param) e_body) cf_2 ...
              (in-hole E_1 v))
        e-return)))


;; ---------------------------------------------------------------------------------------------------
;; tests

;; triangular numbers
(define ex-tri (term (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))))

;; passing functions
(define ex-twice
  (term (prog (defun (twice f) (f (f 1)))
              (defun (inc x) (x + 1))
              (twice inc))))

#;(define ex-lambda
  (term (prog ((λ (x) x) 3))))

#;(define ex-lambdas
  (term (prog (((λ (f) (λ (n) (f (f n)))) (λ (n) (n + 1))) 1))))

(define-test ex-gen-1
  (prog (defun (f x) ((return 1) + (return 2))) ((f 0) + (f 0)))
  3)

(define-test ex-gen-2
  (prog (defun (f x) ((return 1) + (return 2))) (((f 0) + (f 0)) + (f 0)))
  3)


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

(traces func->4 (term (prog (defun (f x) ((return 1) + (return 2))) (((f 0) + (f 0)) + (f 0)))))

(module+ test
  (run-standard-tests func->4)
  (run-test func->4 ex-gen-1)
  (run-test func->4 ex-gen-2))
