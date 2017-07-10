#lang racket

(provide func->1 func->2 func->3)
(provide func-syntax func-lang-1 func-lang-2 func-lang-3)

;; ---------------------------------------------------------------------------------------------------
(require redex)
(require "basic.rkt")
(require "testing.rkt")

;; syntax is unchanged
(define-extended-language func-syntax basic-lang)

;; ---------------------------------------------------------------------------------------------------
;; language 1: Regular function calls

(define-extended-language func-lang-1 basic-lang)

(define func->1 (extend-reduction-relation basic-> func-lang-1))


;; ---------------------------------------------------------------------------------------------------
;; language 2: Function calls are gotos!

(define-extended-language func-lang-2 basic-lang)

(define func->2
  (extend-reduction-relation basic-> func-lang-2
   ;; apply
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (substitute e_body x_param v_arg))
        e-apply)))



;; ---------------------------------------------------------------------------------------------------
;; language 3: COBOL-style "recursion": each function has a single statically-allocated return address

; These syntax extensions are meant to be internal, and thus start with '%'.
(define-extended-language func-syntax-3 basic-lang
  (uf ::= (defun (x x) e)) ; uninitialized functions
  (f ::= (%defun e (x x) e)) ; initialized functions
  (e ::= ....
     (%return x e)))

(define-extended-language func-lang-3 func-syntax-3
  (P ::= (prog f ... E))
  (E ::= ....
     (%return x E)))

(define func->3
  (extend-reduction-relation basic-> func-lang-3
   ;; id (standard)
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E x_fun))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (function x_fun)))
        e-id)
   ;; init
   (--> (prog (defun (x_f1 x_p1) e_1) (defun (x_f2 x_p2) e_2) ... e)
        (prog (%defun 0 (x_f1 x_p1) e_1) (%defun 0 (x_f2 x_p2) e_2) ... e)
        e-init)
   ;; apply
   (--> (prog f_1 ... (%defun e_1 (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (%defun (in-hole E continue) (x_fun x_param) e_body) f_2 ...
              (%return x_fun (substitute e_body x_param v_arg)))
        e-apply)
   ;; COBOL-style return
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (%return x_fun v)))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (substitute e_cont continue v))) ;keep the context E?
        e-return)))


;; ---------------------------------------------------------------------------------------------------
;; tests

;; triangular numbers
(define ex-tri (term (prog (defun (tri n) (if (zero? n) 0 (+ n (tri (+ n -1))))) (tri 5))))

;; passing functions
(define ex-twice
  (term (prog (defun (twice f) (f (f 1)))
              (defun (inc x) (+ x 1))
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
  (test-->> func->2 ex-tri 0)
  (test-->> func->2 ex-twice 2))

(module+ test
  (run-bool-tests func->3)
  (run-str-tests func->3)
  (run-num-tests func->3)
  (run-let-tests func->3)
  (test-->> func->3 ex-twice 3))
