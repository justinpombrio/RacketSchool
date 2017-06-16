#lang racket

(provide func->)

(require redex)

;; ---------------------------------------------------------------------------------------------------
(define-language func-lang
  (p ::=
     (prog f ... g ... e))
  (g ::=
     (defun (x x) e))
  (f ::=
     (defun e (x x) e))
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; strings
     s
     (empty? e)
     (e ++ e)
     ;; numbers
     n
     (e + e)
     (zero? e)
     ;; functions
     x
     (e e)
     (return x e))
  (b ::=
     true
     false)
  (s ::=
     string)
  (n ::=
     number)
  (x ::=
     variable-not-otherwise-mentioned)
  (v ::=
     x ;;  :-/
     b
     s
     n)
  (P ::=
     (prog f ... E))
  (E ::=
     hole
     ;; numbers
     (zero? E)
     (E + e)
     (v + E)
     ;; booleans
     (if E e e)
     ;; strings
     (empty? E)
     (E ++ e)
     (v ++ E)
     ;; functions
     (E e)
     (v E)
     (return x E))
  #:binding-forms 
  (λ (x) e #:refers-to x)
  ((defun (x_1 x_2) e_1 #:refers-to (shadow x_1 ...)) ... e #:refers-to (shadow x_1 ...)))

;; ---------------------------------------------------------------------------------------------------
;; some examples

;; using let
;(define ex-let (let (x false) (if x "yeah" "nah")))

;; triangular numbers
(define ex-tri (term (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))))

;; passing functions
(define ex-twice (term (prog (defun (twice f) (f (f 1))) (defun (inc x) (x + 1)) (twice inc))))

;; two String conditionals 
(define ex-tt (term (prog (if (empty? "") "hell" "no"))))
(define ex-ff (term (prog (if (empty? "way to go") "hell" "no"))))

;; ---------------------------------------------------------------------------------------------------
;; run-time

(define func->
  (reduction-relation func-lang
   ;; booleans
   (--> (in-hole P (if true e_then e_else)) (in-hole P e_then))
   (--> (in-hole P (if false e_then e_else)) (in-hole P e_else))
   ;; numbers
   (--> (in-hole P (n_1 + n_2)) (in-hole P ,(+ (term n_1) (term n_2))))
   (--> (in-hole P (zero? 0)) (in-hole P true))
   (--> (in-hole P (zero? n)) (in-hole P false)
        (side-condition (not (equal? 0 (term n)))))
   ;; strings
   (--> (in-hole P (s_1 ++ s_2)) (in-hole P ,(string-append (term s_1) (term s_2))))
   (--> (in-hole P (empty? "")) (in-hole P true))
   (--> (in-hole P (empty? v)) (in-hole P false)
        (side-condition (not (equal? (term v) ""))))
   ;; init
   (--> (prog (defun (x_f1 x_p1) e_1) (defun (x_f2 x_p2) e_2) ... e)
        (prog (defun 0 (x_f1 x_p1) e_1) (defun 0 (x_f2 x_p2) e_2) ... e))
   ;; eof
   (--> (prog f ... v) v)))

(define func->1
  (extend-reduction-relation func-> func-lang
   ;; call
   (--> (prog f_1 ... (defun e_1 (x_fun x_param) e_body) f_2 ...
              (in-hole E (x_fun v_arg)))
        (prog f_1 ... (defun e_1 (x_fun x_param) e_body) f_2 ...
              (in-hole E (substitute e_body x_param v_arg))))))

(define func->2
  (extend-reduction-relation func-> func-lang
   ;; call
   (--> (prog f_1 ... (defun e_1 (x_fun x_param) e_body) f_2 ...
              (in-hole E (x_fun v_arg)))
        (prog f_1 ... (defun (in-hole E continue) (x_fun x_param) e_body) f_2 ...
              (return x_fun (substitute e_body x_param v_arg))))
   ;; return
   (--> (prog f_1 ... (defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (return x_fun v)))
        (prog f_1 ... (defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (substitute e_cont continue v))))))

(define func->3
  (extend-reduction-relation func-> func-lang
   ;; call
   (--> (prog f_1 ... (defun e_1 (x_fun x_param) e_body) f_2 ...
              (in-hole E (x_fun v_arg)))
        (prog f_1 ... (defun e_1 (x_fun x_param) e_body) f_2 ...
              (substitute e_body x_param v_arg)))))

;(traces func->2 ex-tri)

(module+ test
  (test-->> func->1 (term (prog 3)) 3)
  (test-->> func->1 (term (prog (defun (f x) x) (f 3))) 3)
;  (test-->> func->1 (term (prog (define x (1 + 2)) 4)) 4)
;  (test-->> func->1 (term (prog (define x (1 + 2)) x)) 3)
;  (test-->> func->1 (term (prog (define x (1 + 2)) (x + x))) 6)
  (test-->> func->1 ex-twice 3)
  (test-->> func->1 ex-tri 15))

(module+ test
  (test-->> func->2 (term (prog 3)) 3)
  (test-->> func->2 (term (prog (defun (f x) x) (f 3))) 3)
  (test-->> func->2 ex-twice 3)
  #;(test-->> func->2 ex-tri 15))


(module+ test
  (test-->> func->1 (term (prog 3)) 3)
  (test-->> func->1 (term (prog (defun (f x) x) (f 3))) 3)
  (test-->> func->1 ex-twice 3)
  (test-->> func->1 ex-tri 15))
