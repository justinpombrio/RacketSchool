#lang racket

(provide var->)

(require redex)

;; ---------------------------------------------------------------------------------------------------
(define-language var-lang
  (p ::=
     (prog d ... e))
  (d ::=
     (define x v))
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (e + e)
     (zero? e)
     ;; standard
     (let ((x e)) e)
     (set! x e)
     (begin e ...)
     (λ (x) e)
     x
     (e e))
  (b ::=
     true
     false)
  (n ::=
     number)
  (x ::=
     variable-not-otherwise-mentioned)
  
  (v ::=
     (λ (x) e)
     b
     n)
  (P ::=
     (prog d ... E))
  (E ::=
     hole
     ;; numbers
     (zero? E)
     (E + e)
     (v + E)
     ;; booleans
     (if E e e)
     ;; standard
     (let ((x E)) e)
     (set! x E)
     (begin E e ...)
     ;; functions
     (E e)
     (v E))
  #:binding-forms 
  (λ (x) e #:refers-to x)
  (let ((x e)) e #:refers-to x))


;; ---------------------------------------------------------------------------------------------------
;; run-time

(define var->
  (reduction-relation var-lang
   ;; booleans
   (--> (in-hole P (if true e_then e_else)) (in-hole P e_then))
   (--> (in-hole P (if false e_then e_else)) (in-hole P e_else))
   ;; numbers
   (--> (in-hole P (n_1 + n_2)) (in-hole P ,(+ (term n_1) (term n_2))))
   (--> (in-hole P (zero? 0)) (in-hole P true))
   (--> (in-hole P (zero? n)) (in-hole P false)
        (side-condition (not (equal? 0 (term n)))))
   ;; done
   (--> (prog d ... v) v)
   ;; set!
   (--> (prog d_1 ... (define x v) d_2 ... (in-hole E (set! x v_2)))
        (prog d_1 ... (define x v_2) d_2 ... (in-hole E v_2)))
   ;; let
   (--> (prog (define x_1 v_1) ... (in-hole E (let ((x v)) e)))
        (prog (define x_1 v_1) ... (define x_fresh v) (in-hole E (substitute e x x_fresh)))
        (fresh x_fresh))
   ;; begin
   (--> (in-hole P (begin v))
        (in-hole P v))
   (--> (in-hole P (begin v e_1 e_2 ...))
        (in-hole P (begin e_1 e_2 ...)))))

(define var->1
  (extend-reduction-relation var-> var-lang
   ;; variable
   (--> (prog d_1 ... (define x v) d_2 ... (in-hole E x))
        (prog d_1 ... (define x v) d_2 ... (in-hole E v)))
   ;; apply
   (--> (prog d ... (in-hole E ((λ (x) e) v)))
        (prog d ... (define x_fresh v) (in-hole E (substitute e x x_fresh)))
        (fresh x_fresh))))

(define var->2
  (extend-reduction-relation var-> var-lang
   ;; variable (except in func argument position)
   (--> (prog d_1 ... (define x v) d_2 ... (in-hole E x))
        (prog d_1 ... (define x v) d_2 ... (in-hole E v))
        (side-condition (not (redex-match? var-lang
                                           (in-hole E_any (v_any x_any))
                                           (term (in-hole E x))))))
   ;; apply (aliasing)
   (--> (prog d_1 ... (define x_arg v) d_2 ... (in-hole E ((λ (x) e) x_arg)))
        (prog d_1 ... (define x_arg v) d_2 ... (in-hole E (substitute e x x_arg))))
   ;; apply (no aliasing)
   (--> (prog d ... (in-hole E ((λ (x) e) v)))
        (prog d ... (define x_fresh v) (in-hole E (substitute e x x_fresh)))
        (fresh x_fresh))))

(define var->3
  (extend-reduction-relation var-> var-lang
   ;; variable (except in func argument position)
   (--> (prog d_1 ... (define x v) d_2 ... (in-hole E x))
        (prog d_1 ... (define x v) d_2 ... (in-hole E v))
        (side-condition (not (redex-match? var-lang
                                           (in-hole E_any (v_any x_any))
                                           (term (in-hole E x))))))
   ;; apply (aliasing)
   (--> (prog d_1 ... (define x_arg v) d_2 ... (in-hole E ((λ (x) e) x_arg)))
        (prog d_1 ... (define x_arg v) d_2 ... (define x_new v) (in-hole E
          (let ((x_ans (substitute e x x_new))) (begin (set! x_arg x_new) x_ans))))
        (fresh x_new))
   ;; apply (no aliasing)
   (--> (prog d ... (in-hole E ((λ (x) e) v)))
        (prog d ... (define x_fresh v) (in-hole E (substitute e x x_fresh)))
        (fresh x_fresh))))


;(traces var->2 (term (prog (let ((x 1)) (begin ((λ (y) (set! y (y + 1))) x) x)))))

(module+ test
  (test-->> var->1 (term (prog ((λ (x) x) 3))) 3)
  (test-->> var->1 (term (prog (((λ (x) (x x)) (λ (x) x)) 3))) 3)
  (test-->> var->1 (term (prog (let ((x 1))
                                 (let ((f (λ (y) (set! x (x + y)))))
                                   (begin (f x) (f 1) x))))) 3)
  #;(test-->> var->1 (term (prog (let ((f (λ (x) z))) (let ((z 1)) (f 2))))) 0) ; unbound
  (test-->> var->1 (term (prog (let ((x 1)) (begin ((λ (y) (set! y (y + 1))) x) x)))) 1)
  (test-->> var->1 (term (prog (let ((x 1))
                                 (let ((f (λ (y) (begin (set! y (x + 1)) (set! y (x + 1))))))
                                   (begin (f x) x))))) 1))

(module+ test
  (test-->> var->2 (term (prog (let ((x 1)) (begin ((λ (y) (set! y (y + 1))) x) x)))) 2)
  (test-->> var->2 (term (prog (let ((x 1))
                                 (let ((f (λ (y) (begin (set! y (x + 1)) (set! y (x + 1))))))
                                   (begin (f x) x))))) 3))

(module+ test
  (test-->> var->3 (term (prog (let ((x 1)) (begin ((λ (y) (set! y (y + 1))) x) x)))) 2)
  (test-->> var->3 (term (prog (let ((x 1))
                                 (let ((f (λ (y) (begin (set! y (x + 1)) (set! y (x + 1))))))
                                   (begin (f x) x))))) 2))
