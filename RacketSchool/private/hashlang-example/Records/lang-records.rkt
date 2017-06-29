#lang racket

(provide record->1 record->2 record->3)

(require redex)

;; ---------------------------------------------------------------------------------------------------
(define-language record-syntax
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; records
     {(s e) ...}
     (e @ e)
     ;; strings
     s
     (empty? e)
     (e ++ e)
     ;; standard material 
     x
     (e e)
     (λ (x) e)
     (let ((x e)) e))
  (s ::=
     string)
  (b ::=
     true
     false)
  (x ::=
     variable-not-otherwise-mentioned)
  #:binding-forms 
  (λ (x) e #:refers-to x)
  (let ((x e)) e #:refers-to x))

;; ---------------------------------------------------------------------------------------------------
;; some examples 

;; two String conditionals 
(define ex-tt (term (if (empty? "") "hell" "no")))
(define ex-ff (term (if (empty? "way to go") "hell" "no")))

;; dealing with records 
(define ex-rec (term {("one" "hell") ("two" "no")}))
(define ex-@ (term (,ex-rec @ "two")))
(define ex-dyn (term (,ex-rec @ ("t" ++ "wo"))))

;; using lambda to name records 
(define ex3 (term ((λ (r) ((r @ "two") ++ (r @ "one"))) ,ex-rec)))

;; implicit coercion
(define ex-coerc (term ({("true" "t") ("false" "f")} @ false)))

;; ---------------------------------------------------------------------------------------------------
;; substitution (default)

(module+ test
  (default-language record-syntax)
  (test-equal (term (substitute (λ (x) y) y "hell")) (term (λ (x) "hell")))
  (test-equal (term (substitute (λ (x) y) y (x "hell"))) (term (λ (x_1) (x "hell")))))

;; ---------------------------------------------------------------------------------------------------
;; run-time

(define-extended-language record-lang-base record-syntax
  (v ::=
     b
     s
     {(s v) ...}
     (λ (x) e))
  (E ::=
     hole
     ;; boleans
     (if E e e)
     ;; record 
     {(s v) ... (s E) (s e) ...}
     ;; strings
     (empty? E)
     (E ++ e)
     (v ++ E)
     ;; standard
     (E e)
     (v E)
     (let ((x E)) e)))

(define-extended-language record-lang-1 record-lang-base
  (E ::= ....
     (E @ s)))

(define-extended-language record-lang-2 record-lang-base
  (E ::= ....
     (E @ e)
     (v @ E)))

(define-extended-language record-lang-3 record-lang-base
  (E ::= ....
     (E @ e)
     (v @ E)))

(define record->1
  (reduction-relation record-lang-1
   ;; functions
   (--> (in-hole E ((λ (x) e) v)) (in-hole E (substitute e x v)))
   ;; booleans
   (--> (in-hole E (if true e_then e_else)) (in-hole E e_then))
   (--> (in-hole E (if false e_then e_else)) (in-hole E e_else))
   ;; strings
   (--> (in-hole E (s_1 ++ s_2)) (in-hole E ,(string-append (term s_1) (term s_2))))
   (--> (in-hole E (empty? "")) (in-hole E true))
   (--> (in-hole E (empty? v)) (in-hole E false)
        (side-condition (not (equal? (term v) ""))))
   ;; records
   (--> (in-hole E ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s)) (in-hole E v))))

(define record->2
  (reduction-relation record-lang-2
   ;; functions
   (--> (in-hole E ((λ (x) e) v)) (in-hole E (substitute e x v)))
   ;; booleans
   (--> (in-hole E (if true e_then e_else)) (in-hole E e_then))
   (--> (in-hole E (if false e_then e_else)) (in-hole E e_else))
   ;; strings
   (--> (in-hole E (s_1 ++ s_2)) (in-hole E ,(string-append (term s_1) (term s_2))))
   (--> (in-hole E (empty? "")) (in-hole E true))
   (--> (in-hole E (empty? v)) (in-hole E false)
        (side-condition (not (equal? (term v) ""))))
   ;; records
   (--> (in-hole E ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s)) (in-hole E v))))

(define record->3
  (reduction-relation record-lang-3
   ;; functions
   (--> (in-hole E ((λ (x) e) v)) (in-hole E (substitute e x v)))
   ;; booleans
   (--> (in-hole E (if true e_then e_else)) (in-hole E e_then))
   (--> (in-hole E (if false e_then e_else)) (in-hole E e_else))
   ;; strings
   (--> (in-hole E (s_1 ++ s_2)) (in-hole E ,(string-append (term s_1) (term s_2))))
   (--> (in-hole E (empty? "")) (in-hole E true))
   (--> (in-hole E (empty? v)) (in-hole E false)
        (side-condition (not (equal? (term v) ""))))
   ;; records
   (--> (in-hole E ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s)) (in-hole E v))
   (--> (in-hole E (v @ true)) (in-hole E (v @ "true")))
   (--> (in-hole E (v @ false)) (in-hole E (v @ "false")))))

(module+ test
  (test-->> record->1 ex-tt "hell")
  (test-->> record->1 ex-ff "no")
  (test-->> record->1 ex-@ "no")
  (test-->> record->1 ex3 "nohell")
  (test-->> record->1 ex-dyn (term ((("one" "hell") ("two" "no")) @ ("t" ++ "wo")))))

(module+ test
  (test-->> record->2 ex-tt "hell")
  (test-->> record->2 ex-ff "no")
  (test-->> record->2 ex-@ "no")
  (test-->> record->2 ex3 "nohell")
  (test-->> record->2 ex-dyn "no"))

(module+ test
  (test-->> record->3 ex-tt "hell")
  (test-->> record->3 ex-ff "no")
  (test-->> record->3 ex-@ "no")
  (test-->> record->3 ex3 "nohell")
  (test-->> record->3 ex-dyn "no")
  (test-->> record->3 ex-coerc "f"))


