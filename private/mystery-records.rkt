#lang racket

(provide record->1 record->2 record->3)
(provide record-lang-1 record-lang-2 record-lang-3)

;; ---------------------------------------------------------------------------------------------------
(require redex)
(require "basic.rkt")
(require "testing.rkt")


;; ---------------------------------------------------------------------------------------------------
;; syntax: shared

(define-extended-language record-syntax basic-lang
  (e ::= ....
     (record (s e) ...)
     (@ e e))
  (v ::= ....
     (record (s v) ...)))


;; ---------------------------------------------------------------------------------------------------
;; lanuage 1: Field lookup must be on string literals

(define-extended-language record-lang-1 record-syntax
  (E ::= ....
     (record (s v) ... (s E) (s e) ...)
     (@ E e)))

(define record->1
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_1 ...)))))
        e-at)))


;; ---------------------------------------------------------------------------------------------------
;; lanuage 2: Field lookup can be on any expression that evaluates to a string

(define-extended-language record-lang-2 record-syntax
  (E ::= ....
     (record (s v) ... (s E) (s e) ...)
     (@ E e)
     (@ v E)))

(define record->2
  (extend-reduction-relation basic-> record-lang-2
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_1 ...)))))
        e-at)))


;; ---------------------------------------------------------------------------------------------------
;; lanuage 3: Field lookup can be on any expression; it will be coerced to a string

(define-extended-language record-lang-3 record-syntax
  (E ::= ....
     (record (s v) ... (s E) (s e) ...)
     (@ E e)
     (@ v E)))

(define record->3
  (extend-reduction-relation basic-> record-lang-3
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_1 ...)))))
        e-at)
   (--> (in-hole P (@ v true))
        (in-hole P (@ v "true"))
        e-coerce-true)
   (--> (in-hole P (@ v false))
        (in-hole P (@ v "false"))
        e-coerce-false)
   (--> (in-hole P (@ v n))
        (in-hole P (@ v ,(number->string (term n))))
        e-coerce-number)))




;; ---------------------------------------------------------------------------------------------------
;; tests

(define-test ex-record-1
  (prog (record ("x" (++ "a" "b")) ("y" (empty? ""))))
  (record ("x" "ab") ("y" true)))

(define-test ex-record-2
  (prog (@ (record ("x" true) ("y" false)) "x"))
  true)

(define-test ex-record-3
  (prog (@ (record ("x" true) ("y" false)) "y"))
  false)

(define-test ex-record-4
  (prog (defun (f r) (++ (@ r "two") (@ r "one")))
        (f (record ("one" "k") ("two" "o"))))
  "ok")

(define-test ex-dyn-no
  (prog (@ (record ("one" "k") ("two" "o")) (++ "on" "e")))
  (prog (@ (record ("one" "k") ("two" "o")) (++ "on" "e"))))

(define-test ex-dyn-yes
  (prog (@ (record ("one" "k") ("two" "o")) (++ "on" "e")))
  "k")

(define-test ex-coerc-1-no
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "")))
  (prog (@ (record ("true" "k") ("false" "o")) (empty? ""))))

(define-test ex-coerc-1-kinda
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "")))
  (prog (@ (record ("true" "k") ("false" "o")) true)))

(define-test ex-coerc-1-yes
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "")))
  "k")

(define-test ex-coerc-2-no
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "b")))
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "b"))))

(define-test ex-coerc-2-kinda
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "b")))
  (prog (@ (record ("true" "k") ("false" "o")) false)))

(define-test ex-coerc-2-yes
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "b")))
  "o")

(define-test ex-coerc-3-no
  (prog (let ((r (record ("7" 1) ("8" 2)))) (+ (@ r 7) (@ r 8))))
  (prog (+ (@ (record ("7" 1) ("8" 2)) 7) (@ (record ("7" 1) ("8" 2)) 8))))

(define-test ex-coerc-3-yes
  (prog (let ((r (record ("7" 1) ("8" 2)))) (+ (@ r 7) (@ r 8))))
  3)

(define ex-mult-fields
  (term (prog (@ (record ("a" "first") ("b" "middle") ("a" "last")) "a"))))


(module+ test
  (run-standard-tests record->1)
  (run-tests record->1 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->1 ex-dyn-no)
  (run-test record->1 ex-coerc-1-no)
  (run-test record->1 ex-coerc-2-no)
  (run-test record->1 ex-coerc-3-no)
  (test-->> record->1 ex-mult-fields "first"))

(module+ test
  (run-standard-tests record->2)
  (run-tests record->2 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->2 ex-dyn-yes)
  (run-test record->2 ex-coerc-1-kinda)
  (run-test record->2 ex-coerc-2-kinda)
  (run-test record->2 ex-coerc-3-no)
  (test-->> record->2 ex-mult-fields "first"))

(module+ test
  (run-standard-tests record->3)
  (run-tests record->3 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->3 ex-dyn-yes)
  (run-test record->3 ex-coerc-1-yes)
  (run-test record->3 ex-coerc-2-yes)
  (run-test record->3 ex-coerc-3-yes)
  (test-->> record->3 ex-mult-fields "first"))
