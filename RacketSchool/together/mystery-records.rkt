#lang racket

(require redex)
(require "base.rkt")
(require "testing.rkt")

;; ---------------------------------------------------------------------------------------------------
;; syntax

(define-extended-language record-base-lang basic-lang
  (e :: ....
     {(s e) ...}
     (e @ e))
  (E ::= ....
     {(s v) ... (s E) (s e) ...})
  (v ::= ....
     {(s v) ...}))


(define-extended-language record-lang-1 record-base-lang
  (E ::= ....
     (E @ e)))

(define-extended-language record-lang-2 record-base-lang
  (E ::= ....
     (E @ e)
     (v @ E)))

(define-extended-language record-lang-3 record-base-lang
  (E ::= ....
     (E @ e)
     (v @ E)))

;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define record->1
  (extend-reduction-relation basic-> record-lang-1
   ;; records
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v))))

(define record->2
  (extend-reduction-relation basic-> record-lang-2
   ;; records
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v))))

(define record->3
  (extend-reduction-relation basic-> record-lang-3
   ;; records
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v))
   (--> (in-hole P (v @ true))
        (in-hole P (v @ "true")))
   (--> (in-hole P (v @ false))
        (in-hole P (v @ "false")))
   (--> (in-hole P (v @ n))
        (in-hole P (v @ ,(number->string (term n)))))))

;; ---------------------------------------------------------------------------------------------------
;; tests

;; common tests

(define-test ex-record-1
  (prog {("x" ("a" ++ "b")) ("y" (empty? ""))})
  {("x" "ab") ("y" true)})

(define-test ex-record-2
  (prog ({("x" true) ("y" false)} @ "x"))
  true)

(define-test ex-record-3
  (prog ({("x" true) ("y" false)} @ "y"))
  false)

(define-test ex-record-4
  (prog (defun (f r) ((r @ "two") ++ (r @ "one")))
        (f {("one" "k") ("two" "o")}))
  "ok")

(define-test ex-dyn-no
  (prog ({("one" "k") ("two" "o")} @ ("on" ++ "e")))
  (prog ({("one" "k") ("two" "o")} @ ("on" ++ "e"))))

(define-test ex-dyn-yes
  (prog ({("one" "k") ("two" "o")} @ ("on" ++ "e")))
  "k")

(define-test ex-coerc-1-no
  (prog ({("true" "k") ("false" "o")} @ (empty? "")))
  (prog ({("true" "k") ("false" "o")} @ (empty? ""))))

(define-test ex-coerc-1-kinda
  (prog ({("true" "k") ("false" "o")} @ (empty? "")))
  (prog ({("true" "k") ("false" "o")} @ true)))

(define-test ex-coerc-1-yes
  (prog ({("true" "k") ("false" "o")} @ (empty? "")))
  "k")

(define-test ex-coerc-2-no
  (prog ({("true" "k") ("false" "o")} @ (empty? "b")))
  (prog ({("true" "k") ("false" "o")} @ (empty? "b"))))

(define-test ex-coerc-2-kinda
  (prog ({("true" "k") ("false" "o")} @ (empty? "b")))
  (prog ({("true" "k") ("false" "o")} @ false)))

(define-test ex-coerc-2-yes
  (prog ({("true" "k") ("false" "o")} @ (empty? "b")))
  "o")

(define-test ex-coerc-3-no
  (prog (let ((r {("7" 1) ("8" 2)})) ((r @ 7) + (r @ 8))))
  (prog (({("7" 1) ("8" 2)} @ 7) + ({("7" 1) ("8" 2)} @ 8))))

(define-test ex-coerc-3-yes
  (prog (let ((r {("7" 1) ("8" 2)})) ((r @ 7) + (r @ 8))))
  3)


(module+ test
  (run-standard-tests record->1)
  (run-tests record->1 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->1 ex-dyn-no)
  (run-test record->1 ex-coerc-1-no)
  (run-test record->1 ex-coerc-2-no)
  (run-test record->1 ex-coerc-3-no))

(module+ test
  (run-standard-tests record->2)
  (run-tests record->2 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->2 ex-dyn-yes)
  (run-test record->2 ex-coerc-1-kinda)
  (run-test record->2 ex-coerc-2-kinda)
  (run-test record->2 ex-coerc-3-no))

(module+ test
  (run-standard-tests record->3)
  (run-tests record->3 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->3 ex-dyn-yes)
  (run-test record->3 ex-coerc-1-yes)
  (run-test record->3 ex-coerc-2-yes)
  (run-test record->3 ex-coerc-3-yes))
