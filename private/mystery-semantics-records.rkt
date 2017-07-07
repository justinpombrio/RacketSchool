#lang racket

(require redex)
(require "basic.rkt")
(require "testing.rkt")
(require "mystery-records.rkt")

;; ---------------------------------------------------------------------------------------------------
;; **Mystery semantics**
;; How do these languages differ from Records1?
;; Find programs that will demonstrate the difference.

(define record->4
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_2 ...)))))
        e-at)))

(define record->5
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (append (term (s_1 ...)) (term (s_2 ...))))))
        e-at)))


;; ---------------------------------------------------------------------------------------------------
;; tests (SPOILERS!)

(define-test ex-record-1
  (prog (record ("x" (++ "a" "b")) ("y" (empty? ""))))
  (record ("x" "ab") ("y" true)))

(define-test ex-record-2
  (prog (@ (record ("x" true) ("y" false)) "x"))
  true)

(define-test ex-record-3
  (prog (@ (record ("x" true) ("y" false)) "y"))
  false)

(define-test ex-dyn-no
  (prog (@ (record ("one" "k") ("two" "o")) (++ "on" "e")))
  (prog (@ (record ("one" "k") ("two" "o")) (++ "on" "e"))))

(define-test ex-coerc-1-no
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "")))
  (prog (@ (record ("true" "k") ("false" "o")) (empty? ""))))

(define-test ex-coerc-2-no
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "b")))
  (prog (@ (record ("true" "k") ("false" "o")) (empty? "b"))))

(define-test ex-coerc-3-no
  (prog (let ((r (record ("7" 1) ("8" 2)))) (+ (@ r 7) (@ r 8))))
  (prog (+ (@ (record ("7" 1) ("8" 2)) 7) (@ (record ("7" 1) ("8" 2)) 8))))

(define-test ex-record-4
  (prog (defun (f r) (++ (@ r "two") (@ r "one")))
        (f (record ("one" "k") ("two" "o"))))
  "ok")

(define ex-mult-fields
  (term (prog (@ (record ("a" "first") ("b" "middle") ("a" "last")) "a"))))

(module+ test
  (run-standard-tests record->4)
  (run-tests record->4 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->4 ex-dyn-no)
  (run-test record->4 ex-coerc-1-no)
  (run-test record->4 ex-coerc-2-no)
  (run-test record->4 ex-coerc-3-no)
  (test-->> record->4 ex-mult-fields "last"))

(module+ test
  (run-standard-tests record->5)
  (run-tests record->5 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->5 ex-dyn-no)
  (run-test record->5 ex-coerc-1-no)
  (run-test record->5 ex-coerc-2-no)
  (run-test record->5 ex-coerc-3-no)
  (test-->> record->5 ex-mult-fields
            (term (prog (@ (record ("a" "first") ("b" "middle") ("a" "last")) "a")))))
