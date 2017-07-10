#lang racket

(require "mystery.rkt"
         "mystery-records.rkt"
         "mystery-functions.rkt"
         "mystery-variables.rkt")
(require test-engine/racket-tests)

;; just testing that mystery languages can be run

(check-expect
 (run func->1
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (+ x 1))
            (twice inc)))
 3)

(check-expect
 (run func->2
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (+ x 1))
            (twice inc)))
 2)

(check-expect
 (run func->3
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (+ x 1))
            (twice inc)))
 3)

(check-expect
 (let-values (((a b c)
               (run-all func->1 func->2 func->3
                        (prog (defun (twice f) (f (f 1)))
                              (defun (inc x) (+ x 1))
                              (twice inc)))))
   (list a b c))
 (list 3 2 3))

(check-satisfied
  (run record->1
       (prog (let ((r (record ("doghouse" 1))))
               (@ r (++ "dog" "house")))))
  stuck?)

(check-expect
 (run record->2
      (prog (let ((r (record ("doghouse" 1))))
              (@ r (++ "dog" "house")))))
 1)

(check-satisfied
 (run record->2
      (prog (let ((r (record ("true" 1)))) (@ r true))))
 stuck?)

(check-expect
 (run record->3
      (prog (let ((r (record ("true" 1)))) (@ r true))))
 1)

(check-expect
 (let-values (((a b c)
               (run-all record->1 record->2 record->3
                        (prog (let ((r (record ("true" 1)))) (@ r true))))))
   (list a b c))
 (list 'stuck 'stuck 1))

(check-expect
 (run var->1
      (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))
 1)

(check-expect
 (run var->2
      (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))
 2)

(check-expect
 (run var->3
      (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))
 2)

(check-expect
 (let-values (((a b c)
               (run-all var->1 var->2 var->3
                        (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))))
   (list a b c))
 (list 1 2 2))

(test)
