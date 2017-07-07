#lang racket

(require "mystery.rkt")
(require test-engine/racket-tests)

;; just testing that mystery languages can be run

(check-expect
 (run functions1
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (+ x 1))
            (twice inc)))
 3)

(check-expect
 (run functions2
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (+ x 1))
            (twice inc)))
 2)

(check-expect
 (run functions3
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (+ x 1))
            (twice inc)))
 3)

(check-expect
 (let-values (((a b c)
               (run-all functions1 functions2 functions3
                        (prog (defun (twice f) (f (f 1)))
                              (defun (inc x) (+ x 1))
                              (twice inc)))))
   (list a b c))
 (list 3 2 3))

(check-satisfied
  (run records1
       (prog (let ((r (record ("doghouse" 1))))
               (@ r (++ "dog" "house")))))
  stuck?)

(check-expect
 (run records2
      (prog (let ((r (record ("doghouse" 1))))
              (@ r (++ "dog" "house")))))
 1)

(check-satisfied
 (run records2
      (prog (let ((r (record ("true" 1)))) (@ r true))))
 stuck?)

(check-expect
 (run records3
      (prog (let ((r (record ("true" 1)))) (@ r true))))
 1)

(check-expect
 (let-values (((a b c)
               (run-all records1 records2 records3
                        (prog (let ((r (record ("true" 1)))) (@ r true))))))
   (list a b c))
 (list 'stuck 'stuck 1))

(check-expect
 (run variables1
      (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))
 1)

(check-expect
 (run variables2
      (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))
 2)

(check-expect
 (run variables3
      (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))
 2)

(check-expect
 (let-values (((a b c)
               (run-all variables1 variables2 variables3
                        (prog (defun (f x) (set! x (+ x 1))) (let ((x 1)) (begin (f x) x))))))
   (list a b c))
 (list 1 2 2))

(test)