#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-variables.rkt")

(define-language
  #:module-name RacketSchool/Variables2
  #:reductions (var->2)
  #:grammar var-lang
  #:defn-pattern  (defun (x_1 x_2) e_1))
