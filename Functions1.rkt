#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-functions.rkt")

(define-language
  #:module-name RacketSchool/Functions1
  #:reductions func->1
  #:grammar func-lang-1
  #:defn-pattern  (defun (x_1 x_2) e_1))
