#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-functions.rkt")

(define-language
  #:module-name RacketSchool/Functions2
  #:reductions func->2
  #:grammar func-lang-2
  #:defn-pattern  (defun (x_1 x_2) e_1))
