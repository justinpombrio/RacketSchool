#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-functions.rkt")

(define-language
  #:module-name RacketSchool/Functions3
  #:reductions func->3
  #:grammar func-lang-3
  #:defn-pattern  (defun (x_1 x_2) e_1))
