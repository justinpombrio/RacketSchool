#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-functions.rkt")

(define-language-set
  #:module-name RacketSchool/FunctionsAll
  #:reductions1 func->1
  #:reductions2 func->2
  #:reductions3 func->3
  #:grammar func-lang-1
  #:defn-pattern  (defun (x_1 x_2) e_1))
