#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-variables.rkt")

(define-language-set
  #:module-name RacketSchool/VariablesAll
  #:reductions1 var->1
  #:reductions2 var->2
  #:reductions3 var->3
  #:grammar var-lang
  #:defn-pattern  (defun (x_1 x_2) e_1))
