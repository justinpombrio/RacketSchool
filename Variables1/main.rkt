#lang racket/base
(require "../private/mystery-lang.rkt"
         "../private/mystery-variables.rkt")

(define-language
  #:module-name Variables1
  #:reductions var->1
  #:grammar var-lang
  #:defn-pattern  (defun (x_1 x_2) e_1))
