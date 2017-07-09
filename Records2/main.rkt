#lang racket/base
(require "../private/mystery-lang.rkt"
         "../private/mystery-records.rkt")

(define-language
  #:module-name Records2
  #:reductions record->2
  #:grammar record-lang-2
  #:defn-pattern  (defun (x_1 x_2) e_1))
