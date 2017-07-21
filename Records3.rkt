#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-records.rkt")

(define-language
  #:module-name RacketSchool/Records3
  #:reductions record->3
  #:grammar record-lang-3
  #:defn-pattern  (defun (x_1 x_2) e_1))
