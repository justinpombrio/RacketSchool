#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-records.rkt")

(define-language-set
  #:module-name RacketSchool/RecordsAll
  #:reductions1 record->1
  #:reductions2 record->2
  #:reductions3 record->3
  #:grammar record-syntax
  #:defn-pattern  (defun (x_1 x_2) e_1))
