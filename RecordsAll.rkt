#lang racket/base
(require "private/mystery-lang.rkt"
         "private/mystery-records.rkt")

(define-language
  #:module-name RacketSchool/RecordsAll
  #:reductions (record->1 record->2 record->3)
  #:grammar record-syntax
  #:defn-pattern  (defun (x_1 x_2) e_1))
