#lang racket/base

;; This one is written out the long way.

(provide
 (rename-out
  [top-interaction #%top-interaction]
  [module-begin    #%module-begin]))

;; ---------------------------------------------------------------------------------------------------
;; dependencies

(require "private/mystery.rkt"
         "private/mystery-records.rkt"
         (for-syntax racket/base
                     syntax/parse)
         redex/reduction-semantics)


;; ---------------------------------------------------------------------------------------------------
;; implementation

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . e)
     #`(#%top-interaction . (run-all record-lang-1 record->1 (prog ,@definitions e)))]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ defns:id e ...) ; the `defns` identifier is added by the reader
     #`(#%module-begin
        (define defns
          (filter (redex-match? record-lang-1 (defun (x_1 x_2) e_1)) (term (e ...))))
        (run-all record-lang-1 record->1 (prog e ...)))]))

;; ---------------------------------------------------------------------------------------------------
;; reader

(module reader syntax/module-reader
  RacketSchool/Records1
  
  #:read read
  #:read-syntax read-syntax
  #:wrapper1 (λ (x) (cons 'definitions (x))) ; include `definitions` as if in the original
  #:info (λ (key default default-filter)
      (case key
        [(drracket:toolbar-buttons)
         ((dynamic-require 'RacketSchool/private/stepper 'info)
          (dynamic-require 'RacketSchool/private/mystery-records 'record->1))]
        [else default]))
  
  (require racket))

