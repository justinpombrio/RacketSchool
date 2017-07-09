#lang racket/base

(provide define-language)

;; Defines a language like `Records1` in terms of a Redex reduction
;; like `records1` --- given the grammar and a pattern for recognizing
;; definitions.

(define-syntax-rule (define-language
                      #:module-name lang-module-name
                      #:reductions reductions
                      #:grammar grammar-id
                      #:defn-pattern defn-pattern)
  (...
   (begin
     
     (provide
      (rename-out
       [top-interaction #%top-interaction]
       [module-begin    #%module-begin]))

     ;; ---------------------------------------------------------------------------------------------------
     ;; dependencies

     (require RacketSchool/private/mystery
              (for-syntax racket/base
                          syntax/parse)
              redex/reduction-semantics)

     ;; ---------------------------------------------------------------------------------------------------
     ;; implementation

     (define-syntax (top-interaction stx)
       (syntax-case stx ()
         [(_ . e)
          #`(#%top-interaction . (run reductions (prog ,@definitions e)))]))

     (define-syntax (module-begin stx)
       (syntax-parse stx
         [(_ defns:id e ...) ; the `defns` identifier is added by the reader
          #`(#%module-begin
             (define defns
               (filter (redex-match? grammar-id defn-pattern) (term (e ...))))
             (run reductions (prog e ...)))]))

     ;; ---------------------------------------------------------------------------------------------------
     ;; reader

     (module reader syntax/module-reader
       lang-module-name
       
       #:read read
       #:read-syntax read-syntax
       #:wrapper1 (λ (x) (cons 'definitions (x))) ; include `definitions` as if in the original
       
       (require racket)))))
