#lang racket

(provide (rename-out [top-interaction #%top-interaction]
                     [module-begin #%module-begin]))

(require "../mystery.rkt" (for-syntax syntax/parse) redex/reduction-semantics)

(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(_ . e) #'(#%top-interaction . (run records2 (prog e)))]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ e ...) #'(#%module-begin (run records2 (prog e ...)))]))
