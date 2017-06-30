#lang s-exp syntax/module-reader

Records2/lang

#:read read
#:read-syntax read-syntax
#:wrapper1 (λ (x) (cons dw (x)))

(require racket)

(define dw '*definitions-window)
(provide dw)