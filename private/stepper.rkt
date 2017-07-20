#lang racket
(provide info)
(require syntax/modread
         redex
         racket/gui/base
         racket/draw)

(define (info reduction)
  (list
   (list
    "Redex Stepper"
    (make-object bitmap% 16 16)  ; a 16 x 16 white square
    (λ (drr-window)
      (stepper
       reduction
       (let ([mod
              (with-module-reading-parameterization
                  (λ ()
                    (read
                     (open-input-text-editor
                      (send drr-window get-definitions-text)))))])
         (match mod
           [`(module ,_ ,_
               (#%module-begin
                definitions
                ,defs ...)) (term (prog ,@defs))])
         ))))))