#lang racket/base
(require (for-syntax syntax/parse))
(require (for-syntax racket/base))

(define-syntax (ng-mb stx)
  (syntax-parse stx
    [((~literal ng-mb) PARSE-TREE)
     #'(#%module-begin 'PARSE-TREE)]))
(provide (rename-out [ng-mb #%module-begin]))

(provide #%top #%app #%datum #%top-interaction)
