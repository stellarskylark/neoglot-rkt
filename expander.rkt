#lang racket/base
(require (for-syntax syntax/parse))
(require (for-syntax racket/base))

(define-syntax (ng-mb stx)
  (syntax-parse stx
    [((~literal ng-mb) PARSE-TREE)
     #'(#%module-begin PARSE-TREE)]))
(provide (rename-out [ng-mb #%module-begin]))

(define cat-hash (make-hash))

(define-syntax (arg stx)
  (syntax-parse stx
    [((~literal arg) THE-ARG)
     #'THE-ARG]
    [((~literal arg) "$" THE-ARG)
     #'(hash-ref cat-hash THE-ARG)]
    ))
(provide arg)

(define-syntax (infix-op-exp stx)
  (syntax-parse stx
    [((~literal infix-op-exp) ARG1 "=" ARG2)
     #'(string=? ARG1 ARG2)]
    [((~literal infix-op-exp) ARG1 "!=" ARG2)
     #'(not (string=? ARG1 ARG2))]
    [((~literal infix-op-exp) ARG1 "&" ARG2)
     #'(and ARG1 ARG2)]
    [((~literal infix-op-exp) ARG1 "|" ARG2)
     #'(or ARG1 ARG2)]
    ))
(provide infix-op-exp)

(define-syntax (cat-block stx)
  (syntax-parse stx
    [((~literal cat-block) CAT-NAME BLOCK)
     #'(hash-set! cat-hash CAT-NAME (car BLOCK))]))
(provide cat-block)

; Annoying that these are the same, but even if they
; expand the same, they serve different syntactic functions
(define-syntax (expr stx)
  (syntax-parse stx
    [((~literal expr) ARG ...)
     #'(list ARG ...)]))
(define-syntax (wrap stx)
  (syntax-parse stx
    [((~literal wrap) ARG ...)
     #'(list ARG ...)]))
(provide expr wrap)

(define-syntax (ng-program stx)
  (syntax-parse stx
    [((~literal ng-program) BLOCK ...)
     #'(begin
          ((lambda x BLOCK ...))
          (display cat-hash))]))
(provide ng-program)

(provide #%top #%app #%datum #%top-interaction)
