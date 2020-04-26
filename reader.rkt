#lang racket/base
(require "tokenizer.rkt" "parser.brg")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module ng-module neoglot/expander
                                ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)
