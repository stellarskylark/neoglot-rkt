#lang racket/base
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define neoglot-lexer
      (lexer
           [(char-set "{}(),$") lexeme]
           [(:or "-" "+" "->" ":" "=" "!=" "@" "|" "&")
            (token 'INFIX-OP lexeme)]
           [(char-set "*?") (token 'SUFFIX-OP lexeme)]
           [(:or "ill" "illegal" "syll" "syllable" "mut" "mutate")
            (token 'UNNAMED-BLOCK lexeme)]
           [(:or "cat" "category")
            (token 'NAMED-BLOCK lexeme)]
           [(:* alphabetic) (token 'ID lexeme)]
           [(from/to "//" "\n") (next-token)]
           [whitespace (next-token)]))
    (neoglot-lexer port))
  next-token)
(provide make-tokenizer)
