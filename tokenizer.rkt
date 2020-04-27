#lang racket/base
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define neoglot-lexer
      (lexer
           [(from/to "//" "\n") (next-token)]
           [whitespace (next-token)]  
           [(char-set "{}(),$")
            (token lexeme
              #:line (line lexeme-start)
              #:column (col lexeme-start)
              #:position (pos lexeme-start)
              #:span (- (pos lexeme-end)
                        (pos lexeme-start)))]
           [(:or "-" "+" "->" ":" "=" "!=" "@" "|" "&")
            (token 'INFIX-OP lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(char-set "*?")
            (token 'SUFFIX-OP lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(:or "ill" "illegal" "syll" "syllable" "mut" "mutate")
            (token 'UNNAMED-BLOCK lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(:or "cat" "category")
            (token 'NAMED-BLOCK lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(:* alphabetic)
            (token 'ID lexeme                   
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]))
    (neoglot-lexer port))
  next-token)
(provide make-tokenizer)
