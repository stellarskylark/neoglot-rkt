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
           [(:or "ill" "illegal")
            (token 'ILL-KEY lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(:or "syll" "syllable")
            (token 'SYLL-KEY lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(:or "mut" "mutate")
            (token 'MUT-KEY lexeme
                   #:line (line lexeme-start)
                   #:column (col lexeme-start)
                   #:position (pos lexeme-start)
                   #:span (- (pos lexeme-end)
                             (pos lexeme-start)))]
           [(:or "cat" "category")
            (token 'CAT-KEY lexeme
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
                             (pos lexeme-start)))]
           [any-char
            (raise-syntax-error 'syntax-error
              (format "unexpected character: '~a' (~a:~a)"
                      lexeme
                      (line lexeme-start)
                      (col lexeme-start)))]))
    (neoglot-lexer port))
  next-token)
(provide make-tokenizer)
