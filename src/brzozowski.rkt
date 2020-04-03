#lang racket/base

(require racket/contract
         racket/match
         "regex.rkt")

(provide
 (contract-out [deriv (regex? char? . -> . regex?)]))


; definition of brzozowski derivatives

(define (null-re? e)
  (if (is-null? e)
      (epsilon)
      (empty-set)))

(define (deriv e a)
  (match e
    [(empty-set)     (empty-set)]
    [(epsilon)       (empty-set)]
    [(chr c)         (if (char=? c a)
                         (epsilon)
                         (empty-set))]
    [(choice e1 e2)  (choice (deriv e1 a)
                             (deriv e2 a))]
    [(cat e1 e2)     (choice (cat (deriv e1 a) e2)
                             (cat (null-re? e)
                                  (deriv e2 a)))]
    [(star e1)       (cat (deriv e1 a)
                          (star e1))]))
