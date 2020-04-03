#lang racket/base

(require racket/contract
         racket/list
         "regex.rkt"
         "brzozowski.rkt")

(provide
 (contract-out [parse (regex? string? . -> . boolean?)]))

; simple parsing using brzozowski derivatives

(define (parse e s)
  (define (parse-list e xs)
    (cond ((null? xs) (is-null? e))
          (else (parse-list (deriv e (car xs)) (cdr xs)))))
  (parse-list e (string->list s)))
