#lang racket/base

(require racket/contract
         racket/match
         racket/set
         "regex.rkt")

(provide
 (contract-out [pderiv (regex? char? . -> . regex?)]))


(define set-empty
  (list->set '()))

(define (singleton x)
  (set-add x set-empty))

; cat a set

(define (cat-list es e)
  (set-map (lambda (e1) (cat e1 e)) es))

; definition of antimirov derivatives

(define (pderiv e a)
  (match e
    [(empty-set) set-empty]
    [(epsilon)   set-empty]
    [(chr c)     (if (char=? (chr-sym e) a)
                     (singleton epsilon)
                     (set-empty))]
    [(choice e1 e2) (set-union (pderiv (choice-left e) a)
                               (pderiv (choice-right e) a))]
    [(cat e1 e2)    (set-union (cat-list (pderiv (cat-left e) a)
                                         (cat-right e))
                               (if (is-null? e)
                                   (pderiv (pderiv (cat-right e) a))
                                   (set-empty)))]
    [(star e1)      (cat-list (pderiv e a) (star (star-re e)))]))
