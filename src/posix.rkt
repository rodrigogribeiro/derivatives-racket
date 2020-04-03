#lang racket/base

(require racket/contract
         racket/match
         "regex.rkt"
         "tree.rkt"
         "brzozowski.rkt")

(provide
 (contract-out [lexer (regex? string? . -> . tree-re?)]))


(define (mk-eps e)
  (match e
    [(star e1)      (tree-list '())]
    [(cat e1 e2)    (cons (mk-eps e1) (mk-eps e2))]
    [(choice e1 e2) (cond ((is-null? e1) (tree-inl (mk-eps e1)))
                          ((is-null? e2) (tree-inr (mk-eps e2)))
                          (else error "Impossible contract violation!"))]
    [(epsilon)      (tree-unit)]
    [_              (error "Contract violation! mk-eps")]))

(define (inj e c t)
  (match (cons e t)
    [(cons (star e1) (tree-pair t ts))  (tree-list (cons (inj e1 c t) ts))]
    [(cons (cat e1 e2) (tree-pair t1 t2)) (tree-pair (inj e1 c t1) t2)]
    [(cons (cat e1 e2) (tree-inl (tree-pair tl1 tl2)))  (tree-pair (inj e1 c tl1)
                                                                   tl2)]
    [(cons (cat e1 e2) (tree-inr tr1)) (tree-pair (mk-eps e1) (inj e2 c tr1))]
    [(cons (choice e1 e2) (tree-inl tl)) (tree-inl (inj e1 c tl))]
    [(cons (choice e1 e2) (tree-inr tr)) (tree-inr (inj e2 c tr))]
    [(cons (chr c1) (tree-unit)) (tree-chr c)]
   ))


(define (lexer e s)
  (define (lexer1 e cs)
    (match cs
      ['() (mk-eps e)]
      [(cons c cs1)
       (let ([e1 (deriv e c)])
         (inj e c (lexer1 e1 cs1)))]))
  (lexer1 e (string->list s)))
