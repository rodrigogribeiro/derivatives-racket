#lang racket/base

(require racket/contract
         racket/match
         "regex.rkt")

(provide
 ; structs
 tree-unit
 tree-chr
 tree-inl
 tree-inr
 tree-pair
 tree-list
 ; predicate
 tree-re?
 ; type check
 (contract-out
  [type-check (tree-re? regex? . -> . boolean?)]))

; structure of trees

(struct tree-unit ()         #:transparent)
(struct tree-chr (sym)       #:transparent)
(struct tree-inl (left)      #:transparent)
(struct tree-inr (right)     #:transparent)
(struct tree-pair (fst snd)  #:transparent)
(struct tree-list (trees)    #:transparent)

; tree predicate 

(define (tree-re? tree)
  (match tree
    [(tree-unit) #t]
    [(tree-chr _) #t]
    [(tree-inl tl) (tree-re? tl)]
    [(tree-inr tr) (tree-re? tr)]
    [(tree-pair tl tr) (and (tree-re? tl)
                            (tree-re? tr))]
    [(tree-list ts) (andmap tree-re? ts)]
    [_ (error "Expecting a regex parsing tree!")]))

; type checking trees

(define (type-check tree e)
  (match (cons tree e)
    [(cons (tree-unit) (epsilon)) #t]
    [(cons (tree-chr c)  (chr c1)) (char=? c c1)]
    [(cons (tree-inl tl) (choice e _)) (type-check tl e)]
    [(cons (tree-inr tr) (choice _ e1)) (type-check tr e1)]
    [(cons (tree-pair tl tr) (cat e e1)) (and (type-check tl e)
                                              (type-check tr e1))]
    [(cons (tree-list ts) (star e)) (andmap (lambda (t) (type-check t e)) ts)]))
