#lang racket/base

(require racket/contract
         racket/match)


(provide
 ; structs
 (struct-out empty-set)
 (struct-out epsilon)
 (struct-out chr)
 (struct-out choice)
 (struct-out cat)
 (struct-out star)
 ; functions without contract
 regex?
 ; contracts 
 (contract-out [is-null? (regex? . -> . boolean?)]))


; definition of regular expression

(struct empty-set () #:transparent)
(struct epsilon () #:transparent)
(struct chr (sym) #:transparent)
(struct choice (left right) #:transparent)
(struct cat (left right) #:transparent)
(struct star (re) #:transparent)

; predicate for regex

(define (regex? e)
  (match e
    [(empty-set)       #t]
    [(epsilon)         #t]
    [(chr c)           #t]
    [(choice e1 e2)  (and (regex? e1)
                          (regex? e2))]
    [(cat e1 e2)     (and (regex? e1)
                          (regex? e2))]
    [(star e1)       (regex? e1)]
    [_               #f]))

; nullability test

(define (is-null? e)
  (match e
    [(empty-set)      #f]
    [(epsilon)        #t]
    [(chr c)         #f]
    [(choice e1 e2)  (or (is-null? e1)
                         (is-null? e2))]
    [(cat e1 e2)     (and (is-null? e1)
                          (is-null? e2))]
    [(star e1)       #t]
    [_               #f]))
