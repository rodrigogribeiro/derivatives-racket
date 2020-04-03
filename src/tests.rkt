#lang racket

(require quickcheck
         rackunit/quickcheck
         "brzozowski.rkt"
         "antimirov.rkt"
         "posix.rkt"
         "regex.rkt"
         "parsing.rkt"
         "tree.rkt")

; generating strings accepted

(define (gen-string-for e)

  (define (repeat n xs)
    (cond ((<= n 0) '())
          (else (append xs (repeat (- n 1) xs)))))

  (define (gen-char-list e)
    (match e
      [(epsilon) '()]
      [(chr c)   (list c)]
      [(cat e1 e2) (bind-generators ([s1 (gen-char-list e1)]
                                     [s2 (gen-char-list e2)])
                                    (append s1 s2))]
      [(choice e1 e2) (choose-one-of (list (gen-char-list e1)
                                           (gen-char-list e2)))]
      [(star e1) (bind-generators ([n (choose-integer 0 5)]
                                   [s (gen-char-list e1)])
                                  (repeat n s))]))

  (bind-generators ([xs (gen-char-list e)])
                   (cons e (list->string xs))))


; generator of regular expressions

(define (sized-regex n)
  (define gen-basics
    (generator-unit (epsilon)))

  (define gen-chr
    (bind-generators
     ([c choose-ascii-letter])
     (chr c)))

  (define (gen-choice n)
    (bind-generators
     ([e1 (sized-regex n)]
      [e2 (sized-regex n)])
     (choice e1 e2)))

  (define (gen-cat n)
    (bind-generators
     ([e1 (sized-regex n)]
      [e2 (sized-regex n)])
     (cat e1 e2)))

  (define (gen-star n)
    (bind-generators
     ([e1 (sized-regex n)])
     (star e1)))

  (cond ((<= n 0) gen-basics)
        (else (let ([n2 (quotient n 2)])
                (choose-with-frequencies
                   (list (cons 30 gen-chr)
                         (cons 20 (gen-choice n2))
                         (cons 30 (gen-cat n2))
                         (cons 20 (gen-star n2))))))))


; main regex generator


(define gen-regex
  (sized sized-regex))

(define gen-str
  (generator-bind
   gen-regex
   (lambda (e) (gen-string-for e))))


; testing the generators


(define regex-gen-correct
  (property ([e gen-regex])
            (regex? e)))

(define string-gen-correct
  (property ([s gen-str])
            (string? (cdr s))))

; testing the parser

(define parse-correct
  (property ([p gen-str])
            (parse (car p) (cdr p))))

(define lexer-correct
  (property ([p gen-str])
            (type-check (lexer (car p) (cdr p)) (car p))))

; testing stuff

(check-property regex-gen-correct)

(check-property string-gen-correct)

(check-property parse-correct)

(check-property lexer-correct)
