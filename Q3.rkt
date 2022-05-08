#lang racket



(define (operand? x)
  (cond
    [(list? x) #f]
    [(not (symbol? x)) #t]
    [(and (not (symbol=? '+ x)) (not (symbol=? '* x))) #t]
    [else #f]
    ))




(define (make-simple clear-exp exp)
  (cond[(empty? exp) clear-exp]
       [(operand? exp) exp]
       [(and (or (operand? (first exp)) (list? (first exp))) (not (empty? (rest exp))) (symbol=? '* (second exp))) (make-simple (append clear-exp (list (first (mul-stop empty exp)))) (second (mul-stop empty exp)))]

       
       
       [(and (list? (first exp)) (empty? (rest (first exp)))) (make-simple clear-exp (append (first exp) (rest exp)))]


       [(list? (first exp)) (make-simple (append clear-exp (list (make-simple empty (first exp)))) (rest exp))]


       [(or (operand? (first exp)) (and (symbol? (first exp)) (symbol=? '+ (first exp)))) (make-simple (append clear-exp (list (first exp))) (rest exp))]
       ))

(define (mul-stop new-exp exp)
  (cond[(empty? exp) (list new-exp empty)]
       [(operand? (first exp)) (mul-stop (append new-exp (list (first exp))) (rest exp))]
       [(list? (first exp)) (mul-stop (append new-exp (list (make-simple empty (first exp)))) (rest exp))]
       [(symbol=? '+ (first exp)) (list new-exp exp)]
       [(symbol=? '* (first exp)) (mul-stop (append new-exp (list '*)) (rest exp))]
       
       ))


(define (get-op exp)
  (cond [(empty? exp) empty]
        [(and (symbol? (first exp)) (or (symbol=? '+ (first exp)) (symbol=? '* (first exp)))) (first exp)]
        [else (get-op (rest exp))]))
(define (get-ope ope-list exp)
  (cond[(empty? exp) ope-list]
       [(list? (first exp)) (get-ope (append ope-list (list (ok (first exp)))) (rest exp))]
       [(operand? (first exp)) (get-ope (append ope-list (list (first exp))) (rest exp))]
       [else (get-ope ope-list (rest exp))])) 

(define (ok exp)
  (append (list (get-op exp)) (get-ope empty exp)))

(define (in->pre exp)
  (cond[(operand? exp) exp]
       [(empty? exp) (error "bad expression")]
       [(and (or (list? (first exp)) (operand? (first exp))) (valid? exp)) (d2 empty (d1 (delete-nested empty (ok (make-simple empty exp)))))]
       [else (error "bad expression")]))

(define (d2 expd exp)
  (cond[(empty? exp) expd]
       [(list? (first exp)) (d2 (append expd (list (d1 (first exp)))) (rest exp))]
       [else (d2 (append expd (list (first exp))) (rest exp))]))

(define (d1 exp)
  (cond[(not (list? exp)) exp]
       [(and (list? (first exp)) (empty? (rest exp))) (d1 (first exp))]
       [else exp]))

(define (delete-nested exp-without-nested exp)
  (cond[(empty? exp) exp-without-nested]
       [(and (list? (first exp)) (empty? (first exp))) (delete-nested exp-without-nested (rest exp))]
       [(list? (first exp)) (delete-nested (append exp-without-nested (list (delete-nested empty (first exp)))) (rest exp))]
       [else (delete-nested (append exp-without-nested (list (first exp))) (rest exp))]))





(define (valid? exp)
  (cond[(empty? exp) #t]
       [(and (operand? (first exp)) (empty? (rest exp))) #t]
       [(and (list? (first exp)) (or (list? (first (first exp))) (operand? (first (first exp)))) (not (empty? (rest exp))) (not (operand? (second exp))) (not (list? (second exp)))) (and (valid? (first exp)) (valid? (rest exp)))]
       [(and (list? (first exp)) (or (list? (first (first exp))) (operand? (first (first exp)))) (empty? (rest exp))) (valid? (first exp))]
       [(and (not (list? (first exp))) (operand? (first exp)) (not (empty? (rest exp))) (not (operand? (second exp))) (not (list? (second exp)))) (valid? (rest exp))]
       [(and (not (operand? (first exp))) (not (list? (first exp))) (not (empty? (rest exp))) (or (operand? (second exp)) (list? (second exp)))) (valid? (rest exp))]
       [else #f]))
