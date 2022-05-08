#lang racket

(define (nat-to-blist nat)
  (local[
         (define (nat-blist blist nat)
           (cond
             [(= 0 nat) blist]
             [else (nat-blist (append blist (list (modulo nat 10000))) (floor (/ nat 10000)))]))]
  (nat-blist empty nat))
  )


(define (blist-to-nat blist)
  (local[
         (define (blist-nat blist nat power)
         (cond
           [(empty? blist) nat]
           [else (blist-nat (rest blist) (+ nat (* (expt 10000 power) (first blist))) (add1 power))]))]
    (blist-nat blist 0 0)))

(define (add blist1 blist2)
  (add-help blist1 blist2 0))

(define (add-help blist1 blist2 n)
  (cond
    [(and (empty? blist1) (empty? blist2) (= n 0)) empty]
    [(and (empty? blist1) (empty? blist2)) (cons n empty)]
    [(empty? blist1) (cons (modulo (+ (first blist2) n) 10000) (add-help empty (rest blist2) (/ (- (+ (first blist2) n) (modulo (+ (first blist2) n) 10000)) 10000)))]
    [(empty? blist2) (cons (modulo (+ (first blist1) n) 10000) (add-help (rest blist1) empty (/ (- (+ (first blist1) n) (modulo (+ (first blist1) n) 10000)) 10000)))]
    [else (cons (modulo (+ (first blist1) (first blist2) n) 10000) (add-help (rest blist1) (rest blist2) (/ (- (+ (first blist1) (first blist2) n) (modulo (+ (first blist1) (first blist2) n) 10000)) 10000)))]
    ))
 
(define (mult blist1 blist2)
  (mult-blist2 blist1 blist2 0))

(define (mult-blist2 blist1 blist2 n)
  (cond
    [(empty? blist2) empty]
    [else (add (mult-blist2 blist1 (rest blist2) (add1 n)) (mult-to-blist1 blist1 (first blist2) n 0))]))


(define (mult-to-blist1 blist1 coe n m)
  (cond
    [(and (empty? blist1) (= 0 m)) empty]
    [(empty? blist1) (cons (modulo m 10000) (mult-to-blist1 empty coe n (/ (- m (modulo m 10000)) 10000)))]
    [else (cons (modulo (+ (* (first blist1) coe (expt 10000 n)) m) 10000) (mult-to-blist1 (rest blist1) coe n (/ (- (+ (* (first blist1) coe (expt 10000 n)) m) (modulo (+ (* (first blist1) coe (expt 10000 n)) m) 10000)) 10000)))]))




