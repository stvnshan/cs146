;; m724li + s22shan
#lang racket

(define table (make-hash))

(define (primpl-assemble lst) (main lst 1))

(define (main lst loop)
  (cond
    [(= loop 1) (first-loop lst lst 0)]
    [(= loop 2) (second-loop lst lst)]
    [(= loop 3) (third-loop lst)]
    [else 0]
  )  
)

;; purpose: put (key, (memory, value)) into hashtable
;; spot the "duplicate" error
(define (first-loop lst lst2 indx)
  (cond
    [(empty? lst) (main lst2 2)]
    [else
      (match (first lst)
        [`(label ,x)
            (if (hash-has-key? table x)  
                (error "first-loop | label | duplicate") 
                (begin 
                    (hash-set! table x (list "label" indx empty)) 
                    (first-loop (rest lst) lst2 indx)))]  
        [`(const ,x ,y)
            (if (hash-has-key? table x)  
                (error "first-loop | label | duplicate") 
                (begin
                    (hash-set! table x (list "const" indx y)) 
                    (first-loop (rest lst) lst2 indx)))]
        ;; where (y z) is an array of y # of z
        [`(data ,x (,y ,z))
            (if (hash-has-key? table x)  
                (error "first-loop | (data x (y z)) | duplicate") 
                (begin 
                    (hash-set! table x (list "data" indx (get-list y z))) 
                    (first-loop (rest lst) lst2 (+ y indx))))] ;;I THINK THE INDEX HERE NEEDS TO BE INCREMENTED BY Y AND NOT ONE
                    ;;LOOK AT MEEEEE (╯°□°）╯︵ ┻━┻
        ;;where y is psymbol or literal
        [`(data ,x ,y)
            (if (hash-has-key? table x)  
                (error "first-loop | (data x y) | duplicate") 
                (begin 
                    (hash-set! table x (list "data" indx y)) 
                    (first-loop (rest lst) lst2 (+ 1 indx))))]
        ;; where ... is the array
        [`(data ,x ...)
            (if (hash-has-key? table x)  
                (error "first-loop | (data x ...) | duplicate") 
                (begin 
                    (hash-set! table (second (first lst)) (list "data" indx (rest (rest (first lst))))) 
                    (first-loop (rest lst) lst2 (+ (- (length (first lst)) 2) indx))))] ;;THIS ONE SHOULD ALSO BE INCRMENETD I BLIEVEE (╯°□°）╯︵ ┻━┻
        [x (first-loop (rest lst) lst2 (add1 indx))])]))

;; purpose: spot the "circular" for consts + "undefined" error
(define (second-loop lst lst2)
  (cond
    [(empty? lst) (main lst2 3)]
    [else
      (match (car lst)
        [`(const ,x ,y)
          (begin
            (find-source empty empty x)
            (second-loop (cdr lst) lst2))]
        [`(data ,x ,y)
          (begin 
            (find-source empty empty x)
            (second-loop (cdr lst) lst2))]
        [`(data ,x (,y ,z))
          (begin 
            (find-source empty empty x)
            (second-loop (cdr lst) lst2))]
        [`(data ,x ...)
          (begin 
            (loop-through-list empty empty (rest (rest (first lst))))
            (second-loop (cdr lst) lst2))]
        [`(add ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(sub ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(mul ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(div ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(mod ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]

       [`(gt ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(ge ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(lt ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(le ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(equal ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(not-equal ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(land ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(lor ,x ,y ,z)
         (begin
           (mama-halp x)
           (mama-halp y)
           (mama-halp z)
           (second-loop (cdr lst) lst2))]
        [`(lnot ,x ,y )
         (begin
           (mama-halp x)
           (mama-halp y)
           (second-loop (cdr lst) lst2))]
        [`(jump ,x)
         (begin
           (mama-halp x)
           (second-loop (cdr lst) lst2))]
        [`(branch ,x ,y)
         (begin
           (mama-halp x)
           (mama-halp y)
           (second-loop (cdr lst) lst2))]
        [`(move ,x ,y)
         (begin
           (mama-halp x)
           (mama-halp y)
           (second-loop (cdr lst) lst2))]
        [`(print-val ,x)
         (begin
           (mama-halp x)
           (second-loop (cdr lst) lst2))]

        [`(print-string ,x)
         (begin
           (mama-halp x)
           (second-loop (cdr lst) lst2))]
        [`(label ,x)
         (second-loop (cdr lst) lst2)]
        [`(lit ,x)
         (begin 
            (mama-halp x)
            (second-loop (cdr lst) lst2))]
        [x (begin 
            (mama-halp x)
            (second-loop (cdr lst) lst2))]
      )
    ]
  )
)

(define (mama-halp x)
  (cond
    [(symbol? x) (find-source empty empty x)]
    [(list? x) (daddy-halp x)]
    [else (void)]))

(define (daddy-halp x)
  (cond
    [(empty? x) void]
    [(symbol? (first x))
     (begin
       (find-source empty empty (first x))
       (daddy-halp (rest x)))]
    [(list? (first x))
     (begin
       (find-source empty empty (first (first x)))
       (daddy-halp (rest x)))]
    [else (daddy-halp (rest x))]))

;; helper function of helper function to spo "circular" + "undefined"
(define (find-source vis his key)
  (define stupid-error (hash-has-key? table key))
  (cond
    [(equal? #f stupid-error) (error "find-source undefined ~a" key)]
    [else
     (define stoopid (hash-ref table key))
     (define element (third stoopid))
     (define circular (member key vis)) ;;false if not exist list if does exist
     (cond
       [(list? circular) (error "find-source | ~a | circular" key)] ;;we've already seen this value
       [(number? element) (void)] ;;lit we're done
       [(boolean? element) (void)]
       [(list? element) (loop-through-list vis his element)] ;;list will evalulatae, impossible for loop so reset vis array (data X 1 2 B A C)
       [(string=? "data" (first (hash-ref table element))) empty]
       [(string=? "const" (first (hash-ref table element))) (find-source (cons key vis) his element)] ;;const mark true
       [else (find-source vis his element)] ;;psymbol always evaulates, impossible for loop so reset vis array
       )]
))

(define (loop-through-list vis his lst)
  (cond
    [(empty? lst) void]
    [(number? (first lst)) (loop-through-list vis his (rest lst))]
    [(boolean? (first lst)) (loop-through-list vis his (rest lst))]
    [else
      (begin
        (define exists (member (car lst) his))
        (if (list? exists) (loop-through-list vis his (cdr lst))
            (find-source vis (cons (car lst) his) (car lst))))]))

;; convert A-primpl into primpl
;; "incorrect" error
(define (third-loop lst)
  (cond
    [(empty? lst) empty]
    [else
      (match (first lst)
        [`(const ,x ,y)
          (third-loop (rest lst))]
        [`(halt)
          (cons 0 (third-loop (rest lst)))]
        [`(lit ,x)
          (cond
            [(number? x) (cons x (third-loop (rest lst)))]
            [(number? (get-root x)) (cons (get-root x) (third-loop (rest lst)))]
            [(empty? (get-root x)) (error "third-loop | incorrect")]            
            )]
        [`(label ,x)
          (third-loop (rest lst))]
        [`(data ,x (,y ,z))
          (cond
            [(number? z) (append (get-list y z) (third-loop (rest lst) ))]
            [(number? (get-root z)) (append (get-list y (get-root z )) (third-loop (rest lst) ))]
            [(empty? (get-root z )) (error "third-loop | incorrect")]
            )]
        [`(data ,x ,y)
          (cond
            [(number? y) (cons y (third-loop (rest lst)))]
            [(number? (get-root y)) (cons (get-root y) (third-loop (rest lst)))]
            [(empty? (get-root y)) (error "third-loop | incorrect")]
            )]
        
        [`(data ,x ...)
          (cond
            [else (append (get-correct-datali (rest (rest (first lst))) ) (third-loop (rest lst) ))])]

;; i am fucked
        [`(add ,x ,y ,z)
          (cons (list 'add (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(sub ,x ,y ,z)
          (cons (list 'sub (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(mul ,x ,y ,z)
          (cons (list 'mul (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]
        
        [`(div ,x ,y ,z)
          (cons (list 'div (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(mod ,x ,y ,z)
          (cons (list 'mod (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(gt ,x ,y ,z)
          (cons (list 'gt (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(ge ,x ,y ,z)
          (cons (list 'ge (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(lt ,x ,y ,z)
          (cons (list 'lt (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(le ,x ,y ,z)
          (cons (list 'le (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]
        
        [`(equal ,x ,y ,z)
          (cons (list 'equal (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]
        
        [`(not-equal ,x ,y ,z)
          (cons (list 'not-equal (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(land ,x ,y ,z)
          (cons (list 'land (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(lor ,x ,y ,z)
          (cons (list 'lor (get-dest x) (get-opd y) (get-opd z)) (third-loop (rest lst) ))]

        [`(lnot ,x ,y)
          (cons (list 'lnot (get-dest x) (get-opd y)) (third-loop (rest lst) ))]

        [`(jump ,x)
          (cons (list 'jump (get-jump-odp x)) (third-loop (rest lst) ))]

        [`(branch ,x ,y)
          (cons (list 'branch (get-opd x) (get-jump-odp y)) (third-loop (rest lst) ))]
        
        [`(move ,x ,y)
          (cons (list 'move (get-dest x) (get-opd y)) (third-loop (rest lst) ))]

        [`(print-val ,x)
          (cons (list 'print-val (get-opd x)) (third-loop (rest lst) ))]

        [`(print-string ,x)
          (cons (list 'print-string x) (third-loop (rest lst) ))]
        
        [x
          (cond
            [(number? x) (cons x (third-loop (rest lst)))]
            [(boolean? x) (cons x (third-loop (rest lst)))]
            [(string=? (first (hash-ref table x )) "data") (cons (second (hash-ref table x )) (third-loop (rest lst)))]
            [(string=? (first (hash-ref table x )) "const") (cons (get-root x) (third-loop (rest lst)))]
            [else (error "third-loop | ~a | incorrect" x)])])])) 

;; (add A B C) this validates the A thing, throws "incorrect" error if something is used incorrectly
(define (get-dest x)
  (match x
    [`(,a) (list a)] ;; (11)
    [`(,a (,b)) 
      (cond
        [(number? a) (list a (list b))]
        [(equal? (first (hash-ref table a )) "const") (list (get-root a) (list b))]
        [else (error "get-dest | ~a | incorrect" x)])] ;; (1 (5))
    [`(,a ,b)
      (cond
        [(and (number? a) (string=? (first (hash-ref table b )) "data")) (list a (list (second (hash-ref table b ))))]
        [(and (string=? (first (hash-ref table a )) "const") (string=? (first (hash-ref table b )) "data")) (list (get-root a) (list (second (hash-ref table b ))))]
        [else (error "get-dest | ~a | incorrect" x)])]
    [a 
      (cond
        [(string=? (first (hash-ref table a )) "data") (list (second (hash-ref table a )))]
        [else (error "get-dest | ~a | incorrect" x)])]))
    ; [a (error "get-dest | ~a | incorrect" x)]))

;; solves the (addd a B C) B C thing
(define (get-opd x)
  (match x
    [`(,a) (list a)]
    [`(,a (,b))
      (cond
        [(number? a) (list a (list b))]
        [(string=? (first (hash-ref table a )) "const") (list (get-root a) (list b))]
        [(string=? (first (hash-ref table a )) "data") (list (second (hash-ref table a)) (list b))]
        [else (error "get-opd | ~a | incorrect" x)])]
    [`(,a ,b)
      (cond
        [(and (number? a) (equal? (first (hash-ref table b )) "data")) (list a (list (second (hash-ref table b ))))]
        [(and (equal? (first (hash-ref table a )) "const") (equal? (first (hash-ref table b )) "data")) (list (get-root a) (list (second (hash-ref table b ))))]
        [(and (string=? (first (hash-ref table a )) "data") (string=? (first (hash-ref table b )) "data")) (list (second (hash-ref table a )) (list (second (hash-ref table b ))))]
        [else (error "get-opd | ~a | incorrect" (hash-ref table b ))])]
    [a ;;might need to change
      (cond
        [(number? a) a]
        [(boolean? a) a]
        [(equal? (first (hash-ref table a )) "const") (get-root a)]
        [(equal? (first (hash-ref table a )) "data") (list (second (hash-ref table a )))]
        [else (error "get-opd | ~a | incorrect" x)])]))
    ; [a (error "get-opd | ~a | incorrect" x)]))

(define (get-jump-odp x) 
  (match x
    [a
      (cond
        [(number? a) a]
        [(equal? (first (hash-ref table a )) "const") (list (get-root a))]
        [(equal? (first (hash-ref table a )) "label") (second (hash-ref table a ))]
        [(equal? (first (hash-ref table a )) "data") (list (second (hash-ref table a )))]
        [else (error "get-jump-opd | ~a | incorrect" (hash-ref table x ))])]))


(define (get-correct-datali list ) 
  (cond
    [(empty? list) empty]
    [(number? (first list)) (cons (first list) (get-correct-datali (rest list)))]
    [(boolean? (first list)) (cons (first list) (get-correct-datali (rest list)))]
    [(equal? (first (hash-ref table (first list))) "const" ) (cons (get-root (first list)) (get-correct-datali (rest list)))]
    [(equal? (first (hash-ref table (first list))) "data" ) (cons (second (hash-ref table (first list) )) (get-correct-datali (rest list)))]
    [else (error "get-correct-datali | ~a | incorrect" list)]))
  

(define (get-root a)
  (cond
    [(number? a) a]
    [(boolean? a) a]
    [(string=? (first (hash-ref table a )) "const") (get-root (third (hash-ref table a )))] ;;keep traversing
    [(string=? (first (hash-ref table a )) "data") (second (hash-ref table a ))]
    [(string=? (first (hash-ref table a )) "label") (second (hash-ref table a ))]
    [else (error "get-oot | ~a | incorrect" a)]))




;; "duplicate" error on an attempt to define a duplicate psymbol
;; "circular" error if the code contains a chain of circular references
;; "undefined" error if a psymbol is used but not defined
;; "incorrect" error if a psymbol is used incorrectly



  
;;
(define (get-list x y)
  (cond
    [(zero? x) empty]
    [else (cons y (get-list (sub1 x) y))]))


; (primpl-assemble
; '((label LOOP_TOP)
; (lt TMP1 _i 5)
; (branch TMP1 LOOP_CONT)
; (jump LOOP_DONE)
; (label LOOP_CONT)
; (add _sum _sum (_A _i))
; (add _i _i 1)
; (jump LOOP_TOP)
; (label LOOP_DONE)
; (print-val _sum)
; (halt)
; (data _sum 0)
; (data _i 0)
; (data TMP1 0)
; (data _A 1 2 3 4 5))
; )
; (primpl-assemble '((label L1)(const c d)(lit L1)(const d 3)(data x 2) (data y x)(data z (3 x))(data k 8 9 2)(add (3 y) (y (2)) (x (10)))))
; (primpl-assemble '((add b a a)(data a (3 5))(data b 1 2 3 4)(label x)))
; (primpl-assemble '((const x 10)(data y (3 x))))
; (primpl-assemble '((add b a a)(data a (3 5))(data b 1 2 3 4)(label x)))
; (primpl-assemble '((const a 5) (print-val (0 (a)))))
;(primpl-assemble '((data x 1) x))