  #lang racket

;; s22shan && j2322che

(define lst-of-aprimpl (box empty))
(define lst-of-vars (box empty))
(define lst-of-fn-vars (box empty))
(define lst-of-fn-name (box empty))












(define (add-expr   new-expr)
  (set-box! lst-of-aprimpl (append (unbox lst-of-aprimpl) (list new-expr))))


(define (add-vars new-var val)
  (set-box! lst-of-vars (cons (list 'data new-var val)
                              (unbox lst-of-vars))))

;; handles vars
(define (handle-vars data-lst)
  (cond [(empty? data-lst) void]
        [else (begin (add-vars (first (first data-lst)) (second (first data-lst)))
                     (handle-vars (rest data-lst)))]))






;; handles aexp
(define (handle-aexp aexp f)
  (cond [(number? aexp)
         (begin
           (add-expr (list 'add 'sp 'sp 1))
          
           (add-expr (list 'move (list -1 'sp) aexp)))]
        [(symbol? aexp)
         (begin
           (add-expr (list 'add 'sp 'sp 1))
           (add-expr (list 'move (list -1 'sp) (get-pos aexp f))))]
        [(equal? (first aexp) '+)
         (begin
           (handle-aexp (second aexp) f)
           (handle-aexp (third aexp) f)
           (add-expr (list 'add (list -2 'sp)  (list -1 'sp) (list -2 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first aexp) '-)
         (begin
           (handle-aexp (second aexp) f)
           (handle-aexp (third aexp) f)
           (add-expr (list 'sub (list -2 'sp)  (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first aexp) '*)
         (begin
           (handle-aexp (second aexp) f)
           (handle-aexp (third aexp) f)
           (add-expr (list 'mul (list -2 'sp)  (list -1 'sp) (list -2 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first aexp) 'div)
         (begin
           (handle-aexp (second aexp) f)
           (handle-aexp (third aexp) f)
           (add-expr (list 'div (list -2 'sp)  (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first aexp) 'mod)
         (begin
           (handle-aexp (second aexp) f)
           (handle-aexp (third aexp) f)
           (add-expr (list 'mod (list -2 'sp)  (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [else
         (begin
           (check-argument aexp)
           ;(add-expr (list 'sub 'fp (list 0 'fp) (length (get-fn f (unbox lst-of-fn-vars)))))
           (set-p (rest aexp) f)
           (add-expr (list 'add 'fp 'fp (length (get-fn f (unbox lst-of-fn-vars)))))
           (set-v (first aexp))
           (add-expr (list 'jsr 'RETURN_ADDR (first aexp)))
           
           
           (add-expr (list 'sub 'fp 'fp (length (get-fn f (unbox lst-of-fn-vars))))))]))


(define (set-p params f)
  (cond[(empty? params) void]
       [else
        (begin
          (handle-aexp (first params) f)
          ;(add-expr (list 'move (list 0 'sp) (first params)))
          ;(add-expr (list 'add 'sp 'sp 1))
          (set-p (rest params) f))]))



(define (set-v fname)
  
  (set-v-help (get-v (get-fn fname (unbox lst-of-fn-vars)))))

(define (set-v-help lst)
  (cond
    
    [(empty? lst) void]
    [else
     (begin
       (add-expr (list 'move (list 0 'sp)  (second (second (first lst))) ))
       (add-expr (list 'add 'sp 'sp 1))
       (set-v-help (rest lst)))]))


(define (get-fn fname lst)
  (cond
    [(empty? lst) false]
    [(equal? fname (first (first lst))) (second (first lst))]
    [else (get-fn fname (rest lst))]))

(define (get-v f-lst)
  (cond
    [(empty? f-lst) empty]
    [(list? (second (first f-lst))) f-lst]
    [else (get-v (rest f-lst))]))




;; handles bexp
(define (handle-bexp bexp f)
  (cond [(or (boolean? bexp) (equal? bexp 'true) (equal? bexp 'false))
         (begin
           (add-expr (list 'add 'sp 'sp 1))
           (add-expr (list 'move (list -1 'sp) bexp)))]
        [(symbol? bexp)
         (begin
           (add-expr (list 'add 'sp 'sp 1))
           (add-expr (list 'move (list -1 'sp) (get-pos bexp f))))]       
        [(equal? (first bexp) '=)
         (begin
           (handle-aexp (second bexp) f)
           (handle-aexp (third bexp) f)
           (add-expr (list 'equal (list -2 'sp) (list -1 'sp) (list -2 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first bexp) '>)
         (begin
           (handle-aexp (second bexp) f)
           (handle-aexp (third bexp) f)
           (add-expr (list 'gt (list -2 'sp) (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first bexp) '<)
         (begin
           (handle-aexp (second bexp) f)
           (handle-aexp (third bexp) f)
           (add-expr (list 'lt (list -2 'sp) (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first bexp) '>=)
         (begin
           (handle-aexp (second bexp) f)
           (handle-aexp (third bexp) f)
           (add-expr (list 'ge (list -2 'sp) (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first bexp) '<=)
         (begin
           (handle-aexp (second bexp) f)
           (handle-aexp (third bexp) f)
           (add-expr (list 'le (list -2 'sp) (list -2 'sp) (list -1 'sp)))
           (add-expr (list 'sub 'sp 'sp 1)))]
        [(equal? (first bexp) 'not)
         (begin
           (handle-bexp (second bexp) f)
           (add-expr (list 'lnot (list -1 'sp) (list -1 'sp))))]
        [(equal? (first bexp) 'and)
         (begin
           (if (= (length bexp) 3)
               (begin
                 (handle-bexp (second bexp) f)
                 (handle-bexp (third bexp) f)
                 (add-expr (list 'land (list -2 'sp) (list -2 'sp) (list -1 'sp)))
                 (add-expr (list 'sub 'sp 'sp 1)))
               (begin
                 (handle-bexp (second bexp) f)
                 (handle-bexp (cons 'and (rest (rest bexp))) f)
                 (add-expr (list 'land (list -2 'sp) (list -2 'sp) (list -1 'sp)))
                 (add-expr (list 'sub 'sp 'sp 1)))))]
        [(equal? (first bexp) 'or)
         (begin
           (if (= (length bexp) 3)
               (begin
                 (handle-bexp (second bexp) f)
                 (handle-bexp (third bexp) f)
                 (add-expr (list 'lor (list -2 'sp) (list -2 'sp) (list -1 'sp)))
                 (add-expr (list 'sub 'sp 'sp 1)))
               (begin
                 (handle-bexp (second bexp) f)
                 (handle-bexp (cons 'or (rest (rest bexp))) f)
                 (add-expr (list 'lor (list -2 'sp) (list -2 'sp) (list -1 'sp)))
                 (add-expr (list 'sub 'sp 'sp 1)))))]))


                


(define (handle-stmt stmt f )
  (match stmt

    [(list 'print exp)
     (if (string? exp)
         (add-expr (list 'print-string exp))
         (begin (handle-aexp exp f)
                (add-expr (list 'print-val (list -1 'sp)))
                (add-expr (list 'sub 'sp 'sp 1))))]
    
    [(list 'set var aexp)
     (handle-aexp aexp f)
     (add-expr (list 'move (get-pos var f) (list -1 'sp)))
     (add-expr (list 'sub 'sp 'sp 1))]
    
    [(list 'seq stmt1 ...)
     (if (= (length stmt) 2)
         (handle-stmt (second stmt) f )
         (begin (handle-stmt (second stmt) f )
                (handle-stmt (cons 'seq (rest (rest stmt))) f )))]
    
    [(list 'iif bexp stmt1 stmt2)
     (define iif_label0 (gensym 'iif_label0))
     (define iif_label1 (gensym 'iif_label1))
     (define iif_label2 (gensym 'iif_label2))
     (begin
       (handle-bexp bexp f)
       (add-expr (list 'branch (list -1 'sp) iif_label0))
       (add-expr (list 'sub 'sp 'sp 1))
       (add-expr (list 'jump iif_label1))
       (add-expr (list 'label iif_label0))
       (handle-stmt stmt1 f)
       (add-expr (list 'jump iif_label2))
       (add-expr (list 'label iif_label1))
       (handle-stmt stmt2 f)
       (add-expr (list 'label iif_label2)))]
    
    [(list 'skip) void]

    [(list 'while bexp stmt1 ...)
     (define while_label0 (gensym 'while_label0))
     (define while_label1 (gensym 'while_label1))
     (define while_label2 (gensym 'while_label2))
     (begin
       (add-expr (list 'label while_label0))
       (handle-bexp bexp f)
       (add-expr (list 'branch (list -1 'sp) while_label1))
       (add-expr (list 'sub 'sp 'sp 1))
       (add-expr (list 'jump while_label2))
       (add-expr (list 'label while_label1))
       (handle-stmt (cons 'seq (rest (rest stmt))) f)
       (add-expr (list 'jump while_label0))
       (add-expr (list 'label while_label2)))
     ]
    

    [(list 'return aexp)
     (begin
               (handle-aexp aexp f)
               (add-expr (list 'move (list  (length (get-fn f (unbox lst-of-fn-vars))) 'fp) (list -1 'sp)))
               
               (add-expr (list 'add 'sp  (+ 1 (length (get-fn f (unbox lst-of-fn-vars))))  'fp))
               
               )

     
     
     ]
    [x void]
    ))       
           

(define (get-main lst)
  (cond
    [(empty? lst) empty]
    [(equal? 'main (first (second (first lst)))) (third (first lst))]
    [else (get-main (rest lst))]))



(define (compile-simpl-h m-lst lst)
   (cond[(empty? m-lst)
          (append (list (list 'jump 'main))
                  (unbox lst-of-aprimpl)
                  (list (list 'halt))
                  (list (list 'data 'fp (+ 5 (length (unbox lst-of-aprimpl))))
                        (list 'data 'sp (+ 5 (length (unbox lst-of-aprimpl))))
                        (list 'data 'RETURN_ADDR 0)))]
        [else (begin
                (handle-stmt (first m-lst) 'main)
                (compile-simpl-h (rest m-lst) lst))]))
     


(define (compile-simpl lst)
  (cond[(empty? (get-main lst)) empty]
       [else (begin
               (check-duplicate-halp lst)
  (check-return-halp lst)
   (traverse-lst lst)
   (compile-f lst)
 
   (add-expr (list 'label 'main))
   
   (set-v-main-help (second (get-main lst)))
   (compile-simpl-h (get-main lst) lst)
   )]))
  


(define (set-v-main-help lst)
  (cond[(empty? lst) void]
        [else
         (begin
           (add-expr (list 'move (list 0 'sp) (second (first lst))))
           (add-expr (list 'add 'sp 'sp 1))
           (set-v-main-help (rest lst)))]))






(define (check-duplicate-halp lst)
  (cond[(empty? lst) void]
       [else
        (begin
          (check-duplicate (first lst))
          (check-duplicate-halp (rest lst)))]))

(define (check-return-halp lst)
  (cond[(empty? lst) void]
       [else
        (begin
          (check-return (first lst))
          (check-return-halp (rest lst)))]))



(define (compile-f lst)
  (cond
    [(empty? lst) void]
    [(equal? 'main (first (second (first lst)))) (compile-f (rest lst))]
    [else
     (begin
       (add-expr (list 'label (first (second (first lst)))))
       (handle-f-stmts (rest (rest (first (rest (rest (first lst)))))) (first (second (first lst))))
       (add-expr (list 'jump 'RETURN_ADDR))
       (compile-f (rest lst)))
     ]))

(define (handle-f-stmts lst f)
  (cond[(empty? lst) void]
       [else
        (begin
          (handle-stmt (first lst) f)
          (handle-f-stmts (rest lst) f))]))




(define (traverse-lst lof)
  (cond [(empty? lof) void]
        [(equal? (first (second (first lof))) 'main)
         (begin (set-box! lst-of-fn-vars
                          (cons (list (first (second (first lof)))
                                (traverse-fn (second (third (first lof))) 0))
                                (unbox lst-of-fn-vars)))
                (traverse-lst (rest lof)))]
        [else
         (begin (set-box! lst-of-fn-vars
                          (cons (list (first (second (first lof)))
                                (traverse-fn (append (rest (second (first lof))) (second (third (first lof)))) 0))
                                (unbox lst-of-fn-vars)))
                (traverse-lst (rest lof)))]))

(define (traverse-fn params counter)
  (cond [(empty? params) empty]
        [(list? (first params))
         (cons (list (first (first params)) (list counter (second (first params))))
               (traverse-fn (rest params) (add1 counter)))]
        [else (cons (list (first params) counter)
                    (traverse-fn (rest params) (add1 counter)))]))


(define (get-pos var f)
  (define (get-pos-num var lst-of-vars)
    (cond [(empty? lst-of-vars) 0]
          [(equal? var (first (first lst-of-vars)))
           (if (list? (second (first lst-of-vars)))
               (first (second (first lst-of-vars)))
               (second (first lst-of-vars)))]
          [else (get-pos-num var (rest lst-of-vars))]))
  (define (get-pos-h var f lst-of-fn-vars)
    (cond [(empty? lst-of-fn-vars) false]
          [(equal? f (first (first lst-of-fn-vars)))
           (list (get-pos-num var (second (first lst-of-fn-vars))) 'fp)]
          [else (get-pos-h var f (rest lst-of-fn-vars))]))
  (get-pos-h var f (unbox lst-of-fn-vars)))


(define (check-duplicate fn) ; check duplicates for a function
  (define params (rest (second fn)))
  (define local-vars (second (third fn)))
  (define (check-duplicate-h1 local-vars) ; check duplicates for params vs local-vars
    (cond [(empty? local-vars) void]
          [(list? (member (first (first local-vars)) params))
           (error "duplicate")]
          [else (check-duplicate-h1 (rest local-vars))]))
  (define (check-duplicate-h2 params acc) ; check duplicates for params
    (cond [(empty? params) void]
          [(list? (member (first params) acc))
           (error "duplicate")]
          [else (check-duplicate-h2 (rest params) (cons (first params) acc))]))
  (define (check-duplicate-h3 local-vars acc) ; check duplicates for local-vars
    (cond [(empty? local-vars) void]
          [(list? (member (first (first local-vars)) acc))
           (error "duplicate")]
          [else (check-duplicate-h3 (rest local-vars) (cons (first (first local-vars)) acc))]))
  (begin (check-duplicate-h1 local-vars)
         (check-duplicate-h2 params empty)
         (check-duplicate-h3 local-vars empty)))


(define (check-return fn)
  (define fn-stmts (third fn))
  (define (check-return-h fn-stmts)
    (cond [(empty? fn-stmts) void]
          [(and (list? (first fn-stmts))
                (empty? (rest fn-stmts))
                (not (list? (member 'return (first fn-stmts)))))
           (error "return")]
          [else (check-return-h (rest fn-stmts))]))
  (check-return-h fn-stmts))



(define (check-argument fn-call)
  (define fn-call-length (length (rest fn-call)))
  (define (check-argument-h fn-call lst-of-fn-vars)
    (cond [(empty? lst-of-fn-vars) false]
          [(and (equal? (first fn-call) (first (first lst-of-fn-vars)))
           (not (= fn-call-length (check-argument-help (second (first lst-of-fn-vars)) 0))))
           (error "argument")]
          [else (check-argument-h fn-call (rest lst-of-fn-vars))]))
   (define (check-argument-help fn-vars count)
     (cond [(or (empty? fn-vars) (list? (second (first fn-vars)))) count]
           [else (check-argument-help (rest fn-vars) (add1 count))])) 
  (check-argument-h fn-call (unbox lst-of-fn-vars)))



        