#lang racket


(require test-engine/racket-tests)

(struct bin (op fst snd) #:transparent) ; op is a symbol; fst, snd are ASTs.

(struct fun (param body) #:transparent) ; param is a symbol; body is an AST.

(struct app (fn arg) #:transparent) ; fn and arg are ASTs.

(struct seq (fst snd) #:transparent) ; 

(struct set (var newval) #:transparent)

(struct result (val newstore) #:transparent)



;; An AST is a (union bin fun app).

(struct sub (name val) #:transparent)
(struct subSt (loc val) #:transparent)

;; A substitution is a (sub n v), where n is a symbol and v is a value.
;; An environment (env) is a list of substitutions.

(struct closure (var body envt store) #:transparent)

;; A closure is a (closure v bdy env), where
;; v is a symbol, bdy is an AST, and env is a environment.
;; A value is a (union number closure).

;; parse: sexp -> AST

(define (parse sx)
  (match sx
    [`(with ((,nm ,nmd)) ,bdy) (app (fun nm (parse bdy)) (parse nmd))]
    [`(+ ,x ,y) (bin '+ (parse x) (parse y))]
    [`(* ,x ,y) (bin '* (parse x) (parse y))]
    [`(- ,x ,y) (bin '- (parse x) (parse y))]
    [`(/ ,x ,y) (bin '/ (parse x) (parse y))]
    [`(fun (,x) ,bdy) (fun x (parse bdy))]
    [`(,f ,x) (app (parse f) (parse x))]
    [`(seq ,fst ,snd) (seq (parse fst) (parse snd))]
    [`(set ,val ,newval) (set val (parse newval))]
    [x x]))

; op-trans: symbol -> (number number -> number)
; converts symbolic representation of arithmetic function to actual Racket function
(define (op-trans op)
  (match op
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]))



;; lookup: symbol env -> value
;; looks up a substitution in an environment (topmost one)

(define (lookup var env)
  (cond
    [(empty? env) (error 'interp "unbound variable ~a" var)]
    [(symbol=? var (sub-name (first env))) (sub-val (first env))]
    [else (lookup var (rest env))]))

(define (lookupSt loc store)
  (cond
    [(empty? store) (error 'interp "unbound variable ~a" loc)]
    [(symbol=? loc (subSt-loc (first store))) (subSt-val (first store))]
    [else (lookupSt loc (rest store))]))

(define (set-store loc s store)
  (cond
    [(empty? store) empty]
    [(symbol=? (subSt-loc (first store)) loc) (cons (subSt loc s) (rest store))]
    [else (cons (first store) (set-store loc s (rest store)))]))


;; interp: AST env sto-> value

(define (interp ast env store)
  (match ast
    [(fun v bdy) (closure v bdy env store)]
    [(app fun-exp arg-exp)
       (match (interp fun-exp env store)
         [(closure v bdy cl-env sto)
          (let ([loc (gensym v)])
            (interp bdy
                    (cons (sub v loc) cl-env)
                    (cons (subSt loc (result-val (interp arg-exp env store)))
                          (result-newstore (interp arg-exp env store)))))
          ])]
    [(bin op x y)
     (let ([sto1 (result-newstore (interp x env store))])
       (let ([sto2 (result-newstore (interp y env sto1))])
         (result (
                  (op-trans op)
                  (result-val (interp x env store))
                  (result-val (interp y env sto1))) sto2)))]
    [(seq ft sd)
     (let ([sto (result-newstore (interp ft env store))])
       (interp sd env sto))]



    [(set v nv)
     (let ([loc (lookup v env)])
       (let ([s (result-val (interp nv env store))])
         (result empty (set-store loc s store))))]
       
    

    
    [x (if (number? x)
           (result x store)
           (result (lookupSt (lookup x env) store) store))]))
            