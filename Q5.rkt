#lang racket

(require test-engine/racket-tests)

(struct bin (op fst snd) #:transparent) ; op is a symbol; fst, snd are ASTs.

(struct fun (param body) #:transparent) ; param is a symbol; body is an AST.

(struct app (fn arg) #:transparent) ; fn and arg are ASTs.

(struct rec (nm nmd body) #:transparent)

(struct ifzero (t tb fb) #:transparent)

;; An AST is a (union bin fun app).

(struct sub (name val) #:transparent)

;; A substitution is a (sub n v), where n is a symbol and v is a value.
;; An environment (env) is a list of substitutions.

(struct closure (var body envt) #:transparent #:mutable)

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
    [`(rec ((,nm ,nmd)) ,body)
     (rec nm (parse nmd) (parse body))]
    [`(ifzero ,t ,tb ,fb)
     (ifzero (parse t) (parse tb) (parse fb))]
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


;; interp: AST env -> value

(define (interp ast env)
  (match ast
    [(fun v bdy) (closure v bdy env)]
    [(app fun-exp arg-exp)
       (match (interp fun-exp env)
         [(closure v bdy cl-env)
          (interp bdy (cons (sub v (interp arg-exp env)) cl-env))])]
    [(bin op x y)
       ((op-trans op) (interp x env) (interp y env))]


    [(rec nm nmd body)
     (match (interp nmd env)
       [(closure v bdy cl-env)
        (define new-nmd (closure v bdy empty))
        (define new-env (cons (sub nm new-nmd) cl-env))
             (set-closure-envt! new-nmd new-env)
                   (interp body new-env)]
       [const
        (interp body (cons (sub nm const) env))])]


    [(ifzero t tb fb)
     (if (zero? (interp t env))
         (interp tb env)
         (interp fb env))]


    [x (if (number? x)
           x
           (lookup x env)
    )]))
             
; completely inadequate tests
;;(check-expect (parse '(* 2 3)) (bin '* 2 3))

;;(check-expect (interp (parse '(* 2 3)) empty) 6)


;;(test)
