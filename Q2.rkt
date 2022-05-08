#lang racket


(define (get-char)
  (define my-char (read-char))
    (cond [(eof-object? my-char) empty]
          [else (cons my-char (get-char))]))


(define (member? piece cake)
  (cond
    [(empty? cake) false]
    [(equal? piece (first (first cake))) (second (first cake))]
    [else (member? piece (rest cake))]))



(define (Q2)
  (define (q2 get-char)
    (local
      [(define (output output-list lib my-word char-list index)
         (cond[(and (empty? char-list) (boolean? (member? my-word lib))) (display (list->string (append output-list my-word)))]
              
              [(empty? char-list) (display (list->string (append output-list (member? my-word lib))))]
              
              [(and (or (char=? #\space (first char-list)) (char=? #\newline (first char-list))) (not (empty? my-word)) (boolean? (member? my-word lib))) (output (append output-list (append my-word (list (first char-list)))) (append lib (list (list (string->list (number->string index)) my-word))) empty (rest char-list) (add1 index))]

              [(and (or (char=? #\space (first char-list)) (char=? #\newline (first char-list))) (boolean? (member? my-word lib))) (output (append output-list (list (first char-list))) lib empty (rest char-list) index)]

              [(or (char=? #\space (first char-list)) (char=? #\newline (first char-list))) (output (append output-list (append (member? my-word lib) (list (first char-list)))) lib empty (rest char-list) index)]

              [else (output output-list lib (append my-word (list (first char-list))) (rest char-list) index)]
              ))
       ]
      (output empty empty empty get-char 0)))
  (q2 (get-char)))


(Q2)

