#lang racket

(define (get-char)
  (define my-char (read-char))
    (cond [(eof-object? my-char) empty]
          [else (cons my-char (get-char))]))

(define (member? piece cake)
  (cond
    [(empty? cake) false]
    [(equal? piece (first (first cake))) true]
    [else (member? piece (rest cake))]))

(define (Q1)
  (define (q1 get-char)
  (local[(define (output output-list lib my-word char-list index)
           (cond[(and (empty? char-list) (not (member? my-word lib)))  (display (list->string (append output-list my-word)))]

                [(empty? char-list)  (display (list->string (append output-list (string->list (number->string (foldr (lambda(x rror)
                                                                        (cond[(equal? my-word (first x)) (+ rror (second x))]
                                                                             [else rror])) 0 lib))))))]

                [(and (or (char=? #\space (first char-list)) (char=? #\newline (first char-list))) (not (member? my-word lib)) (not (empty? my-word))) (output (append output-list (append my-word (list (first char-list)))) (append lib (list (list my-word index))) empty (rest char-list) (add1 index))]

                [(and (or (char=? #\space (first char-list)) (char=? #\newline (first char-list))) (not (member? my-word lib))) (output (append output-list (list (first char-list))) lib empty (rest char-list) index)]
                
                [(or (char=? #\space (first char-list)) (char=? #\newline (first char-list))) (output (append output-list
                                                                                                              (append (string->list (number->string (foldr (lambda(x rror)
                                                                                                                                     (cond[(equal? my-word (first x)) (+ rror (second x))]
                                                                                                                                          [else rror])) 0 lib)))
                                                                                                                      (list (first char-list))
                                                                                                                                  )) lib empty (rest char-list) index)]

                [else (output output-list lib (append my-word (list (first char-list))) (rest char-list) index)]))
         
         ]
    (output empty empty empty get-char 0)))
  (q1 (get-char)))

(Q1)


