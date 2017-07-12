#lang racket

(define atom?
   (lambda (a)
      (not (list? a))))

(define a 'a)
(define b 'b)
(define c 'c)
(define l '(a b c))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons old
                     (cons new (cdr lat))))
               (else (cons (car lat)
                           (insertR new old
                                    (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))
                    