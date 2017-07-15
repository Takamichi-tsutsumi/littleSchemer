#lang racket
(define atom?
   (lambda (a)
      (not (list? a))))

(define a 'a)
(define b 'b)
(define c 'c)
(define l '(a b c))


; insert new to the right of old in lat
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

; insert new to the left of old in lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))


; substitute old in lat with new
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old
                         (cdr lat)))))))


; subst first occurence of o1 or o2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      ((eq? o1 (car lat)) (cons new (cdr lat)))
      ((eq? o2 (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))












