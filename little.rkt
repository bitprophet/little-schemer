; Load readline
(require readline)
(require readline/rep-start)

; atom?
(define atom?
  (lambda (a)
    (not (list? a))))

; remove member
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))
