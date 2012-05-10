; Load readline
(require readline)
(require readline/rep-start)

; atom?
(define (atom? a)
  (not (list? a)))

; remove member
(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (cdr lat))
    (else (cons (car lat) (rember a (cdr lat))))))

; first item of each list
(define (firsts lists)
  (cond
    ((null? lists) '())
    (else (cons
            (car (car lists))
            (firsts (cdr lists))))))

; insert atom to right of other atom
(define (insertR new existing lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) existing)
         (cons existing (cons new (cdr lat))))
        (else
          (cons (car lat) (insertR new existing (cdr lat))))))))

; insert atom to LEFT of other atom!
(define (insertL new existing lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) existing)
         (cons new lat))
        (else
          (cons (car lat) (insertL new existing (cdr lat))))))))

; substitute one atom for 1st occurrence of another
(define (subst new old lat)
  (cond
    ((null? lat) '())
    (else (cond
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat))))))))

; substitute one atom for 1st occurrence of one or two others
(define (subst2 new old1 old2 lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((or (eq? (car lat) old1) (eq? (car lat) old2))
         (cons new (cdr lat)))
        (else (cons (car lat) (subst2 new old1 old2 (cdr lat))))))))

; remove all matching members
(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))
