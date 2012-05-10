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

; insert to right of all matching members
(define (multiinsertR new existing lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) existing)
         (cons existing (cons new (multiinsertR new existing (cdr lat)))))
        (else
          (cons (car lat) (multiinsertR new existing (cdr lat))))))))

; insert to left of all matching members
(define (multiinsertL new existing lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) existing)
         (cons new (cons existing (multiinsertL new existing (cdr lat)))))
        (else
          (cons (car lat) (multiinsertL new existing (cdr lat))))))))

; substitute all matching members
(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else
          (cons (car lat) (multisubst new old (cdr lat))))))))

; addition
(define (add a b)
  (cond
    ((zero? b) a)
    (else (add (add1 a) (sub1 b)))))

; subtraction
(define (sub a b)
  (cond
    ((zero? b) a)
    (else (sub (sub1 a) (sub1 b)))))

; add up all numbers in a tuple
(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (add (car tup) (addtup (cdr tup))))))

; multiplication
(define (mult a b)
  (cond
    ((zero? b) 0)
    (else (add a (mult a (sub1 b))))))

; zip-add two tuples
(define (tupadd tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons
            (add (car tup1) (car tup2))
            (tupadd (cdr tup1) (cdr tup2))))))

; greater-than test
(define (gt n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)
    (else (gt (sub1 n) (sub1 m)))))

; less-than
(define (lt n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (lt (sub1 n) (sub1 m)))))

; equal
(define (eq n m)
  (cond
    ((and (zero? n) (zero? m)) #t)
    ((or (zero? n) (zero? m)) #f)
    (else (eq (sub1 n) (sub1 m)))))
