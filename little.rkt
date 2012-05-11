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
    ((zero? m) (zero? n))
    ((zero? n) #f)
    (else (eq (sub1 n) (sub1 m)))))

; raising to power
(define (pwr n m)
  (cond
    ((zero? m) 1)
    ((eq m 1) n)
    (else (mult n (pwr n (sub1 m))))))

; Length of a lat ("length" a racket builtin, using "len")
(define (len lat)
  (cond
    ((null? lat) 0)
    (else (add1 (len (cdr lat))))))

; Pick Nth item from a lat
(define (pick n lat)
  (cond
    ((eq? 1 n) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

; Remove Nth item
(define (rempick n lat)
  (cond
    ((eq? 1 n) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

; Remove all numbers
(define (nonums lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((number? (car lat)) (nonums (cdr lat)))
        (else (cons (car lat) (nonums (cdr lat))))))))

; Remove all non-numbers
(define (allnums lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((number? (car lat)) (cons (car lat) (allnums (cdr lat))))
        (else (allnums (cdr lat)))))))

; type-friendly equality
(define (eqan? a b)
  (cond
    ((and (number? a) (number? b)) (= a b))
    ((or (number? a) (number? b)) #f)
    (else (eq? a b))))

; counting an atom in a lat
(define (occur a lat)
  (cond
    ((null? lat) 0)
    (else
      (cond
        ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat)))))))

; recursive removal
(define (rember* a l)
  (cond
    ((null? l) '())
    ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
    (else
      (cond
        ((eqan? (car l) a) (rember* a (cdr l)))
        (else (cons (car l) (rember* a (cdr l))))))))

; recursive right insertion
(define (insertR* new existing l)
  (cond
    ((null? l) '())
    ((list? (car l))
     (cons (insertR* new existing (car l)) (insertR* new existing (cdr l))))
    (else
      (cond
        ((eq? (car l) existing)
         (cons existing (cons new (insertR* new existing (cdr l)))))
        (else (cons (car l) (insertR* new existing (cdr l))))))))
