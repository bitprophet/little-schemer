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

; recursive occur
(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
    (else
      (add (occur* a (car l)) (occur* a (cdr l))))))

; recursive subst
(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eqan? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons
            (subst* new old (car l))
            (subst* new old (cdr l))))))

; recursive left-insert
(define (insertL* new existing l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eqan? (car l) existing)
        (cons new (cons existing (insertL* new existing (cdr l)))))
       (else (cons (car l) (insertL* new existing (cdr l))))))
    (else (cons
            (insertL* new existing (car l))
            (insertL* new existing (cdr l))))))

; recursive member test
(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eqan? (car l) a) #t)
       (else (member* a (cdr l)))))
    (else (or (member* a (car l)) (member* a (cdr l))))))

; Find leftmost atom in a nested list
(define (leftmost l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

; Compare lists
(define (eqlist? l1 l2)
  (cond
    ; two nil lists are equal
    ((and (null? l1) (null? l2)) #t)
    ; no both nil, so then OR means one is nil, ergo definitely not equal
    ((or (null? l1) (null? l2)) #f)
    ; both cars are atoms?
    ((and (atom? (car l1)) (atom? (car l2)))
     (cond
      ; eqan? recurse onto cdrs
      ((eqan? (car l1) (car l2)) (and (eqlist? (cdr l1) (cdr l2))))
      ; not equan? #f
      (else #f)))
    ; only one is an atom? #f
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    ; both cars are lists? recurse onto both
    ((and (list? (car l1)) (list? (car l2)))
     (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    ; only one is a list? #f
    (else #f)))

; Compare S-expressions
(define (myequal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    ; At this point atoms and nulls are ruled out; must be two lists
    (else (eqlist? s1 s2))))

; eqlist using myequal
(define (eqlist2? l1 l2)
  (cond
    ; two nil lists are equal
    ((and (null? l1) (null? l2)) #t)
    ; no both nil, so then OR means one is nil, ergo definitely not equal
    ((or (null? l1) (null? l2)) #f)
    ; equality test on cars & cdrs
    (else (and (myequal? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2))))))

; member?
(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eqan? (car lat) a) #t)
    (else (member? a (cdr lat)))))

; set test
(define (myset? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (myset? (cdr lat)))))

; set enforcement
(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

; set enforcement using multirember
(define (makeset2 lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))
