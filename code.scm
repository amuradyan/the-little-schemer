;;;;;;; Toys

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(atom? 'a)          ; true
(atom? '(f (g h)))  ; false

(null? ())  ; true

; The Law of Car
;
;   The primitive `car` is defined only for non-empty lists.

; The Law of Cdr
;
;   The primitive `cdr` is defined only for non-empty lists.
;   The `cdr` of any non-empty list is always another list.

; The Law of Cons
;
;   The primitive `cons` takes two arguments. The second
;   argument to `cons` must be a list. The result is a list.

; The Law of Null?
;
;   The primitive `null?` is defined only for lists.

; The Law of Eq?
;
;   The primitive `eq?` takes two atguments. Each must be
;   a non-numeric atom.


;;;;;;; Do it, do it again, and again, and again ...

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(lat? ())           ; true
(lat? '((t g) r))   ; false

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(member? 'd '(a d g)) ; true
(member? 'd '(a g))   ; false

; The First Commandment
;     (preliminary)
;
;   Always ask `null?` as the first questionm in expressing any function.


;;;;;;; Cons the Magnificent

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) ())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(rember 'a '(a b c))      ; (b c)
(rember 'a '(a b a c))    ; (b a c)
(rember 'a '(b a g a c))  ; (b g a c)
(rember 'a '(f g))        ; (f g)
(rember 'a ())            ; ()

; The Second Commandment
;
;   Use `cons` to build lists.

(define firsts
  (lambda (l)
    (cond
      ((null? l) ())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts ())                       ; ()
(firsts '((a b) (c d) (e f)))     ; (a c e)
(firsts '(((a b) c) (d e) (f)))   ; ((a b) d f)

(define seconds
  (lambda (l)
    (cond
      ((null? l) ())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(seconds ())                              ; ()
(seconds '((a b) (c d) (e f)))            ; (b d f)
(seconds '(((a b) c) (d e) (f (g h))))    ; (c e (g h)))

; The Third Commandment
;
;   When building a list, describe the first typical element,
;   and then `cons` it onto the natural recursion.

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat)))))))))

(insertR 'c 'b '(a b d))     ; (a b c d)
(insertR 'c 'b '(a b b d))   ; (a b c b d)
(insertR 'c 'b ())           ; ()

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))))

(insertL 'c 'b '(a b d))     ; (a c b d)
(insertL 'c 'b '(a b b d))   ; (a c b b d)
(insertL 'c 'b ())           ; ()

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat)))))))))

(subst 'c 'b '(a b d))     ; (a c d)
(subst 'c 'b '(a b b d))   ; (a c b d)
(subst 'c 'b ())           ; ()

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((or (eq? (car lat) o1) (eq? (car lat) o2))
          (cons new (cdr lat)))
        (else
          (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(subst2 'c 'b 'd '(a b d))     ; (a c d)
(subst2 'c 'b 'd '(a d b))     ; (a c b)
(subst2 'c 'b 'd '(a b b d))   ; (a c b d)
(subst2 'c 'b 'd ())           ; ()

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) ())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'a '(a b c))     ; (b c)
(multirember 'a '(a b a c))   ; (b c)
(multirember 'a '(b a g a c)) ; (b g c)
(multirember 'a '(f g))       ; (f g)
(multirember 'a ())           ; ()

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(multiinsertR 'c 'b '(a b d))     ; (a b c d)
(multiinsertR 'c 'b '(a b b d))   ; (a b c b c d)
(multiinsertR 'c 'b ())           ; ()

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(multiinsertL 'c 'b '(a b d))     ; (a c b d)
(multiinsertL 'c 'b '(a b b d))   ; (a c b c b d)
(multiinsertL 'c 'b ())           ; ()

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(multisubst 'c 'b '(a b d))     ; (a c d)
(multisubst 'c 'b '(a b b d))   ; (a c c d)
(multisubst 'c 'b ())           ; ()

; The Fourth Commandment
;
;   Always change at least one argument while recurring. It
;   must be changed to be closer to termination. The changing
;   argument must be tested in the termination condition:
;   when using `cdr`, test termination with `null?`.


;;;;;;; Numbers Games

(define add1
  (lambda (n) (+ n 1)))

(add1 5)  ; 6

(define sub1
  (lambda (n) (- n 1)))

(sub1 5)  ; 4

(define add
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add (add1 a) (sub1 b))))))

(add 46 12) ; 58

(define sub
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub (sub1 a) (sub1 b))))))

(sub 14 3)  ; 11

; The First Commandment
;     (first revision)
;
;   When recurring on a list of atoms, `lat`, ask two questions
;   about it: `(null? lat)` and else.
;   When recurring on a number, `n`, ask two questions about
;   it: `(zero? n)` and else.

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4)) ; 10
(addtup ())         ; 0

; The Fourth Commandment
;     (first revision)
;
;   Always change at least one argument while recurring. It
;   must be changed ti be closer to termination. The changing
;   argument must be tested in the termination condition:
;   when using `cdr`, test termination with `null?` and
;   when testing `sub1`, test termination with `zero?`.

(define times
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (times n (sub1 m)))))))

(times 13 4)  ; 52
(times 13 0)  ; 0
(times 0 13)  ; 0
(times 1 13)  ; 13
(times 13 1)  ; 13

; The Fifth Commandment
;
;   When building a value with `add`, always use 0 for the value of the
;   terminating line, for adding 0 does not change the value of an
;   addition.
;   When building a value with `times`, always use 1 for the value of the
;   terminating line, for multiplying with 1 does not change the value
;   of a multiplication.
;   When building a value with `cons`, always consider () for the value
;   of the terminating line.

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (add (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7)) ; (11 11 11 11 11)
(tup+ '(3 7) '(4 6 8 1))          ; (7 13 8 1)
(tup+ '(3 7 8 1) '(4 6))          ; (7 13 8 1)
(tup+ () ())                      ; ()

(define gt
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (gt (sub1 m) (sub1 n))))))

(gt 12 120) ; false
(gt 12 12)  ; false
(gt 120 12) ; true

(define lt
  (lambda (m n)
    (gt n m)))

(lt 4 44) ; true
(lt 2 2)  ; false
(lt 44 4) ; false

(define equals?
  (lambda (m n)
    (cond
      ((or (gt m n) (lt m n)) #f)
      (else #t))))

(equals? 4 44)  ; false
(equals? 44 4)  ; false
(equals? 4 4)   ; true

(define pow
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (times m (pow m (sub1 n)))))))

(pow 1 1) ; 1
(pow 2 3) ; 8
(pow 5 3) ; 125

(define div
  (lambda (m n)
    (cond
      ((lt m n) 0)
      (else (add1 (div (sub m n) n))))))

(div 15 4)  ; 3

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length '(a b c d)) ; 4
(length ())         ; 0

(define pick
  (lambda (index lat)
    (cond
      ((zero? (sub1 index)) (car lat))
      ((zero? index) ())
      (else (pick (sub1 index) (cdr lat))))))

(pick 3 '(a b c d e)) ; c
(pick 0 '(a))         ; ()

(define rempick
  (lambda (index lat)
    (cond
      ((lt (length lat) index) ())
      ((null? lat) ())
      ((zero? (sub1 index)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 index) (cdr lat)))))))

(rempick 3 '(a b c d e))  ; (a b d e)
(rempick 9 '(a b c d e))  ; ()
(rempick 2 ())            ; ()

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) ())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates)) ; (pears prunes dates)
(no-nums '(5 6 7))                    ; ()
(no-nums ())                          ; ()

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) ())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))  ; (5 6 9)
(all-nums '(5 6 7))                     ; (5 6 7)
(all-nums ())                           ; ()

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (equals? a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      (else #f))))

(eqan? 'a 1)  ; false
(eqan? 1 'a)  ; false
(eqan? 1 3)   ; false
(eqan? 'a 'b) ; false
(eqan? 'a 'a) ; true
(eqan? 1 1)   ; true

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(occur 1 '(1 a 1 b 3 f 1))  ; 3
(occur 1 ())                ; 0
(occur 1 '(a b c d f))      ; 0

(define one?
  (lambda (n)
    (eqan? n 1)))

(one? 1)    ; true
(one? 'one) ; false
(one? 0)    ; false

(define rempick-via-one
  (lambda (index lat)
    (cond
      ((lt (length lat) index) ())
      ((null? lat) ())
      ((one? index) (cdr lat))
      (else (cons (car lat) (rempick-via-one (sub1 index) (cdr lat)))))))

(rempick-via-one 3 '(a b c d e))  ; (a b d e)
(rempick-via-one 9 '(a b c d e))  ; ()
(rempick-via-one 2 ())            ; ()


;;;;;;; *Oh My Gawd*: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond
      ((null? l) ())
      ((atom? (car l))
        (cond
          ((eqan? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))                ; ((coffee) ((tea)) (and (hick)))
(rember* 'sauce '(((tomato) sauce) ((bean) sauce) (and ((flying) sauce)))) ; (((tomato)) ((bean)) (and ((flying))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) ())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
; ((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood)

; The First Commandment
;     (final version)
;
;   When recurring on a list of atoms, `lat`, ask two questions
;   about it: `(null? lat) and else.
;   When recurring on a number, `n`, ask two questions about
;   it: `(zero? n)` and else
;   When recurring on a list of S-expressions, `l`, ask three
;   questions about it: `(null? l)`, `(atom? (car l))`, and else.

; The Fourth Commandment
;     (final version)
;
;   Always change at least one argument while recurring.
;   When recurring on a list of atoms, `lat`, use `(cdr lat).
;   When recurring on a number, `n`, use `(sub1 n)`. And when
;   recurring on a list of S-expressions, `l`, use `(car l)` and
;   `(cdr l)` if neither `(null? l)` nor `(atom? (car l))` are true.
;
;   It must be changed closer to be closer to termination. The
;   changing argument must be tested in the termination condition:
;   - when using `cdr`, test termination with `null?`, and
;   - when using `sub1`, test termination with `zero?`.

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eqan? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else (add (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'a '(a ((b) a) (c d (a) a) (a)))  ; 5
(occur* 'a ())                            ; 0
(occur* 'a '(b (c d (e (f)) g)))          ; 0

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) ())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'b 'a '(a (((a) b) c) (b (a) ((a) c)))) ; (b (((b) b) c) (b (b) ((b) c)))
(subst* 'b 'a ())                               ; ()
(subst* 'b 'a '(b (c (d (e) f) g) h))           ; (b (c (d (e) f) g) h)

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) ())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons old (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'b 'a '(a (((a) b) c) (b (a) ((a) c)))) ; (b a (((b a) b) c) (b (b a) ((b a) c)))
(insertL* 'b 'a ())                               ; ()
(insertL* 'b 'a '(b (c (d (e) f) g) h))           ; (b (c (d (e) f) g) h)

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (cond
          ((eqan? a (car l)) #t)
          (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'a '(a (((a) b) c) (b (a) ((a) c)))) ; true
(member* 'a ())                               ; false
(member* 'a '(b (c (d (e) f) g) h))           ; false

(define leftmost
  (lambda (l)
    (cond
      ((null? l) ())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost ())               ; ()
(leftmost '(a b c))         ; a
(leftmost '((a (b)) c))     ; a
(leftmost '(() (a (b)) c))  ; ()

; `eqlist?` without `equals?`
; -----------------------------
; (define eqlist?
;   (lambda (l1 l2)
;     (cond
;       ((and (null? l1) (null? l2)) #t)
;       ((atom? (car l1))
;         (cond
;           ((atom? (car l2)) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;           (else #f)))
;       (else
;         (cond
;           ((atom? (car l2)) #f)
;           (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))
; -----------------------------
; (eqlist? '(a (b)) '((a) b)) ; false
; (eqlist? '(a (b)) '(a (b))) ; true
; (eqlist? '(a b) '(a b))     ; true
; (eqlist? () ())             ; true

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

(eqlist? '(a (b)) '((a) b)) ; false
(eqlist? '(a (b)) '(a (b))) ; true
(eqlist? '(a b) '(a b))     ; true
(eqlist? () ())             ; true

(define equal?
  (lambda (sexp1 sexp2)
    (cond
      ((and (atom? sexp1) (atom? sexp2)) (eqan? sexp1 sexp2))
      ((or (atom? sexp1) (atom? sexp2)) #f)
      (else (eqlist? sexp1 sexp2)))))

(equal? () ())              ; true
(equal? 'a 'a)              ; true
(equal? '(a b) '(a b))      ; true
(equal? () 'a)              ; false
(equal? 'a 'b)              ; false
(equal? '(a (b)) '((a) b))  ; false

; The Sixth Commandment
;
;   Simplify only after the function is correct.

(define rember-simplified
  (lambda (s l)
    (cond
      ((null? l) ())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember-simplified s (cdr l)))))))

(rember-simplified 'a '(a b c))      ; (b c)
(rember-simplified 'a '(a b a c))    ; (b a c)
(rember-simplified 'a '(b a g a c))  ; (b g a c)
(rember-simplified 'a '(f g))        ; (f g)
(rember-simplified 'a ())            ; ()


;;;;;;; Shadows

; Below we assume the names for the following arithmetic operations:
;   - plus for +
;   - times for *
;   - pow for ^

(define numbered?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((number? x) #t)
      ((atom? (car x))
        (cond
          ((number? (car x)) (numbered? (cdr x)))
          ((equal? (car x) 'pow) (numbered? (cdr x)))
          ((equal? (car x) 'plus) (numbered? (cdr x)))
          ((equal? (car x) 'times) (numbered? (cdr x)))
          (else #f)))
      (else (and (numbered? (car x)) (numbered? (cdr x)))))))

(numbered? 4)                               ; true
(numbered? '(3 plus (4 pow (5 times 2))))   ; true
(numbered? ())                              ; true
(numbered? '(3 what? (4 pow (5 times 2))))  ; false

(define numbered?-per-book
  (lambda (x)
    (cond
      ((atom? x) (number? x))
      (else (and (numbered?-per-book (car x)) (numbered?-per-book (car (cdr (cdr x)))))))))


(numbered?-per-book 4)                                ; true
(numbered?-per-book '(3 plus (4 pow (5 times 2))))    ; true
(numbered?-per-book ())                               ; true
(numbered?-per-book '(3 what? (4 pow (5 times 2))))   ; false

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp))
      ((eq? (car (cdr (nexp))) 'plus) (add (value (car nexp) (value (cdr (cdr nexp))))))
      ((eq? (car (cdr (nexp))) 'times) (times (value (car nexp) (value (cdr (cdr nexp))))))
      (else (pow (value (car nexp)) (value (cdr (cdr nexp))))))))

(value 3)                             ; 3
(value '(3 plus 5))                   ; 8
(value '(8 times 10))                 ; 80
(value '(8 times (10 plus 1)))        ; 88
(value '(2 plus (3 times (2 pow 2)))) ; 14

; The Seventh Commandment
;
;   Recur on the `subparts` that are of the same nature:
;   - On the sublists of a list
;   - On the subexpressions of an arithmetic expression

; First try before the appropriate abstractions
;
; (define prefix-value
;   (lambda (aexp)
;     (cond
;       ((number? aexp) aexp)
;       ((eq? (car aexp) 'plus) (add (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))))
;       ((eq? (car aexp) 'times) (times (value (car (cdr aexp))) (value (car (cdr (cdr aexp))))))
;       (else (pow (value (car (cdr aexp))) (value (car (cdr (cdr aexp)))))))))

; (prefix-value '(plus 3 4))                   ; 7
; (prefix-value '(plus (times 3 2) (pow 4 2))) ; 22
; (prefix-value '(times 5 (plus 2 3)))         ; 25

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(1st-sub-exp '(plus 3 4))                   ; 3
(1st-sub-exp '(plus (times 3 2) (pow 4 2))) ; (times 3 2)
(1st-sub-exp '(times 5 (plus 2 3)))         ; 5

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(2nd-sub-exp '(plus 3 4))                   ; 4
(2nd-sub-exp '(plus (times 3 2) (pow 4 2))) ; (pow 4 2)
(2nd-sub-exp '(times 5 (plus 2 3)))         ; (plus 2 3)

(define operator
  (lambda (aexp)
    (car aexp)))

(operator '(plus 3 4))                    ; plus
(operator '(plus (times 3 2) (pow 4 2)))  ; plus
(operator '(times 5 (plus 2 3)))          ; times

(define prefix-value
  (lambda (aexp)
    (cond
      ((number? aexp) aexp)
      ((eq? (operator aexp) 'plus) (add (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) 'times) (times (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
      (else (pow (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp)))))))

(prefix-value '(plus 3 4))                   ; 7
(prefix-value '(plus (times 3 2) (pow 4 2))) ; 22
(prefix-value '(times 5 (plus 2 3)))         ; 25

(define 1st-infix-sub-exp
  (lambda (nexp)
    (car nexp)))

(1st-infix-sub-exp 3)                             ; 3
(1st-infix-sub-exp '(3 plus 5))                   ; 3
(1st-infix-sub-exp '(8 times 10))                 ; 8
(1st-infix-sub-exp '(8 times (10 plus 1)))        ; 8
(1st-infix-sub-exp '(2 plus (3 times (2 pow 2)))) ; 2

(define 2nd-infix-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(2nd-infix-sub-exp '(3 plus 5))                 ; 5
(2nd-infix-sub-exp '(8 times 10))               ; 10
(2nd-infix-sub-exp '(8 times (10 plus 1)))      ; (10 plus 1)
(2nd-infix-sub-exp '(2 plus (3 times (2 pow 2)) ; (3 times (2 pow 2))

(define infix-operator
  (lambda (aexp)
    (car (cdr aexp))))

(infix-operator '(3 plus 5))                    ; plus
(infix-operator '(8 times 10))                  ; times
(infix-operator '(8 times (10 plus 1)))         ; times
(infix-operator '(2 plus (3 times (2 pow 2))))  ; plus

(define infix-value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (infix-operator nexp) 'plus)
        (add
          (value (1st-infix-sub-exp nexp)
          (value (2nd-infix-sub-exp nexp)))))
      ((eq? (infix-operator nexp) 'times)
        (times
          (value (1st-infix-sub-exp nexp)
          (value (2nd-infix-sub-exp nexp)))))
      (else
        (pow
          (value (1st-infix-sub-exp nexp))
          (value (2nd-infix-sub-exp nexp)))))))

(infix-value 3)                             ; 3
(infix-value '(3 plus 5))                   ; 8
(infix-value '(8 times 10))                 ; 80
(infix-value '(8 times (10 plus 1)))        ; 88
(infix-value '(2 plus (3 times (2 pow 2)))) ; 14

; The Eighth Commandment
;
;   Use help functions to abstract fom representations

(define zro?
  (lambda (n)
    (null? n)))

(zro? (()))  ; true

(define pljusmek
  (lambda (n)
    (cons () n)))

(pljusmek '(())) ; (() ())

(define minusmek
  (lambda (n)
    (cdr n)))

(minusmek '(() ())) ; (())

(define gumar
  (lambda (a b)
    ((zro? b) a)
    (else (pljusmek (gumar a (minusmek b))))))

(gumar '(()) '(() ()))  ; (() () ())
