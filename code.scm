(define section
  (lambda (caption)
    (newline)
    (display caption)
    (newline)
    (newline)))

(define print
  (lambda (sexp)
    (display sexp)
    (display " => ")
    (display (eval sexp))
    (newline)))

(section "Toys")

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(print '(atom? 'a))          ; true
(print '(atom? '(f (g h))))  ; false

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


(section "Do it, do it again, and again, and again ...")

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(print '(lat? '()))          ; true
(print '(lat? '((t g) r)))   ; false

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(print '(member? 'd '(a d g))) ; true
(print '(member? 'd '(a g)))   ; false

; The First Commandment
;     (preliminary)
;
;   Always ask `null?` as the first questionm in expressing any function.


;;;;;;; Cons the Magnificent

(section "Cons the Magnificent")

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(print '(rember 'a '(a b c)))      ; (b c)
(print '(rember 'a '(a b a c)))    ; (b a c)
(print '(rember 'a '(b a g a c)))  ; (b g a c)
(print '(rember 'a '(f g)))        ; (f g)
(print '(rember 'a '()))           ; ()

; The Second Commandment
;
;   Use `cons` to build lists.

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(print '(firsts '()))                      ; ()
(print '(firsts '((a b) (c d) (e f))))     ; (a c e)
(print '(firsts '(((a b) c) (d e) (f))))   ; ((a b) d f)

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(print '(seconds '()))                             ; ()
(print '(seconds '((a b) (c d) (e f))))            ; (b d f)
(print '(seconds '(((a b) c) (d e) (f (g h)))))    ; (c e (g h)))

; The Third Commandment
;
;   When building a list, describe the first typical element,
;   and then `cons` it onto the natural recursion.

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat)))))))))

(print '(insertR 'c 'b '(a b d)))     ; (a b c d)
(print '(insertR 'c 'b '(a b b d)))   ; (a b c b d)
(print '(insertR 'c 'b '()))          ; ()

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))))

(print '(insertL 'c 'b '(a b d)))     ; (a c b d)
(print '(insertL 'c 'b '(a b b d)))   ; (a c b b d)
(print '(insertL 'c 'b '()))          ; ()

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat)))))))))

(print '(subst 'c 'b '(a b d)))     ; (a c d)
(print '(subst 'c 'b '(a b b d)))   ; (a c b d)
(print '(subst 'c 'b '()))          ; ()

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((or (eq? (car lat) o1) (eq? (car lat) o2))
          (cons new (cdr lat)))
        (else
          (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(print '(subst2 'c 'b 'd '(a b d)))     ; (a c d)
(print '(subst2 'c 'b 'd '(a d b)))     ; (a c b)
(print '(subst2 'c 'b 'd '(a b b d)))   ; (a c b d)
(print '(subst2 'c 'b 'd '()))          ; ()

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(print '(multirember 'a '(a b c)))     ; (b c)
(print '(multirember 'a '(a b a c)))   ; (b c)
(print '(multirember 'a '(b a g a c))) ; (b g c)
(print '(multirember 'a '(f g)))       ; (f g)
(print '(multirember 'a '()))          ; ()

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(print '(multiinsertR 'c 'b '(a b d)))     ; (a b c d)
(print '(multiinsertR 'c 'b '(a b b d)))   ; (a b c b c d)
(print '(multiinsertR 'c 'b '()))          ; ()

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(print '(multiinsertL 'c 'b '(a b d)))     ; (a c b d)
(print '(multiinsertL 'c 'b '(a b b d)))   ; (a c b c b d)
(print '(multiinsertL 'c 'b '()))          ; ()

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(print '(multisubst 'c 'b '(a b d)))     ; (a c d)
(print '(multisubst 'c 'b '(a b b d)))   ; (a c c d)
(print '(multisubst 'c 'b '()))          ; ()

; The Fourth Commandment
;
;   Always change at least one argument while recurring. It
;   must be changed to be closer to termination. The changing
;   argument must be tested in the termination condition:
;   when using `cdr`, test termination with `null?`.


; ;;;;;;; Numbers Games

(section "Numbers Games")

(define add1
  (lambda (n) (+ n 1)))

(print '(add1 5))  ; 6

(define sub1
  (lambda (n) (- n 1)))

(print '(sub1 5))  ; 4

(define add
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add (add1 a) (sub1 b))))))

(print '(add 46 12)) ; 58

(define sub
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub (sub1 a) (sub1 b))))))

(print '(sub 14 3))  ; 11

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

(print '(addtup '(1 2 3 4))) ; 10
(print '(addtup '()))        ; 0

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

(print '(times 13 4))  ; 52
(print '(times 13 0))  ; 0
(print '(times 0 13))  ; 0
(print '(times 1 13))  ; 13
(print '(times 13 1))  ; 13

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

(print '(tup+ '(3 6 9 11 4) '(8 5 2 0 7))) ; (11 11 11 11 11)
(print '(tup+ '(3 7) '(4 6 8 1)))          ; (7 13 8 1)
(print '(tup+ '(3 7 8 1) '(4 6)))          ; (7 13 8 1)
(print '(tup+ '() '()))                    ; ()

(define gt
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (gt (sub1 m) (sub1 n))))))

(print '(gt 12 120)) ; false
(print '(gt 12 12))  ; false
(print '(gt 120 12)) ; true

(define lt
  (lambda (m n)
    (gt n m)))

(print '(lt 4 44)) ; true
(print '(lt 2 2))  ; false
(print '(lt 44 4)) ; false

(define equals?
  (lambda (m n)
    (cond
      ((or (gt m n) (lt m n)) #f)
      (else #t))))

(print '(equals? 4 44))  ; false
(print '(equals? 44 4))  ; false
(print '(equals? 4 4))   ; true

(define pow
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (times m (pow m (sub1 n)))))))

(print '(pow 1 1)) ; 1
(print '(pow 2 3)) ; 8
(print '(pow 5 3)) ; 125

(define div
  (lambda (m n)
    (cond
      ((lt m n) 0)
      (else (add1 (div (sub m n) n))))))

(print '(div 15 4))  ; 3

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(print '(length '(a b c d))) ; 4
(print '(length '()))        ; 0

(define pick
  (lambda (index lat)
    (cond
      ((zero? (sub1 index)) (car lat))
      ((zero? index) '())
      (else (pick (sub1 index) (cdr lat))))))

(print '(pick 3 '(a b c d e))) ; c
(print '(pick 0 '(a)))         ; ()

(define rempick
  (lambda (index lat)
    (cond
      ((lt (length lat) index) '())
      ((null? lat) '())
      ((zero? (sub1 index)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 index) (cdr lat)))))))

(print '(rempick 3 '(a b c d e)))  ; (a b d e)
(print '(rempick 9 '(a b c d e)))  ; ()
(print '(rempick 2 '()))           ; ()

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(print '(no-nums '(5 pears 6 prunes 9 dates))) ; (pears prunes dates)
(print '(no-nums '(5 6 7)))                    ; ()
(print '(no-nums '()))                         ; ()

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(print '(all-nums '(5 pears 6 prunes 9 dates)))  ; (5 6 9)
(print '(all-nums '(5 6 7)))                     ; (5 6 7)
(print '(all-nums '()))                          ; ()

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (equals? a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      (else #f))))

(print '(eqan? 'a 1))  ; false
(print '(eqan? 1 'a))  ; false
(print '(eqan? 1 3))   ; false
(print '(eqan? 'a 'b)) ; false
(print '(eqan? 'a 'a)) ; true
(print '(eqan? 1 1))   ; true

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(print '(occur 1 '(1 a 1 b 3 f 1)))  ; 3
(print '(occur 1 '()))               ; 0
(print '(occur 1 '(a b c d f)))      ; 0

(define one?
  (lambda (n)
    (eqan? n 1)))

(print '(one? 1))    ; true
(print '(one? 'one)) ; false
(print '(one? 0))    ; false

(define rempick-via-one
  (lambda (index lat)
    (cond
      ((lt (length lat) index) '())
      ((null? lat) '())
      ((one? index) (cdr lat))
      (else (cons (car lat) (rempick-via-one (sub1 index) (cdr lat)))))))

(print '(rempick-via-one 3 '(a b c d e)))  ; (a b d e)
(print '(rempick-via-one 9 '(a b c d e)))  ; '()
(print '(rempick-via-one 2 '()))           ; '()


;;;;;;; *Oh My Gawd*: It's Full of Stars

(section "*Oh My Gawd*: It's Full of Stars")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(print '(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)))                ; ((coffee) ((tea)) (and (hick)))
(print '(rember* 'sauce '(((tomato) sauce) ((bean) sauce) (and ((flying) sauce))))) ; (((tomato)) ((bean)) (and ((flying))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(print '(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
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

(print '(occur* 'a '(a ((b) a) (c d (a) a) (a))))  ; 5
(print '(occur* 'a '()))                           ; 0
(print '(occur* 'a '(b (c d (e (f)) g))))          ; 0

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(print '(subst* 'b 'a '(a (((a) b) c) (b (a) ((a) c))))) ; (b (((b) b) c) (b (b) ((b) c)))
(print '(subst* 'b 'a '()))                              ; '()
(print '(subst* 'b 'a '(b (c (d (e) f) g) h)))           ; (b (c (d (e) f) g) h)

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons old (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(print '(insertL* 'b 'a '(a (((a) b) c) (b (a) ((a) c))))) ; (b a (((b a) b) c) (b (b a) ((b a) c)))
(print '(insertL* 'b 'a '()))                              ; '()
(print '(insertL* 'b 'a '(b (c (d (e) f) g) h)))           ; (b (c (d (e) f) g) h)

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (cond
          ((eqan? a (car l)) #t)
          (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(print '(member* 'a '(a (((a) b) c) (b (a) ((a) c))))) ; true
(print '(member* 'a '()))                              ; false
(print '(member* 'a '(b (c (d (e) f) g) h)))           ; false

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(print '(leftmost '()))              ; '()
(print '(leftmost '(a b c)))         ; a
(print '(leftmost '((a (b)) c)))     ; a
(print '(leftmost '('() (a (b)) c))) ; '()

; `eqlist?` without `equals?`
; -----------------------------

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((atom? (car l1))
        (cond
          ((atom? (car l2)) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
          (else #f)))
      (else
        (cond
          ((atom? (car l2)) #f)
          (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))

; -----------------------------

(print '(eqlist? '(a (b)) '((a) b))) ; false
(print '(eqlist? '(a (b)) '(a (b)))) ; true
(print '(eqlist? '(a b) '(a b)))     ; true
(print '(eqlist? '() '()))           ; true

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

(print '(eqlist? '(a (b)) '((a) b))) ; false
(print '(eqlist? '(a (b)) '(a (b)))) ; true
(print '(eqlist? '(a b) '(a b)))     ; true
(print '(eqlist? '() '()))            ; true

(define equal?
  (lambda (sexp1 sexp2)
    (cond
      ((and (atom? sexp1) (atom? sexp2)) (eqan? sexp1 sexp2))
      ((or (atom? sexp1) (atom? sexp2)) #f)
      (else (eqlist? sexp1 sexp2)))))

(print '(equal? '() '()))            ; true
(print '(equal? 'a 'a))              ; true
(print '(equal? '(a b) '(a b)))      ; true
(print '(equal? '() 'a))             ; false
(print '(equal? 'a 'b))              ; false
(print '(equal? '(a (b)) '((a) b)))  ; false

; The Sixth Commandment
;
;   Simplify only after the function is correct.

(define rember-simplified
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember-simplified s (cdr l)))))))

(print '(rember-simplified 'a '(a b c)))      ; (b c)
(print '(rember-simplified 'a '(a b a c)))    ; (b a c)
(print '(rember-simplified 'a '(b a g a c)))  ; (b g a c)
(print '(rember-simplified 'a '(f g)))        ; (f g)
(print '(rember-simplified 'a '()))           ; '()


;;;;;;; Shadows

(section "Shadows")

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

(print '(numbered? 4))                               ; true
(print '(numbered? '(3 plus (4 pow (5 times 2)))))   ; true
(print '(numbered? '()))                             ; true
(print '(numbered? '(3 what? (4 pow (5 times 2)))))  ; false

(define numbered?-per-book
  (lambda (x)
    (cond
      ((atom? x) (number? x))
      (else (and (numbered?-per-book (car x)) (numbered?-per-book (car (cdr (cdr x)))))))))

(print '(numbered?-per-book 4))                                ; true
(print '(numbered?-per-book '(3 plus (4 pow (5 times 2)))))    ; true
; (numbered?-per-book '())                            ; can't test, since we don't consider empty lists
(print '(numbered?-per-book '(3 what? (4 pow (5 times 2)))))   ; false

(define value
  (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      ((and (list? nexp) (eq? 1 (length nexp))) (value (car nexp)))
      ((eq? (car (cdr nexp)) 'plus) (add (value (car nexp)) (value (cdr (cdr nexp)))))
      ((eq? (car (cdr nexp)) 'times) (times (value (car nexp)) (value (cdr (cdr nexp)))))
      (else (pow (value (car nexp)) (value (cdr (cdr nexp))))))))

(print '(value 3))                             ; 3
(print '(value '(3 plus 5)))                   ; 8
(print '(value '(8 times 10)))                 ; 80
(print '(value '(8 times (10 plus 1))))        ; 88
(print '(value '(2 plus (3 times (2 pow 2))))) ; 14

; The Seventh Commandment
;
;   Recur on the `subparts` that are of the same nature:
;   - On the sublists of a list
;   - On the subexpressions of an arithmetic expression

; First try before the appropriate abstractions

(define prefix-value
  (lambda (aexp)
    (cond
      ((number? aexp) aexp)
      ((eq? (car aexp) 'plus) (add (prefix-value (car (cdr aexp))) (prefix-value (car (cdr (cdr aexp))))))
      ((eq? (car aexp) 'times) (times (prefix-value (car (cdr aexp))) (prefix-value (car (cdr (cdr aexp))))))
      (else (pow (prefix-value (car (cdr aexp))) (prefix-value (car (cdr (cdr aexp)))))))))

(print '(prefix-value 7))                             ; 7
(print '(prefix-value '(plus 3 4)))                   ; 7
(print '(prefix-value '(plus (times 3 2) (pow 4 2)))) ; 22
(print '(prefix-value '(times 5 (plus 2 3))))         ; 25

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(print '(1st-sub-exp '(plus 3 4)))                   ; 3
(print '(1st-sub-exp '(plus (times 3 2) (pow 4 2)))) ; (times 3 2)
(print '(1st-sub-exp '(times 5 (plus 2 3))))         ; 5

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(print '(2nd-sub-exp '(plus 3 4)))                   ; 4
(print '(2nd-sub-exp '(plus (times 3 2) (pow 4 2)))) ; (pow 4 2)
(print '(2nd-sub-exp '(times 5 (plus 2 3))))         ; (plus 2 3)

(define operator
  (lambda (aexp)
    (car aexp)))

(print '(operator '(plus 3 4)))                    ; plus
(print '(operator '(plus (times 3 2) (pow 4 2))))  ; plus
(print '(operator '(times 5 (plus 2 3))))          ; times

(define prefix-value
  (lambda (aexp)
    (cond
      ((number? aexp) aexp)
      ((eq? (operator aexp) 'plus) (add (prefix-value (1st-sub-exp aexp)) (prefix-value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) 'times) (times (prefix-value (1st-sub-exp aexp)) (prefix-value (2nd-sub-exp aexp))))
      (else (pow (prefix-value (1st-sub-exp aexp)) (prefix-value (2nd-sub-exp aexp)))))))

(print '(prefix-value '(plus 3 4)))                   ; 7
(print '(prefix-value '(plus (times 3 2) (pow 4 2)))) ; 22
(print '(prefix-value '(times 5 (plus 2 3))))         ; 25

(define 1st-infix-sub-exp
  (lambda (nexp)
    (car nexp)))

(print '(1st-infix-sub-exp '(3 plus 5)))                   ; 3
(print '(1st-infix-sub-exp '(8 times 10)))                 ; 8
(print '(1st-infix-sub-exp '(8 times (10 plus 1))))        ; 8
(print '(1st-infix-sub-exp '(2 plus (3 times (2 pow 2))))) ; 2

(define 2nd-infix-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(print '(2nd-infix-sub-exp '(3 plus 5)))                   ; 5
(print '(2nd-infix-sub-exp '(8 times 10)))                 ; 10
(print '(2nd-infix-sub-exp '(8 times (10 plus 1))))        ; (10 plus 1)
(print '(2nd-infix-sub-exp '(2 plus (3 times (2 pow 2))))) ; (3 times (2 pow 2))

(define infix-operator
  (lambda (aexp)
    (car (cdr aexp))))

(print '(infix-operator '(3 plus 5)))                    ; plus
(print '(infix-operator '(8 times 10)))                  ; times
(print '(infix-operator '(8 times (10 plus 1))))         ; times
(print '(infix-operator '(2 plus (3 times (2 pow 2)))))  ; plus

(define infix-value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (infix-operator nexp) 'plus)
        (add
          (infix-value (1st-infix-sub-exp nexp))
          (infix-value (2nd-infix-sub-exp nexp))))
      ((eq? (infix-operator nexp) 'times)
        (times
          (infix-value (1st-infix-sub-exp nexp))
          (infix-value (2nd-infix-sub-exp nexp))))
      (else
        (pow
          (infix-value (1st-infix-sub-exp nexp))
          (infix-value (2nd-infix-sub-exp nexp)))))))

(print '(infix-value 3))                             ; 3
(print '(infix-value '(3 plus 5)))                   ; 8
(print '(infix-value '(8 times 10)))                 ; 80
(print '(infix-value '(8 times (10 plus 1))))        ; 88
(print '(infix-value '(2 plus (3 times (2 pow 2))))) ; 14

; The Eighth Commandment
;
;   Use help functions to abstract fom representations

(define zro?
  (lambda (n)
    (null? n)))

(print '(zro? '())) ; true
(print '(zro? '(()))) ; false

(define pljusmek
  (lambda (n)
    (cons '() n)))

(print '(pljusmek '(()))) ; (() ())

(define minusmek
  (lambda (n)
    (cdr n)))

(print '(minusmek '(() ()))) ; (())

(define goomar
  (lambda (a b)
    (cond
      ((zro? b) a)
      (else (pljusmek (goomar a (minusmek b)))))))

(print '(goomar '(()) '(() ())))  ; (() () ())


;;;;;;; Friends and Relations

(section "Friends and Relations")

(define member-via-equal?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a) (member? a (cdr lat)))))))

(print '(member-via-equal? 'd '(a d g))) ; true
(print '(member-via-equal? 'd '(a g)))   ; false

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member-via-equal? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(print '(set? '()))      ; true
(print '(set? '(a b c))) ; true
(print '(set? '(a b b))) ; false

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(print '(makeset '()))         ; '()
(print '(makeset '(a b a c)))  ; (a b c)
(print '(makeset '(a b c)))    ; (a b c)

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member-via-equal? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(print '(subset? '() '(a b c)))               ; true
(print '(subset? '(a b c) '(a aa b bb c cc))) ; true
(print '(subset? '(a b c) '()))               ; false
(print '(subset? '(d e f) '(a aa b bb c cc))) ; false

(define subset-via-and?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member-via-equal? (car set1) set2) (subset-via-and? (cdr set1) set2))))))

(print '(subset-via-and? '() '(a b c)))               ; true
(print '(subset-via-and? '(a b c) '(a aa b bb c cc))) ; true
(print '(subset-via-and? '(a b c) '()))               ; false
(print '(subset-via-and? '(d e f) '(a aa b bb c cc))) ; false

(define eqset?
  (lambda (set1 set2)
    (and (subset? set2 set1) (subset? set1 set2))))

(print '(eqset? '(a b) '(a b)))   ; true
(print '(eqset? '() '(a b)))      ; false
(print '(eqset? '(a b) '()))      ; false
(print '(eqset? '(a b) '(a b c))) ; false

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member-via-equal? (car set1) set2) (intersect? (cdr set1) set2))))))

(print '(intersect? '(a b c) '(c d e))) ; true
(print '(intersect? '() '(c d e)))      ; false
(print '(intersect? '(a b c) '(d e f))) ; false

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member-via-equal? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(print '(intersect '(a b c) '(c d e))) ; (c)
(print '(intersect '() '(c d e)))      ; ()
(print '(intersect '(c d e) '()))      ; ()
(print '(intersect '(a b c) '(d e f))) ; ()

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member-via-equal? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(print '(union '() '()))      ; ()
(print '(union '() '(a b c))) ; (a b c)
(print '(union '(a b c) '())) ; (a b c)
(print '(union '(a b) '(c)))  ; (a b c)

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member-via-equal? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(print '(difference '() '()))      ; ()
(print '(difference '() '(a b c))) ; ()
(print '(difference '(a b c) '())) ; (a b c)
(print '(difference '(a b) '(c)))  ; (a b)

(define intersect-all
  (lambda (l-set)
    (cond
      ((null? l-set) '())
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersect-all (cdr l-set)))))))

(print '(intersect-all '()))                        ; ()
(print '(intersect-all '((a b c))))                 ; (a b c)
(print '(intersect-all '((a b c) (a f g))))         ; (a)
(print '(intersect-all '((a b c) (a f g) (a r t)))) ; (a)
(print '(intersect-all '((a b c) (d e f) (g h i)))) ; ()
