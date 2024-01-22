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

(print
  '(atom? 'a))          ; true
(print
  '(atom? '(f (g h))))  ; false

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

(print
  '(lat? '()))          ; true
(print
  '(lat? '((t g) r)))   ; false

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(print
  '(member? 'd '(a d g))) ; true
(print
  '(member? 'd '(a g)))   ; false

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

(print
  '(rember 'a '(a b c)))      ; (b c)
(print
  '(rember 'a '(a b a c)))    ; (b a c)
(print
  '(rember 'a '(b a g a c)))  ; (b g a c)
(print
  '(rember 'a '(f g)))        ; (f g)
(print
  '(rember 'a '()))           ; ()

; The Second Commandment
;
;   Use `cons` to build lists.

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(print
  '(firsts '()))                      ; ()
(print
  '(firsts '((a b) (c d) (e f))))     ; (a c e)
(print
  '(firsts '(((a b) c) (d e) (f))))   ; ((a b) d f)

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(print
  '(seconds '()))                             ; ()
(print
  '(seconds '((a b) (c d) (e f))))            ; (b d f)
(print
  '(seconds '(((a b) c) (d e) (f (g h)))))    ; (c e (g h)))

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

(print
  '(insertR 'c 'b '(a b d)))     ; (a b c d)
(print
  '(insertR 'c 'b '(a b b d)))   ; (a b c b d)
(print
  '(insertR 'c 'b '()))          ; ()

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))))

(print
  '(insertL 'c 'b '(a b d)))     ; (a c b d)
(print
  '(insertL 'c 'b '(a b b d)))   ; (a c b b d)
(print
  '(insertL 'c 'b '()))          ; ()

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat)))))))))

(print
  '(subst 'c 'b '(a b d)))     ; (a c d)
(print
  '(subst 'c 'b '(a b b d)))   ; (a c b d)
(print
  '(subst 'c 'b '()))          ; ()

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

(print
  '(subst2 'c 'b 'd '(a b d)))     ; (a c d)
(print
  '(subst2 'c 'b 'd '(a d b)))     ; (a c b)
(print
  '(subst2 'c 'b 'd '(a b b d)))   ; (a c b d)
(print
  '(subst2 'c 'b 'd '()))          ; ()

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(print
  '(multirember 'a '(a b c)))     ; (b c)
(print
  '(multirember 'a '(a b a c)))   ; (b c)
(print
  '(multirember 'a '(b a g a c))) ; (b g c)
(print
  '(multirember 'a '(f g)))       ; (f g)
(print
  '(multirember 'a '()))          ; ()

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(print
  '(multiinsertR 'c 'b '(a b d)))     ; (a b c d)
(print
  '(multiinsertR 'c 'b '(a b b d)))   ; (a b c b c d)
(print
  '(multiinsertR 'c 'b '()))          ; ()

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(print
  '(multiinsertL 'c 'b '(a b d)))     ; (a c b d)
(print
  '(multiinsertL 'c 'b '(a b b d)))   ; (a c b c b d)
(print
  '(multiinsertL 'c 'b '()))          ; ()

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(print
  '(multisubst 'c 'b '(a b d)))     ; (a c d)
(print
  '(multisubst 'c 'b '(a b b d)))   ; (a c c d)
(print
  '(multisubst 'c 'b '()))          ; ()

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

(print
  '(add1 5))  ; 6

(define sub1
  (lambda (n) (- n 1)))

(print
  '(sub1 5))  ; 4

(define add
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add (add1 a) (sub1 b))))))

(print
  '(add 46 12)) ; 58

(define sub
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub (sub1 a) (sub1 b))))))

(print
  '(sub 14 3))  ; 11

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

(print
  '(addtup '(1 2 3 4))) ; 10
(print
  '(addtup '()))        ; 0

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

(print
  '(times 13 4))  ; 52
(print
  '(times 13 0))  ; 0
(print
  '(times 0 13))  ; 0
(print
  '(times 1 13))  ; 13
(print
  '(times 13 1))  ; 13

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

(print
  '(tup+ '(3 6 9 11 4) '(8 5 2 0 7))) ; (11 11 11 11 11)
(print
  '(tup+ '(3 7) '(4 6 8 1)))          ; (7 13 8 1)
(print
  '(tup+ '(3 7 8 1) '(4 6)))          ; (7 13 8 1)
(print
  '(tup+ '() '()))                    ; ()

(define gt
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (gt (sub1 m) (sub1 n))))))

(print
  '(gt 12 120)) ; false
(print
  '(gt 12 12))  ; false
(print
  '(gt 120 12)) ; true

(define lt
  (lambda (m n)
    (gt n m)))

(print
  '(lt 4 44)) ; true
(print
  '(lt 2 2))  ; false
(print
  '(lt 44 4)) ; false

(define equals?
  (lambda (m n)
    (cond
      ((or (gt m n) (lt m n)) #f)
      (else #t))))

(print
  '(equals? 4 44))  ; false
(print
  '(equals? 44 4))  ; false
(print
  '(equals? 4 4))   ; true

(define pow
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (times m (pow m (sub1 n)))))))

(print
  '(pow 1 1)) ; 1
(print
  '(pow 2 3)) ; 8
(print
  '(pow 5 3)) ; 125

(define div
  (lambda (m n)
    (cond
      ((zero? n) '())
      ((lt m n) 0)
      (else (add1 (div (sub m n) n))))))

(print
  '(div 15 4))  ; 3
(print
  '(div 15 0))  ; ()

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(print
  '(length '(a b c d))) ; 4
(print
  '(length '()))        ; 0

(define pick
  (lambda (index lat)
    (cond
      ((zero? (sub1 index)) (car lat))
      ((zero? index) '())
      (else (pick (sub1 index) (cdr lat))))))

(print
  '(pick 3 '(a b c d e))) ; c
(print
  '(pick 0 '(a)))         ; ()

(define rempick
  (lambda (index lat)
    (cond
      ((lt (length lat) index) '())
      ((null? lat) '())
      ((zero? (sub1 index)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 index) (cdr lat)))))))

(print
  '(rempick 3 '(a b c d e)))  ; (a b d e)
(print
  '(rempick 9 '(a b c d e)))  ; ()
(print
  '(rempick 2 '()))           ; ()

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(print
  '(no-nums '(5 pears 6 prunes 9 dates))) ; (pears prunes dates)
(print
  '(no-nums '(5 6 7)))                    ; ()
(print
  '(no-nums '()))                         ; ()

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(print
  '(all-nums '(5 pears 6 prunes 9 dates)))  ; (5 6 9)
(print
  '(all-nums '(5 6 7)))                     ; (5 6 7)
(print
  '(all-nums '()))                          ; ()

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (equals? a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      (else #f))))

(print
  '(eqan? 'a 1))  ; false
(print
  '(eqan? 1 'a))  ; false
(print
  '(eqan? 1 3))   ; false
(print
  '(eqan? 'a 'b)) ; false
(print
  '(eqan? 'a 'a)) ; true
(print
  '(eqan? 1 1))   ; true

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(print
  '(occur 1 '(1 a 1 b 3 f 1)))  ; 3
(print
  '(occur 1 '()))               ; 0
(print
  '(occur 1 '(a b c d f)))      ; 0

(define one?
  (lambda (n)
    (eqan? n 1)))

(print
  '(one? 1))    ; true
(print
  '(one? 'one)) ; false
(print
  '(one? 0))    ; false

(define rempick-via-one
  (lambda (index lat)
    (cond
      ((lt (length lat) index) '())
      ((null? lat) '())
      ((one? index) (cdr lat))
      (else (cons (car lat) (rempick-via-one (sub1 index) (cdr lat)))))))

(print
  '(rempick-via-one 3 '(a b c d e)))  ; (a b d e)
(print
  '(rempick-via-one 9 '(a b c d e)))  ; '()
(print
  '(rempick-via-one 2 '()))           ; '()


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

(print
  '(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)))                ; ((coffee) ((tea)) (and (hick)))
(print
  '(rember* 'sauce '(((tomato) sauce) ((bean) sauce) (and ((flying) sauce))))) ; (((tomato)) ((bean)) (and ((flying))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(print
  '(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood)))
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

(print
  '(occur* 'a '(a ((b) a) (c d (a) a) (a))))  ; 5
(print
  '(occur* 'a '()))                           ; 0
(print
  '(occur* 'a '(b (c d (e (f)) g))))          ; 0

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(print
  '(subst* 'b 'a '(a (((a) b) c) (b (a) ((a) c))))) ; (b (((b) b) c) (b (b) ((b) c)))
(print
  '(subst* 'b 'a '()))                              ; '()
(print
  '(subst* 'b 'a '(b (c (d (e) f) g) h)))           ; (b (c (d (e) f) g) h)

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eqan? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons old (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(print
  '(insertL* 'b 'a '(a (((a) b) c) (b (a) ((a) c))))) ; (b a (((b a) b) c) (b (b a) ((b a) c)))
(print
  '(insertL* 'b 'a '()))                              ; '()
(print
  '(insertL* 'b 'a '(b (c (d (e) f) g) h)))           ; (b (c (d (e) f) g) h)

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (cond
          ((eqan? a (car l)) #t)
          (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(print
  '(member* 'a '(a (((a) b) c) (b (a) ((a) c))))) ; true
(print
  '(member* 'a '()))                              ; false
(print
  '(member* 'a '(b (c (d (e) f) g) h)))           ; false

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(print
  '(leftmost '()))              ; '()
(print
  '(leftmost '(a b c)))         ; a
(print
  '(leftmost '((a (b)) c)))     ; a
(print
  '(leftmost '('() (a (b)) c))) ; '()

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

(print
  '(eqlist? '(a (b)) '((a) b))) ; false
(print
  '(eqlist? '(a (b)) '(a (b)))) ; true
(print
  '(eqlist? '(a b) '(a b)))     ; true
(print
  '(eqlist? '() '()))           ; true

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

(print
  '(eqlist? '(a (b)) '((a) b))) ; false
(print
  '(eqlist? '(a (b)) '(a (b)))) ; true
(print
  '(eqlist? '(a b) '(a b)))     ; true
(print
  '(eqlist? '() '()))            ; true

(define equal?
  (lambda (sexp1 sexp2)
    (cond
      ((and (atom? sexp1) (atom? sexp2)) (eqan? sexp1 sexp2))
      ((or (atom? sexp1) (atom? sexp2)) #f)
      (else (eqlist? sexp1 sexp2)))))

(print
  '(equal? '() '()))            ; true
(print
  '(equal? 'a 'a))              ; true
(print
  '(equal? '(a b) '(a b)))      ; true
(print
  '(equal? '() 'a))             ; false
(print
  '(equal? 'a 'b))              ; false
(print
  '(equal? '(a (b)) '((a) b)))  ; false

; The Sixth Commandment
;
;   Simplify only after the function is correct.

(define rember-simplified
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember-simplified s (cdr l)))))))

(print
  '(rember-simplified 'a '(a b c)))      ; (b c)
(print
  '(rember-simplified 'a '(a b a c)))    ; (b a c)
(print
  '(rember-simplified 'a '(b a g a c)))  ; (b g a c)
(print
  '(rember-simplified 'a '(f g)))        ; (f g)
(print
  '(rember-simplified 'a '()))           ; '()


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

(print
  '(numbered? 4))                               ; true
(print
  '(numbered? '(3 plus (4 pow (5 times 2)))))   ; true
(print
  '(numbered? '()))                             ; true
(print
  '(numbered? '(3 what? (4 pow (5 times 2)))))  ; false

(define numbered?-per-book
  (lambda (x)
    (cond
      ((atom? x) (number? x))
      (else (and (numbered?-per-book (car x)) (numbered?-per-book (car (cdr (cdr x)))))))))

(print
  '(numbered?-per-book 4))                                ; true
(print
  '(numbered?-per-book '(3 plus (4 pow (5 times 2)))))    ; true
; (numbered?-per-book '())                            ; can't test, since we don't consider empty lists
(print
  '(numbered?-per-book '(3 what? (4 pow (5 times 2)))))   ; false

(define value
  (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      ((and (list? nexp) (eq? 1 (length nexp))) (value (car nexp)))
      ((eq? (car (cdr nexp)) 'plus) (add (value (car nexp)) (value (cdr (cdr nexp)))))
      ((eq? (car (cdr nexp)) 'times) (times (value (car nexp)) (value (cdr (cdr nexp)))))
      (else (pow (value (car nexp)) (value (cdr (cdr nexp))))))))

(print
  '(value 3))                             ; 3
(print
  '(value '(3 plus 5)))                   ; 8
(print
  '(value '(8 times 10)))                 ; 80
(print
  '(value '(8 times (10 plus 1))))        ; 88
(print
  '(value '(2 plus (3 times (2 pow 2))))) ; 14

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

(print
  '(prefix-value 7))                             ; 7
(print
  '(prefix-value '(plus 3 4)))                   ; 7
(print
  '(prefix-value '(plus (times 3 2) (pow 4 2)))) ; 22
(print
  '(prefix-value '(times 5 (plus 2 3))))         ; 25

(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))

(print
  '(1st-sub-exp '(plus 3 4)))                   ; 3
(print
  '(1st-sub-exp '(plus (times 3 2) (pow 4 2)))) ; (times 3 2)
(print
  '(1st-sub-exp '(times 5 (plus 2 3))))         ; 5

(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(print
  '(2nd-sub-exp '(plus 3 4)))                   ; 4
(print
  '(2nd-sub-exp '(plus (times 3 2) (pow 4 2)))) ; (pow 4 2)
(print
  '(2nd-sub-exp '(times 5 (plus 2 3))))         ; (plus 2 3)

(define operator
  (lambda (aexp)
    (car aexp)))

(print
  '(operator '(plus 3 4)))                    ; plus
(print
  '(operator '(plus (times 3 2) (pow 4 2))))  ; plus
(print
  '(operator '(times 5 (plus 2 3))))          ; times

(define prefix-value
  (lambda (aexp)
    (cond
      ((number? aexp) aexp)
      ((eq? (operator aexp) 'plus) (add (prefix-value (1st-sub-exp aexp)) (prefix-value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) 'times) (times (prefix-value (1st-sub-exp aexp)) (prefix-value (2nd-sub-exp aexp))))
      (else (pow (prefix-value (1st-sub-exp aexp)) (prefix-value (2nd-sub-exp aexp)))))))

(print
  '(prefix-value '(plus 3 4)))                   ; 7
(print
  '(prefix-value '(plus (times 3 2) (pow 4 2)))) ; 22
(print
  '(prefix-value '(times 5 (plus 2 3))))         ; 25

(define 1st-infix-sub-exp
  (lambda (nexp)
    (car nexp)))

(print
  '(1st-infix-sub-exp '(3 plus 5)))                   ; 3
(print
  '(1st-infix-sub-exp '(8 times 10)))                 ; 8
(print
  '(1st-infix-sub-exp '(8 times (10 plus 1))))        ; 8
(print
  '(1st-infix-sub-exp '(2 plus (3 times (2 pow 2))))) ; 2

(define 2nd-infix-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(print
  '(2nd-infix-sub-exp '(3 plus 5)))                   ; 5
(print
  '(2nd-infix-sub-exp '(8 times 10)))                 ; 10
(print
  '(2nd-infix-sub-exp '(8 times (10 plus 1))))        ; (10 plus 1)
(print
  '(2nd-infix-sub-exp '(2 plus (3 times (2 pow 2))))) ; (3 times (2 pow 2))

(define infix-operator
  (lambda (aexp)
    (car (cdr aexp))))

(print
  '(infix-operator '(3 plus 5)))                    ; plus
(print
  '(infix-operator '(8 times 10)))                  ; times
(print
  '(infix-operator '(8 times (10 plus 1))))         ; times
(print
  '(infix-operator '(2 plus (3 times (2 pow 2)))))  ; plus

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

(print
  '(infix-value 3))                             ; 3
(print
  '(infix-value '(3 plus 5)))                   ; 8
(print
  '(infix-value '(8 times 10)))                 ; 80
(print
  '(infix-value '(8 times (10 plus 1))))        ; 88
(print
  '(infix-value '(2 plus (3 times (2 pow 2))))) ; 14

; The Eighth Commandment
;
;   Use help functions to abstract fom representations

(define zro?
  (lambda (n)
    (null? n)))

(print
  '(zro? '())) ; true
(print
  '(zro? '(()))) ; false

(define pljusmek
  (lambda (n)
    (cons '() n)))

(print
  '(pljusmek '(()))) ; (() ())

(define minusmek
  (lambda (n)
    (cdr n)))

(print
  '(minusmek '(() ()))) ; (())

(define goomar
  (lambda (a b)
    (cond
      ((zro? b) a)
      (else (pljusmek (goomar a (minusmek b)))))))

(print
  '(goomar '(()) '(() ())))  ; (() () ())


;;;;;;; Friends and Relations

(section "Friends and Relations")

(define member-via-equal?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a) (member? a (cdr lat)))))))

(print
  '(member-via-equal? 'd '(a d g))) ; true
(print
  '(member-via-equal? 'd '(a g)))   ; false

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member-via-equal? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(print
  '(set? '()))      ; true
(print
  '(set? '(a b c))) ; true
(print
  '(set? '(a b b))) ; false

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(print
  '(makeset '()))         ; '()
(print
  '(makeset '(a b a c)))  ; (a b c)
(print
  '(makeset '(a b c)))    ; (a b c)

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member-via-equal? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(print
  '(subset? '() '(a b c)))               ; true
(print
  '(subset? '(a b c) '(a aa b bb c cc))) ; true
(print
  '(subset? '(a b c) '()))               ; false
(print
  '(subset? '(d e f) '(a aa b bb c cc))) ; false

(define subset-via-and?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member-via-equal? (car set1) set2) (subset-via-and? (cdr set1) set2))))))

(print
  '(subset-via-and? '() '(a b c)))               ; true
(print
  '(subset-via-and? '(a b c) '(a aa b bb c cc))) ; true
(print
  '(subset-via-and? '(a b c) '()))               ; false
(print
  '(subset-via-and? '(d e f) '(a aa b bb c cc))) ; false

(define eqset?
  (lambda (set1 set2)
    (and (subset? set2 set1) (subset? set1 set2))))

(print
  '(eqset? '(a b) '(a b)))   ; true
(print
  '(eqset? '() '(a b)))      ; false
(print
  '(eqset? '(a b) '()))      ; false
(print
  '(eqset? '(a b) '(a b c))) ; false

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member-via-equal? (car set1) set2) (intersect? (cdr set1) set2))))))

(print
  '(intersect? '(a b c) '(c d e))) ; true
(print
  '(intersect? '() '(c d e)))      ; false
(print
  '(intersect? '(a b c) '(d e f))) ; false

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member-via-equal? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(print
  '(intersect '(a b c) '(c d e))) ; (c)
(print
  '(intersect '() '(c d e)))      ; ()
(print
  '(intersect '(c d e) '()))      ; ()
(print
  '(intersect '(a b c) '(d e f))) ; ()

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member-via-equal? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(print
  '(union '() '()))      ; ()
(print
  '(union '() '(a b c))) ; (a b c)
(print
  '(union '(a b c) '())) ; (a b c)
(print
  '(union '(a b) '(c)))  ; (a b c)

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member-via-equal? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(print
  '(difference '() '()))      ; ()
(print
  '(difference '() '(a b c))) ; ()
(print
  '(difference '(a b c) '())) ; (a b c)
(print
  '(difference '(a b) '(c)))  ; (a b)

(define intersect-all
  (lambda (l-set)
    (cond
      ((null? l-set) '())
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersect-all (cdr l-set)))))))

(print
  '(intersect-all '()))                        ; ()
(print
  '(intersect-all '((a b c))))                 ; (a b c)
(print
  '(intersect-all '((a b c) (a f g))))         ; (a)
(print
  '(intersect-all '((a b c) (a f g) (a r t)))) ; (a)
(print
  '(intersect-all '((a b c) (d e f) (g h i)))) ; ()

(define pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (cdr l)) #f)
      ((null? (cdr (cdr l))) #t)
      (else #f))))

(print
  '(pair? 'a))         ; false
(print
  '(pair? '(a)))       ; false
(print
  '(pair? '()))        ; false
(print
  '(pair? '(a b c)))   ; false
(print
  '(pair? '(a b (c)))) ; false
(print
  '(pair? '(a b)))     ; true
(print
  '(pair? '(a (c))))   ; true
(print
  '(pair? '((a) c)))   ; true

(define first
  (lambda (p)
    (cond
      ((null? p) '())
      (else (car p)))))

(print
  '(first '()))    ; ()
(print
  '(first '(a b))) ; a

(define second
  (lambda (p)
    (cond
      ((null? p) '())
      ((null? (cdr p)) '())
      (else (car (cdr p))))))

(print
  '(second '()))     ; ()
(print
  '(second '(a)))    ; ()
(print
  '(second '(a b)))  ; b

(define build (lambda (f s) (cons f (cons s '()))))

(print
  '(build 'a 'b)) ; (a b)

(define third
  (lambda (p)
    (cond
      ((or
        (null? p)
        (or (null? (cdr p))
            (null? (cdr (cdr p))))) '())
      (else (car (cdr (cdr p)))))))

(print
  '(third '()))      ; ()
(print
  '(third '(a)))     ; ()
(print
  '(third '(a b)))   ; ()
(print
  '(third '(a b c))) ; c

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(print
  '(fun? '((a 1) (b 2) (c 3)))) ; true
(print
  '(fun? '((a 1) (b 2) (a 3)))) ; false

(define revrel
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (build (second (car l)) (first (car l))) (revrel (cdr l)))))))

(print
  '(revrel '((a 1) (b 2) (c 3))))  ; ((1 a) (2 b) (3 c))
(print
  '(revrel '((a 1) (b 2) (c 3))))  ; ((1 a) (2 b) (3 c))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(print
  '(revpair '(a 1)))  ; (1 a)

(define revrel-via-revpair
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (revpair (car l)) (revrel-via-revpair (cdr l)))))))

(print
  '(revrel-via-revpair '((a 1) (b 2) (c 3))))  ; ((1 a) (2 b) (3 c))

(define fullfun?
  (lambda (f)
    (fun? (revrel-via-revpair f))))

(print
  '(fullfun? '((a 1) (b 2) (c 3)))) ; true
(print
  '(fullfun? '((a 1) (b 2) (c 1)))) ; true
(print
  '(fullfun? '((a 1) (b 2) (a 3)))) ; false

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (second (first l)) (seconds (cdr l)))))))

(print
  '(seconds '()))                    ; ()
(print
  '(seconds '((a b) (c d) (e f))))   ; (b d f)
(print
  '(seconds '(((a b) c) (d e) (f)))) ; ((a b) d ())


;;;;;;; Friends and Relations

(section "Lambda the Ultimate")

(define rember-f
  (lambda (test? a lat)
    (cond
      ((null? lat) '())
      ((test? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember-f test? a (cdr lat)))))))

(print
  '(rember-f eq? 'a '(a b c)))      ; (b c)
(print
  '(rember-f eq? 'a '(a b a c)))    ; (b a c)
(print
  '(rember-f eq? 'a '(b a g a c)))  ; (b g a c)
(print
  '(rember-f eq? 'a '(f g)))        ; (f g)
(print
  '(rember-f eq? 'a '()))           ; ()

(define rember-fc
  (lambda (test?)
    (lambda (a lat)
      (cond
      ((null? lat) '())
      ((test? a (car lat)) (cdr lat))
      (else (cons (car lat) ((rember-fc test?) a (cdr lat))))))))

(print
  '((rember-fc eq?) 'a '(a b c)))      ; (b c)
(print
  '((rember-fc eq?) 'a '(a b a c)))    ; (b a c)
(print
  '((rember-fc eq?) 'a '(b a g a c)))  ; (b g a c)
(print
  '((rember-fc eq?) 'a '(f g)))        ; (f g)
(print
  '((rember-fc eq?) 'a '()))           ; ()

(define insertR-fc
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
          ((test? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat) ((insertR-fc test?) new old (cdr lat))))))))))

(print
  '((insertR-fc eq?) 'c 'b '(a b d)))     ; (a b c d)
(print
  '((insertR-fc eq?) 'c 'b '(a b b d)))   ; (a b c b d)
(print
  '((insertR-fc eq?) 'c 'b '()))          ; ()

(define insertL-fc
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
          ((test? (car lat) old) (cons new lat))
          (else (cons (car lat) ((insertL-fc test?) new old (cdr lat))))))))))

(print
  '((insertL-fc eq?) 'c 'b '(a b d)))     ; (a c b d)
(print
  '((insertL-fc eq?) 'c 'b '(a b b d)))   ; (a c b b d)
(print
  '((insertL-fc eq?) 'c 'b '()))          ; ()

(define insert-g
  (lambda (inserter)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
          ((eq? (car lat) old) (inserter new old lat))
          (else (cons (car lat) ((insert-g inserter) new old (cdr lat))))))))))

(define left-inserter
  (lambda (new old lat)
    (cons new lat)))

(define right-inserter
  (lambda (new old lat)
    (cons old (cons new (cdr lat)))))

(print
  '((insert-g right-inserter) 'c 'b '(a b d)))   ; (a b c d)
(print
  '((insert-g right-inserter) 'c 'b '(a b b d))) ; (a b c b d)
(print
  '((insert-g right-inserter) 'c 'b '()))        ; ()
(print
  '((insert-g left-inserter) 'c 'b '(a b d)))    ; (a c b d)
(print
  '((insert-g left-inserter) 'c 'b '(a b b d)))  ; (a c b b d)
(print
  '((insert-g left-inserter) 'c 'b '()))         ; ()

(define insertR-via-insertg (insert-g right-inserter))

(print
  '(insertR-via-insertg 'c 'b '(a b d)))     ; (a b c d)
(print
  '(insertR-via-insertg 'c 'b '(a b b d)))   ; (a b c b d)
(print
  '(insertR-via-insertg 'c 'b '()))          ; ()

(define insertL-via-insertg (insert-g left-inserter))

(print
  '(insertL-via-insertg 'c 'b '(a b d)))     ; (a c b d)
(print
  '(insertL-via-insertg 'c 'b '(a b b d)))   ; (a c b b d)
(print
  '(insertL-via-insertg 'c 'b '()))          ; ()

(define replace
  (lambda (new old lat)
    (cons new (cdr lat))))


(define subst-via-insertg (insert-g replace))

(print
  '(subst 'c 'b '(a b d)))     ; (a c d)
(print
  '(subst 'c 'b '(a b b d)))   ; (a c b d)
(print
  '(subst 'c 'b '()))          ; ()

; The Ninth Commandment
;
;   Abstract common patterns with a new function

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a 'plus) add)
      ((eq? a 'times) times)
      (else pow))))

(define prefix-value-via-atf
  (lambda (aexp)
    (cond
      ((number? aexp) aexp)
      (else
        ((atom-to-function (operator aexp))
          (prefix-value-via-atf (1st-sub-exp aexp))
          (prefix-value-via-atf (2nd-sub-exp aexp)))))))

(print
  '(prefix-value-via-atf '(plus 3 4)))                    ; 7
(print
  '(prefix-value-via-atf '(plus (times 3 2) (pow 4 2))))  ; 22
(print
  '(prefix-value-via-atf '(times 5 (plus 2 3))))          ; 25

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(print
  '((multirember-f eq?) 'a '(a b c)))     ; (b c)
(print
  '((multirember-f eq?) 'a '(a b a c)))   ; (b c)
(print
  '((multirember-f eq?) 'a '(b a g a c))) ; (b g c)
(print
  '((multirember-f eq?) 'a '(f g)))       ; (f g)
(print
  '((multirember-f eq?) 'a '()))          ; ()

(define multirember-eq? (multirember-f eq?))

(print
  '(multirember-eq? 'a '(a b c)))     ; (b c)
(print
  '(multirember-eq? 'a '(a b a c)))   ; (b c)
(print
  '(multirember-eq? 'a '(b a g a c))) ; (b g c)
(print
  '(multirember-eq? 'a '(f g)))       ; (f g)
(print
  '(multirember-eq? 'a '()))          ; ()

(define eq?-c
  (lambda (a)
    (lambda (b)
      (eq? a b))))

(define eq?-tuna (eq?-c 'tuna))

(print
  '(eq?-tuna 'tuna)) ; true
(print
  '(eq?-tuna 'anut)) ; false

(define multirember-T
  (lambda (test?)
    (lambda (lat)
      (cond
        ((null? lat) '())
        ((test? (car lat)) ((multirember-T test?) (cdr lat)))
        (else (cons (car lat) ((multirember-T test?) (cdr lat))))))))

(print
  '((multirember-T eq?-tuna) '(shrimp salad tuna saland and tuna)))
; (shrimp salad salad and)

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
        (multirember&co
          a (cdr lat)
          (lambda (newlat seen)
            (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co
          a (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(print
  '(multirember&co 'tuna '(strawberries tuna and sandwich) a-friend))  ; false

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

(define last-friend
  (lambda (x y)
    (length x)))

(print
  '(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))  ; 3

; The Tenth Commandment
;
;   Build functions to collect more than one value at a time

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
        (cons new
          (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
        (cons oldR
          (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(print
  '(multiinsertLR 'c 'b 'd '(a b d)))  ; (a c b d c)
(print
  '(multiinsertLR 'c 'b 'd '()))  ; ()

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
        (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))))

(define the-last-friend
  (lambda (a b c)
    (cons a (cons b (cons c '())))))

(print
  '(multiinsertLR&co 'salty 'fish 'chips '() the-last-friend))  ; (() 0 0)
(print
  '(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) the-last-friend))
; ((chips salty and salty fish of salty fish and chips salty) 2 2)

(define even?
  (lambda (n)
    (= (* (div n 2) 2) n)))

(print
  '(even? 0))  ; #t
(print
  '(even? 1))  ; #f
(print
  '(even? 2))  ; #t

(define evens-only*
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
        (cond
          ((even? (car lat)) (cons (car lat) (evens-only* (cdr lat))))
          (else (evens-only* (cdr lat)))))
      (else (cons (evens-only* (car lat)) (evens-only* (cdr lat)))))))

(print
  '(evens-only* '()))  ; ()
(print
  '(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))  ; ((2 8) 10 (() 6) 2)

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (evens-only*&co (cdr l)
              (lambda (evens even-product odd-sum)
                (col (cons (car l) evens) (times (car l) even-product) odd-sum))))
          (else
            (evens-only*&co (cdr l)
              (lambda (evens even-product odd-sum)
                (col evens even-product (add (car l) odd-sum)))))))
      (else (evens-only*&co (car l)
        (lambda (a-evens a-even-product a-odd-sum)
          (evens-only*&co (cdr l)
            (lambda (d-evens d-even-product d-odd-sum)
              (col (cons a-evens d-evens) (times a-even-product d-even-product) (add a-odd-sum d-odd-sum))))))))))

(print
  '(evens-only*&co '() the-last-friend))  ; (() 1 0)
(print
  '(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend))  ; (((2 8) 10 (() 6) 2) 38 1920)


;;;;;;; ... and Again, and Again, and Again, ...

(section "... and Again, and Again, and Again, ...")


(define keep-looking
  (lambda (spec sample-or-index set)
    (cond
      ((number? sample-or-index)
        (keep-looking spec (pick sample-or-index set) set))
      (else (eq? spec sample-or-index)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(print
  '(looking 'caviar '(6 2 4 caviar 5 7 3)))  ; #t
(print
  '(looking 'caviar '(6 2 grits caviar 5 7 3)))  ; #f

; looking is _partial_, because in some cases it won't terminate :O
;
; (print
  ; '(looking 'caviar '(2 1 caviar)))

(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

(print
  '(shift '((a b) c))) ; (a (b c))
(print
  '(shift '((a b) (c d)))) ; (a (b (c d)))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(print
  '(align '((a b) ((c d) e)))) ; (a (b (c (d e))))
(print
  '(align '((((a b) c) d) e))) ; (a (b (c (d e))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (add (length* (first pora)) (length* (second pora)))))))

(print
  '(length* '((a b) c))) ; 3
(print
  '(length '((a b) c))) ; 2

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (add (times 2 (weight* (first pora))) (weight* (second pora)))))))

(print
  '(weight* '((a b) c))) ; 7
(print
  '(weight* '(a (b c)))) ; 5

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

(print
  '(shuffle '((a b) c)))

; (define C
;   (lambda (n)
;     (cond
;       ((one? n) 1)
;       (else
;         (cond
;           ((even? n) (C (div n 2)))
;           (else (add1 (times 3 n))))))))
;
; The function above does not yield a value for 0,
; but otherwise we don't know if it's total.

; Thanks, Lothar Collatz /1910-1990/

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

; Wilhelm Ackerman /1835-1946/

(print
  '(A 0 4))  ; 5
(print
  '(A 1 0))  ; 2
(print
  '(A 1 1))  ; 3
(print
  '(A 2 2))  ; 7

; length = 0

; ((lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
; eternity)

; length < 1

; (lambda (l)
;   (cond
;     ((null? l) 0)
;     (else
;       (add1
;         ((lambda (l)
;           (cond
;             ((null? l) 0)
;             (else (add1 (eternity (cdr l))))))
;         (cdr l))))))

; length < 2

; (lambda (l)
;   (cond
;     ((null? l) 0)
;     (else (add1
;         ((lambda (l)
;           (cond
;             ((null? l) 0)
;             (else (add1
;               ((lambda (l)
;                 (cond
;                   ((null? l) 0)
;                   (else (add1
;                     (eternity (cdr l))))))
;             (cdr l))))))
;         (cdr l))))))

(define eternity (lambda () (eternity)))

; No more define|s below

((lambda (length0)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length0 (cdr l)))))))
  eternity)

((lambda (length-will-be-called-with-the-next-lambda-as-argument)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length-will-be-called-with-the-next-lambda-as-argument (cdr l)))))))
 ((lambda (length-im-the-aforementioned-argument-and-will-eat-eternity)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length-im-the-aforementioned-argument-and-will-eat-eternity (cdr l)))))))
  eternity))

((lambda (f)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
  ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  ((lambda (h)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (h (cdr l)))))))
  eternity)))

((lambda (mk-length0)
  (mk-length0 eternity))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

((lambda (mk-length1)
  (mk-length1 (mk-length1 eternity)))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

((lambda (mk-length3)
  (mk-length3
    (mk-length3
      (mk-length3
        (mk-length3 eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

((lambda (mk-length)
  (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))

(print
  '(((lambda (mk-length)
    (mk-length mk-length))
    (lambda (mk-length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 ((mk-length eternity) (cdr l))))))))
  '(apple)))  ; 1

((lambda (mk-length)
    (mk-length mk-length))
    (lambda (mk-length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 ((mk-length mk-length) (cdr l))))))))

(print
  '(((lambda (mk-length)
    (mk-length mk-length))
    (lambda (mk-length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 ((mk-length mk-length) (cdr l))))))))
  '(apple cherry pie))) ; 3

; ((lambda (mk-length)
;     (mk-length mk-length))
;     (lambda (mk-length)
;       ((lambda (length)
;         (lambda (l)
;           (cond
;             ((null? l) 0)
;             (else (add1 (length (cdr l)))))))
;         (mk-length mk-length))))

; ((lambda (mk-length)
;     (mk-length mk-length))
;     (lambda (mk-length)
;       ((lambda (length)
;         (lambda (l)
;           (cond
;             ((null? l) 0)
;             (else (add1 (length (cdr l)))))))
;         (mk-length mk-length))))

; ((lambda (mk-length)
;   ((lambda (length)
;     (lambda (l)
;       (cond
;         ((null? l) 0)
;         (else (add1 (length (cdr l)))))))
;     (mk-length mk-length)))
;   (lambda (mk-length)
;     ((lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l)))))))
;       (mk-length mk-length))))

; ((lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
;   ((lambda (length)
;     (lambda (l)
;       (cond
;         ((null? l) 0)
;         (else (add1 (length (cdr l)))))))
;     ((lambda (mk-length)
;       ((lambda (length)
;         (lambda (l)
;           (cond
;             ((null? l) 0)
;             (else (add1 (length (cdr l)))))))
;         (mk-length mk-length)))
;     (lambda (mk-length)
;       ((lambda (length)
;         (lambda (l)
;           (cond
;             ((null? l) 0)
;             (else (add1 (length (cdr l)))))))
;         (mk-length mk-length))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
     (lambda (x)
       ((mk-length mk-length) x)))))

((lambda (le)
  ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (le (lambda (x) ((mk-length mk-length) x))))))
(lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
      (le (lambda (x) ((f f) x)))))))

(print
  '((Y (lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l))))))))
  '(apple sauce))) ; 2

(section "What Is the Value of All of This?")


; These are entries: pairs of lists where the fisrt one is a set
'((tell me more) (tell me more))
'((apple bottom jeans) (wat wat wat))

; This is also an entry, but we are not interested in ones like this.
; We'll prefer atoms as elements in the first list of entry
'((also (might be) this) (sure why not))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry fallback)
    (lookup-in-entry-help name
      (first entry)
      (second entry)
      fallback)))

(define lookup-in-entry-help
  (lambda (name names values fallback)
    (cond
      ((null? names) (fallback name))
      ((eq? name (car names)) (car values))
      (else
        (lookup-in-entry-help
          name
          (cdr names)
          (cdr values)
          fallback)))))

(print
  '(lookup-in-entry
    'entrée
    '((appetizer entrée beverage) (food tastes good))
     (lambda (x) ('())))) ; tastes

(print
  '(lookup-in-entry
    'butter
    '((appetizer entrée beverage) (food tastes good))
     zro?)) ; #f

; A table /also called an environment/ is a list of entries.
'() ; the empty one, e.g.
'(((one two three) (1 2 3))
  ((uno dos tres) (one two three))
  ((lav haskacanq) (mi ara)))

(define extend-table cons)

(print
  '(extend-table '((fruit) (apple)) '((berry) (blueberry)))) ; (((fruit) (apple)) ((berry) (blueberry)))

(define lookup-in-table
  (lambda (name table fallback)
    (cond
      ((null? table) (fallback name))
      (else
        (lookup-in-entry
          name
          (car table)
          (lambda (name)
            (lookup-in-table
              name
              (cdr table)
              fallback)))))))

(print
  '(lookup-in-table 'mek '(((mek erku)(erku mek)) ((mek erku)(ereq chors))) zro?))  ; erku
(print
  '(lookup-in-table 'kem '(((mek erku)(erku mek)) ((mek erku)(ereq chors))) zro?))  ; #f

(print
  '(cons 'car
      (cons (cons 'quote
        (cons
          (cons 'a
            (cons 'b
              (cons 'c
                (quote ()))))
          (quote ())))
        (quote ())))) ; (car (quote (a b c)))

; The code below can't be evaluated yet but provides important context
; Also, we'll use _quote_ instead of _'_ to make its' use more eye-catchy

; (value (car (quote (a b c)))) ; ......... a
; (value (quote (car (quote (a b c))))) ; . (car (quote (a b c)))
; (value (add1 6)) ; ...................... 7
; (value 6) ; ............................. 6
; (value (quote nothing)) ; ............... nothing
; (value nothing) ; ....................... nothing has no value

; (value
;   ((lambda (nothing)
;     (cons nothing (quote ())))
;   (quote
;       (from nothing comes something)))) ; ((from nothing comes something))

; (value
;   ((lambda (nothing)
;     (cond
;       (nothing (quote something))
;       (else (quote nothing))))
;     #f)) ; .............................. something

; (type 6)  ; ........................ *const
; (type #f) ; ........................ *const
; (value #f)  ; ...................... #f
; (type cons) ; ...................... *const
; (value car) ; ...................... (primitive car)
; (type (quote something)) ; ......... *quote
; (type nothing) ; ................... *identifier
; (type (lambda (x y) (cons x y))) ; . *lambda

; (type
;   ((lambda (nothing)
;     (cond
;       (nothing (quote something))
;       (else (quote nothing))))
;     #t)) ; ......................... *application

; (type
;   (cond
;     (nothing (quote something))
;     (else (quote nothing))))) ; .... *cond

; Overall we counted six _types_:
;  - *const
;  - *quote
;  - *identifier
;  - *lambda
;  - *cond
;  - *application

; We can think of these types as functions\actions, that do the right thing,
; when applied to the right type. In this case, we can re-think of the _value_
; function as something that finds out the type of the expression it was
; passed and uses the associated action.

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (a)
    (cond
      ((number? a) *const)
      ((eq? a #t) *const)
      ((eq? a #f) *const)
      ((eq? a 'cons) *const)
      ((eq? a 'car) *const)
      ((eq? a 'cdr) *const)
      ((eq? a 'null?) *const)
      ((eq? a 'eq?) *const)
      ((eq? a 'atom?) *const)
      ((eq? a 'zero?) *const)
      ((eq? a 'number?) *const)
      ((eq? a 'add1) *const)
      ((eq? a 'sub1) *const)
      (else *identifier))))

(define list-to-action
  (lambda (l)
    (cond
      ((atom? (car l))
        (cond
          ((eq? (car l) 'quote) *quote)
          ((eq? (car l) 'lambda) *lambda)
          ((eq? (car l) 'cond) *cond)
          (else *application)))
      (else *application))))

(define *value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (expression table)
    (cond
      ((number? expression) expression)
      ((eq? expression #f) #f)
      ((eq? expression #t) #t)
      (else (build 'primitive expression)))))

(print
  '(*const '#f '()))   ; #f
(print
  '(*const '#t '()))   ; #t
(print
  '(*const '7 '()))    ; 7
(print
  '(*const 'alo '()))  ; (primitive alo)

(define *quote
  (lambda (expression table)
    (text-of expression)))

(define text-of
  (lambda (expression)
    (second expression)))

(print
  '(*quote '(mi asa) '())) ; asa

; Authors propose the fallback below, but that will crash, intentionally.
; I'll use `zro?` to see it called
;
; (define initial-table
;   (lambda (name)
;     (car '())))

(define *identifier
  (lambda (identifier table)
    (lookup-in-table identifier table zro?)))

(print
  '(*identifier 'mek '(((mek erku)(erku mek)) ((mek erku)(ereq chors)))))  ; erku

(print
  '(*identifier 'alo '(((mek erku)(erku mek)) ((mek erku)(ereq chors))))) ; #f

(define *lambda
  (lambda (expression table)
    (build 'non-primitive (cons table (cdr expression)))))

(print
  '(*lambda '(lambda (x) x) '((('x)(2)))))  ; (non-primitive (((('x)(2))) (x) x))

(define table-of first)

(define formals-of second)

(define body-of third)

(print
  '(table-of '(((('x)(2))) (x) x))) ; ((('x)(2)))
(print
  '(formals-of '(((('x)(2))) (x) x))) ; (x)
(print
  '(body-of '(((('x)(2))) (x) x))) ; x


(define else?
  (lambda (question)
    (cond
      ((atom? question)(eq? question 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
        (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *application (lambda (x y) x))

(define *cond
  (lambda (expression table)
    (evcon (cond-lines-of expression) table)))

(print
  '(*cond
    '(cond
      (nothing (quote something))
      (else (quote nothing)))
    '(((nothing)(#t))))) ; something

(print
  '(*cond
    '(cond
      (nothing (quote something))
      (else (quote nothing)))
    '(((nothing)(#f))))) ; nothing

(define evlist
  (lambda (arguments table)
    (cond
      ((null? arguments) '())
      (else
        (cons
          (meaning (car arguments) table)
          (evlist (cdr arguments) table))))))

(print
  '(evlist '((quote x) y 8) '(((y)(7))))) ; (x 7 8)

(print
  '(evlist '() '(((y)(7))))) ; ()

(define function-of car)
(define arguments-of cdr)

(define *application
  (lambda (application table)
    (*apply
      (meaning (function-of application) table)
      (evlist (arguments-of application) table))))

; There are two types of functions
; - primitives, that are represented as (primitive _name_)
; - non-primitives, that are represented as (non-primitive (table formals body)), these are called closure/ record/s.

(define primitive?
  (lambda (function)
    (cond
      ((atom? function) #f)
      ((null? function) #f)
      (else (eq? (first function) 'primitive)))))

(print
  '(primitive? '(primitive car)))  ; #t

(print
  '(primitive? '()))  ; #f

(print
  '(primitive? '(non-primitive (((('x)(2))) (x) x))))  ; #f

(define non-primitive?
  (lambda (function)
    (cond
      ((atom? function) #f)
      ((null? function) #f)
      (else (eq? (first function) 'non-primitive)))))

(print
  '(non-primitive? '(primitive car)))  ; #f
(print
  '(non-primitive? '()))  ; #f
(print
  '(non-primitive? '(non-primitive (((('x)(2))) (x) x))))  ; #t

; We consider both our function representations to be atoms, hence the new _:atom_.
(define :atom?
  (lambda (expression)
    (cond
      ((atom? expression) #t)
      ((null? expression) #f)
      ((eq? (car expression) 'primitive) #t)
      ((eq? (car expression) 'non-primitive) #t)
      (else #f))))

(print
  '(:atom? '(primitive car)))  ; #t
(print
  '(:atom? '(non-primitive (((('x)(2))) (x) x))))  ; #t
(print
  '(:atom? 'primitive))  ; #t
(print
  '(:atom? '('not an atom)))  ; #f

; The application below has a lot of holes, e.g _null?_ in _car_ or _cdr_,
; but it will suffice
(define apply-primitive
  (lambda (name values)
    (cond
      ((eq? name 'cons) (cons (first values) (second values)))
      ((eq? name 'car) (car (first values)))
      ((eq? name 'cdr) (cdr (first values)))
      ((eq? name 'eq?) (eq? (first values) (second values)))
      ((eq? name 'null?) (null? (first values)))
      ((eq? name 'atom?) (:atom? (first values)))
      ((eq? name 'zero?) (zero? (first values)))
      ((eq? name 'add1) (add1 (first values)))
      ((eq? name 'sub1) (sub1 (first values)))
      ((eq? name 'number?) (number? (first values))))))

; This _apply_ will fail if the expression is not a function.
(define *apply
  (lambda (function values)
    (cond
      ((primitive? function) (apply-primitive (second function) values))
      ((non-primitive? function) (apply-closure (second function) values)))))

(print
  '(*apply '(primitive car) '((1 2 3)))) ; 1
(print
  '(*apply '(primitive cons) '(1 (2)))) ; (1 2)
(print
  '(*apply '(primitive atom?) '((primitive car))))  ; #t
(print
  '(*apply '(primitive atom?) '(indeed)))  ; #t
(print
  '(*apply '(primitive null?) '((1 2))))  ; #f

(define apply-closure
  (lambda (closure values)
    (meaning
      (body-of closure)
      (extend-table
        (new-entry (formals-of closure) values)
        (table-of closure)))))

(print
  '(apply-closure '(() (x y) (cons x y)) '(1 (2))))  ; (1 2)

(print
  '(*apply '(non-primitive (() (x y) (cons (add1 x) (cons (car (cdr y)) '())))) '(1 (2 bob #f))))  ; (2 bob)
