;;;;;;; Toys

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(atom? 'a)          ; true
(atom? '(f (g h)))  ; false

(null? ())  ; true

; The Law of Car
;   The primitive `car` is defined only for non-empty lists.

; The Law of Cdr
;   The primitive `cdr` is defined only for non-empty lists.
;   The `cdr` of any non-empty list is always another list.

; The Law of Cons
;   The primitive `cons` takes two arguments. The second
;   argument to `cons` must be a list. The result is a list.

; The Law of Null?
;   The primitive `null?` is defined only for lists.

; The Law of Eq?
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

; The First Commandment (preliminary):
;   Always ask `null?` as the first questionm in expressing any function.


;;;;;;; Cons the Magnificent

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) ())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(rember 'a '(a b c))	    ; (b c)
(rember 'a '(a b a c))    ; (b a c)
(rember 'a '(b a g a c))	; (b g a c)
(rember 'a '(f g))		    ; (f g)
(rember 'a ())			      ; ()

; The Second Commandment
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

(insertR 'c 'b '(a b d)) 	  ; (a b c d)
(insertR 'c 'b '(a b b d)) 	; (a b c b d)
(insertR 'c 'b ())		 	    ; ()

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))))

(insertL 'c 'b '(a b d)) 	  ; (a c b d)
(insertL 'c 'b '(a b b d)) 	; (a c b b d)
(insertL 'c 'b ())		 	    ; ()

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat)))))))))

(subst 'c 'b '(a b d)) 	  ; (a c d)
(subst 'c 'b '(a b b d)) 	; (a c b d)
(subst 'c 'b ())		 	    ; ()

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

(subst2 'c 'b 'd '(a b d)) 	  ; (a c d)
(subst2 'c 'b 'd '(a d b)) 	  ; (a c b)
(subst2 'c 'b 'd '(a b b d)) 	; (a c b d)
(subst2 'c 'b 'd ())		 	    ; ()

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) ())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'a '(a b c))	    ; (b c)
(multirember 'a '(a b a c))   ; (b c)
(multirember 'a '(b a g a c)) ; (b g c)
(multirember 'a '(f g))       ; (f g)
(multirember 'a ())			      ; ()

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(multiinsertR 'c 'b '(a b d)) 	  ; (a b c d)
(multiinsertR 'c 'b '(a b b d)) 	; (a b c b c d)
(multiinsertR 'c 'b ())		 	      ; ()

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(multiinsertL 'c 'b '(a b d)) 	  ; (a c b d)
(multiinsertL 'c 'b '(a b b d)) 	; (a c b c b d)
(multiinsertL 'c 'b ())		 	      ; ()

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) ())
      (else
        (cond
        ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(multisubst 'c 'b '(a b d)) 	  ; (a c d)
(multisubst 'c 'b '(a b b d)) 	; (a c c d)
(multisubst 'c 'b ())		 	      ; ()

; The Fourth Commandment
;   Always change at least one argument while recurring. It
;   must be changed to be closer to termination. The changing
;   argument must be tested in the termination condition:
;   when using `cdr`, test termination with `null?`.
