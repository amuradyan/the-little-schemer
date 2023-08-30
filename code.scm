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
