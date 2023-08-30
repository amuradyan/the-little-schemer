(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(atom? 'a)          ; true
(atom? '(f (g h)))  ; false

(null? ())  ; true

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
;   Always ask `null?` as the first questionm in expressing any function,
