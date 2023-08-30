(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(atom? 'a)
(atom? '(f (g h)))

(null? ())

;;;;;;; Do it, do it again, and again, and again ...

(define lat?
  (lambda (x)
    (cond
     ((null? x) #t)
     ((atom? (car x)) (lat? (cdr x)))
     (else #f))))

(lat? ())
(lat? '((t g) r))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(member? 'd '(a d g))
(member? 'd '(a g))

; The First Commandment (preliminary):
;   Always ask `null?` as the first questionm in expressing any function,
