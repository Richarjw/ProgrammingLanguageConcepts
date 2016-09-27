; Jack Richard - 4th Hour
; Assignment 1



;(1)Fahrenheit Problem
;Take an input fahrenheit temperature and return Celsius temperature

(define Fahrenheit->Celsius
	(lambda (f) 
		(* (/ 5 9) (- f 32))))


;(2) Interval-Contains method that checks if an input number is
;	 in the given interval
(define interval-contains? 
	(lambda (x y)
		(and (<= y (cadr x)) (>= y (car x)))))


;(3) Interval-Intersects method that checks if two intervals overlap at all
(define interval-intersects? 
	(lambda (x y)
		(or (and (<= (car y) (cadr x)) (>= (car y) (car x)))
			(and (<= (car x) (cadr y)) (>= (car x) (car y))))))
;(4) Interval- Union method puts two intervals together if they overlap. returns
; both if they aren't intersecting.
(define interval-union 
	(lambda (x y)
		(if (interval-intersects? x y)
			(list (list (min (car x) (car y)) (max (cadr x) (cadr y))))
			(list x y))))
;(5) Divisible-by-7? checks if a value is divisible by 7.
(define divisible-by-7?
	(lambda (x)
		(eq? (remainder x 7) 0)))
;(6)  Ends-With-7 checks if a value ends with 7
(define ends-with-7?
	(lambda (x)
		(eq? (remainder x 10) 7)))
;(7) The next 3 definitions will give the first second and third
;	 values in a list
(define 1st
	(lambda (x)
		(car x)))

(define 2nd
	(lambda (x)
		(cadr x)))

(define 3rd
	(lambda (x)
		(caddr x)))
