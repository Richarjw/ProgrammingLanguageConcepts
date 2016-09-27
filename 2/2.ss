; Jack Richard - 4th Hour
; Assignment 2


; (1) Fact function returns the factorial of a given non negative
;     number, Then you use the function in choose, which finds the
;	  Binomial Coefficient.
(define fact
	(lambda (n)
		(if (= n 0)
			1
		(* n (fact (- n 1))))))

(define choose
	(lambda (n k) 
		(/ (fact n) (* (fact k) (fact (- n k))))))

; (2) Range Function returns a list of all the values between m and n,
;	  not including n. Will return empty list if m = n
(define range
	(lambda (m n)
		(if (>= m n)
			'()
			(cons m (range (+ m 1) n)))))

; (3) Checks for duplicates in a given list, if there are no duplicates
;	  it will return true, confirming it being a set.

(define set?
	(lambda (n)
		(if (equal? '() n)
			#t
			(if (or (member (car n) (cdr n)) (eq? (list? n) #f))
				#f
				(set? (cdr n))))))
; (4) returns the sum of the squares of each number in a list

(define sum-of-squares
	(lambda (lon)
		(if (eq? lon '())
			0
			(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))))))

; (5) Returns a vector from point 1 to point 2.

(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) (- (caddr p2) (caddr p1)))))

; (6) Returns the dot product of 2 vectors

(define dot-product
	(lambda (v1 v2)
		(+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)) (* (caddr v1) (caddr v2)))))

; (7) Returns the magnitude of vector v

(define vec-length
	(lambda (v)
		(sqrt (sum-of-squares v))))

; (8) Returns the distance from 1 point to another

(define distance
	(lambda (p1 p2)
		(sqrt (+ (expt (- (car p2) (car p1)) 2) (expt (- (cadr p2) (cadr p1)) 2) (expt (- (caddr p2) (caddr p1)) 2)))))

; (9) Returns the Cross Product of 2 points

(define cross-product
	(lambda (p1 p2)
		(list (- [* (cadr p1) (caddr p2)] [* (caddr p1) (cadr p2)]) (- [* (caddr p1) (car p2)] [* (car p1) (caddr p2)]) (- [* (car p1) (cadr p2)] [* (cadr p1) (car p2)]))))

; (10) return whether two vectors are parallel or not.

(define parallel?
	(lambda (v1 v2)
		(and (eq? 0 (car (cross-product v1 v2))) (eq? 0 (cadr (cross-product v1 v2))) (eq? 0 (caddr (cross-product v1 v2))))))

(define collinear? 
	(lambda (p1 p2 p3)
		(parallel? (make-vec-from-points p1 p2) (make-vec-from-points p2 p3))))