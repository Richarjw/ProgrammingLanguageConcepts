;Jack Richard - 4th Hour
;Assignment 3

;(1) finds the nearest point to a point, p, from a list of points.
; distance method calculates distance between two points. (from hw 2)
(define distance
	(lambda (p1 p2)
		(sqrt (+ (expt (- (car p2) (car p1)) 2) (expt [- (cadr p2) (cadr p1)] 2) (expt (- (caddr p2) (caddr p1)) 2)))))

(define nearest-point
	(lambda (p lop)
		(if [eq? (cdr lop) '()]
			(car lop)
			(if [<= (distance p (car lop)) (distance p (nearest-point p (cdr lop)))]
				(car lop)
				(nearest-point p (cdr lop))))))

;(2) Finds the union of two sets. 
;    Helper Function: Element? Returns true if element is in set

(define element?
	(lambda (x set)
		(cond ((null? set) #f)
			((eq? x (car set)) #t)
			(#t (element? x (cdr set))))))

(define union
	(lambda (s1 s2)
		(cond ((null? s2) s1)
		((element? (car s2) s1)
			(union s1 (cdr s2)))
		(#t (union (cons (car s2) s1) (cdr s2))))))
;(3) Finds the intersection of two sets

(define intersection
	(lambda (s1 s2)
		(if (null? s1) 
			'()
			(let ((temp (element? (car s1) s2)))
				(if (null? (cdr s1))
					(if temp s1 '())
						(if temp
							(cons (car s1) (intersection (cdr s1) s2))
							(intersection (cdr s1) s2)))))))
;(4) Returns if a set 1 is a subset of set 2
(define subset?
	(lambda (x y)
		(if (null? x)
			#t
			(if (element? (car x) y)
				(subset? (cdr x) y)
				#f))))

;(5) relation function returns if the set is a relation (or a set
; 	 of ordered pairs)
(define size?
	(lambda (x)
		(if (null? x)
			0
			(+ 1 (size? (cdr x))))))
(define set?
	(lambda (n)
		(if (equal? '() n)
			#t
			(if (eq? (list? n) #f)
				#f
				(if (member (car n) (cdr n))
					#f
					(set? (cdr n)))))))

(define relation?
	(lambda (x)
		(if (null? x)
			#t
			(if (eq? (set? x) #f)
				#f
				(if (and (list? (car x)) (= (size? (car x)) 2))
					(relation? (cdr x))
					#f)))))

;(6) Returns the domain of a given relation.

(define domain
	(lambda (r)
		(if (null? r)
			'()
			(if (member (caar r) (domain (cdr r)))
				(domain (cdr r))
				(cons (caar r) (domain (cdr r)))))))

; To get a set of the range, A helper is designed to get all of the second values
; Then it will work through the set in the second definition.
(define range-helper
	(lambda (set range)
		(if (eq? set '())
			range
			(if [and (or [equal? range '()] [not (equal? [cadr (car set)] [car range])])
					(not (member (cadr (car set)) range))]
				(range-helper (cdr set) (cons (cadr (car set)) range))
				(range-helper (cdr set) range)))))

(define range
	(lambda (set)
		(if (eq? set '())
			'()
			(range-helper set '()))))
;(7) Returns if the relation is reflexive or not. The helper takes a domain and range
; Then checks that the set contains a set of (a a) or (b b) for example.

(define reflexive-helper
	(lambda (dom rng set)
		(if (and (for-all (lambda (x)
						(if [not (member [list x x] set)]
							#f
							)) dom)
		(for-all (lambda (y)
			(if (not (member [list y y] set))
				#f)) rng))
		#t
		#f)))

(define reflexive?
	(lambda (set)
		(reflexive-helper (domain set) (range set) set)))
;(8) Hailstone step count uses the hailstone sequences given in the homework.
;	It implements the function f(x) shown in the description. the real method
;	just calls the helper method starting with step = 0.

(define hailstone-helper
	(lambda (x step)
		(if (eq? x 1)
			step
			(if (eq? (mod x 2) 0)
				(hailstone-helper (/ x 2) (+ step 1))
				(hailstone-helper (+ (* 3 x) 1) (+ step 1))))))

(define hailstone-step-count
	(lambda (x)
		(hailstone-helper x 0)))