;Jack Richard
;Homework 6

;(1) curry2 returns the curried version of the question in eoPL-1

(define curry2
	(lambda (pred)
		(lambda (first)
			(lambda (second)
				(pred first second)))))


;(2) Curried version of compose
(define curried-compose
	(lambda (pred1)
		(lambda (pred2)
			(lambda (a)
				(pred1 (pred2 a))))))

;(3) compose a list of predicates

(define compose
	(lambda rest
		(lambda (x)
			(compose-helper rest x))))

(define compose-helper
	(lambda (rest x)
		(if (null? rest)
			x
			((car rest) (compose-helper (cdr rest) x)))))

;(4) makes a list of the given obj by a quantity x
		
(define make-list-c 
	(lambda (x)
		(lambda (ls)
			(make-list-c-helper x ls))))

(define make-list-c-helper
	(lambda (x ls)
		(if (zero? x)
			'()
			(cons ls (make-list-c-helper (- x 1) ls)))))

;(5) Takes a let and produces an application
(define let->application
	(lambda (lst)
		(cons (list 'lambda (map car (cadr lst)) (caddr lst)) (map cadr (cadr lst)))))


;(6) takes a let* and turns it into a let statement
(define let*->let
	(lambda (lst)
		(let build ([variable-definition (cadr lst)] [lst (caddr lst)])
			(if (null? variable-definition)
				lst
				`(let (, (car variable-definition)) ,(build (cdr variable-definition) lst))))))		 

;(7) filter in applies the predicate to the given list and keeps 
;    the values that are true

(define filter-in
	(lambda (pred lst)
		(if (eq? '() lst)
			'()
			(if (pred (car lst))
				(cons (car lst) (filter-in pred (cdr lst)))
				(filter-in pred (cdr lst))))))
;(8) Filters out the values that are true in the list to a given predicate
(define filter-out
	(lambda (pred lst)
		(if (eq? '() lst)
			'()
			(if (not (pred (car lst)))
				(cons (car lst) (filter-out pred (cdr lst)))
				(filter-out pred (cdr lst))))))


(define sort-list-of-symbols
	(lambda (lst)
		(map string->symbol (list-sort string<? (map symbol->string lst)))))

(define invert
	(lambda (lst)
		(if (eq? '() lst)
			'()
			(cons (list (cadar lst) (caar lst)) (invert (cdr lst))))))
(define vector-index
	(lambda (pred vec)
		(letrec ([check (lambda (n)
			(if (eq? n (vector-length vec))
				#f
				(if (pred (vector-ref vec n))
					n
					(check (+ n 1)))))])
		(check 0))))