;Jack Richard 
;Homework 4

;(1) Multiset: Returns if the set is a multi set.
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
(define domain
	(lambda (r)
		(if (null? r)
			'()
			(cons (caar r) (domain (cdr r))))))
(define domain-check
	(lambda (n)
		(if (null? r)
			#t
			(if (member? (caar n) (cdr n))
				#f
				(domain-check (cdr n))))))
(define range-helper
	(lambda (set range)
		(if (eq? set '())
			range
			(range-helper (cdr set) (cons (cadr (car set)) range))
				)))

(define range
	(lambda (set)
		(if (eq? set '())
			'()
			(range-helper set '()))))
(define relation?
	(lambda (x)
		(if (null? x)
			#t
			(if (eq? (set? x) #f)
				#f
				(if (and (list? (car x)) (= (size? (car x)) 2))
					(relation? (cdr x))
					#f)))))
(define norepeat?
	(lambda (n)
		(if (eq? '() n)
			#t
			(if (member (car n) (cdr n))
				#f
				(norepeat? (cdr n))))))
(define char-lst? 
	(lambda (n)
		(if (eq? '() n)
			#t
			(if (symbol? (car n))
				(char-lst? (cdr n))
				#f))))
(define num-lst?
	(lambda (n)
		(if (eq? '() n)
			#t
			(if (and (number? (car n)) (> (car n) 0))
				(num-lst? (cdr n))
				#f))))
(define multi-set?
	(lambda (n)
		(if (eq? '() n)
			#t
			(if (eq? (relation? n)  #f)
				#f
				(let ([domn (domain n)])
					(if (norepeat? domn)
						(if (eq? (char-lst? domn) #f)
							#f
							(let ([rng (range n)])
								(if (eq? (num-lst? rng) #f)
									#f
									(if (relation? n)
										#t
										#f))))
						#f))))))
;(2) Returns size of multi set by adding the elements of the range
(define ms-size
	(lambda (n)
		(if (multi-set? n)
			(let ([rng (range n)])
				(apply + rng)))))

;(3) Returns the value at the specific location.

(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))

;(4) Checks if object is a matrix

(define sizelists
	(lambda (n)
		(if (null? n)
			'()
			(if (eq? (list? n) #f)
				#f
				(cons (length (car n))
				(sizelists (cdr n)))))))
(define all-equal?
	(lambda (head n)
		(if (null? n)
			#t
			(and (= head (car n)) (not (= head 0))
				(all-equal? head (cdr n))))))
(define check-lists
	(lambda (n)
		(let ([lengths (sizelists n)])
			(all-equal? (car lengths) (cdr lengths)))))
(define matrix? 
	(lambda (obj)
		(if (and (list? obj) (list? (car obj)))
			(check-lists obj)
			#f)))

;(5) Matrix-Transpose returns a matrix transposed (columns and rows swap)

(define get-new-lists
	(lambda (index lists)
		(if (eq? '() lists)
			'()
			(cons (matrix-ref lists 0 index) (get-new-lists index (cdr lists))))))

(define transpose-helper
	(lambda (matrix index)
		(if (eq? (length (car matrix)) index)
			'()
			(cons (get-new-lists index matrix) 
				(transpose-helper matrix (+ index 1))))))
(define matrix-transpose
	(lambda (matrix)
		(if (eq? '() matrix)
			'()
			(transpose-helper matrix 0))))
;(6) Last returns last element of a list

(define last
	(lambda (ls)
		(list-ref ls (- (length ls) 1))))
;(7) all-but-last returns a list with every element except for the last

(define all-but-last
	(lambda (ls)
		(if (eq? (length ls) 1)
			'()
			(cons (car ls) (all-but-last (cdr ls))))))