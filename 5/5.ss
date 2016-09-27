;Jack Richard
;Homework 5a


;(1)Minimizes the interval list by sorting the intervals then comparing them.

(define minimize-interval-list
	(lambda (lst)
		(if (eq? (length lst) 1)
			lst
			(let ([sortedlst (list-sort (lambda (pr1 pr2) (< (car pr1) (car pr2))) lst)])
				(if (>= (cadar sortedlst) (caadr sortedlst))
					(minimize-interval-list [cons (list [caar sortedlst] (max [cadadr sortedlst] [cadar sortedlst])) (cddr sortedlst)])
					[cons (car sortedlst) (minimize-interval-list (cdr sortedlst))])))))

;(2) Exists? Checks predicate with list.

(define exists?
	(lambda (pred lst)
		(if (eq? lst '())
			#f
			(if (pred (car lst))
				#t
				(exists? pred (cdr lst))))))

;(3)
(define list-index
	(lambda (pred ls)
		(if (null? ls)
			#f
			(if (exists? pred ls)
				(list-index-helper pred ls)
				#f))))
(define list-index-helper
	(lambda (pred ls)
		(if (pred (car ls))
			0
			(+ 1 (list-index-helper pred (cdr ls))))))


;(4) Pascals Triangle

(define pascal-elements
	(lambda (row col)
		(cond((= col 0) 1)
			((= col row) 1)
			(#t (+ (pascal-elements (- row 1) (- col 1))
				(pascal-elements(- row 1) col))))))

(define create-rows
	(lambda (row count)
		(if (= row count)
			'(1)
			(cons (pascal-elements row count) 
				(create-rows row (+ count 1))))))
(define pascal-triangle
	(lambda (rows)
		(if (<= rows -1)
			'()
			(cons (create-rows rows 0) (pascal-triangle (- rows 1))))))
(define product
	(lambda (x y)
		(if (eq? '() x)
			'()
			(append-list (product-helper (car x) y) (product (cdr x) y)))))

(define product-helper
	(lambda (first y)
		(if (eq? '() y)
			'()
			(cons (list first (car y)) (product-helper first (cdr y))))))
(define append-list
	(lambda (x y)
		(if (eq? '() x)
			y
			(cons (car x) (append-list (cdr x) y)))))

;(6) Max-edges gives the max amount of edges in an undirected graph
;    with a given number of vertices

(define max-edges
	(lambda (n)
		(/ (* n (- n 1)) 2)))
;(7) Checks if the graph is complete or not
(define element?
	(lambda (x set)
		(cond ((null? set) #f)
			((eq? x (car set)) #t)
			(#t (element? x (cdr set))))))

(define domain
	(lambda (r)
		(if (null? r)
			'()
			(cons (caar r) (domain (cdr r))))))
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

(define subset?
	(lambda (x y)
		(if (null? x)
			#t
			(if (element? (car x) y)
				(subset? (cdr x) y)
				#f))))


(define complete?
	(lambda (g)
		(if (eq? '() g)
			#t
			(let ([domn (reverse (domain g))]
				[rng (range g)])
				(complete-helper domn rng domn)))))


(define complete-helper
	(lambda (domn rng check)
		(if (and (eq? '() domn) (eq? '() rng))
			#t
			(if (or (eq? '() domn) (eq? '() rng))
				#f
				(if (not (subset? (cons (car domn) (car rng)) check))
					#f
					(if (eq? (length (cons (car domn) (car rng))) (length check))
						(complete-helper (cdr domn) (cdr rng) check)
						#f))))))

;(8) Makes a connected graph of the given list
(define make-set-of-others
	(lambda (x set)
		(if (eq? set '())
			'()
			(if (eq? (car set) x)
				(make-set-of-others x (cdr set))
				(cons (car set) (make-set-of-others x (cdr set)))))))

(define complete
	(lambda (lst)
		(middle-complete lst lst)))

(define middle-complete
	(lambda (lst total)
		(if (eq? lst '())
			'()
			(cons (list (car lst) (make-set-of-others (car lst) total))
				(middle-complete (cdr lst) total)))))


;(9) Replaces old value with new value in a list

(define replace
	(lambda (old new lst)
		(if (eq? lst '())
			'()
			(if (eq? (car lst) old)
				(cons new (replace old new (cdr lst)))
				(cons (car lst) (replace old new (cdr lst)))))))

;(10) Remove first existence of a given element in a list.


(define remove-first
	(lambda (x lst)
		(if (eq? '() lst)
			'()
			(if (eq? (car lst) x)
				(cdr lst)
				(cons (car lst) (remove-first x (cdr lst)))))))

;(11) Remove the last existence of a given element in a list.

(define remove-last
	(lambda (x lst)
		(let ([rev (reverse lst)])
			(reverse (remove-first x rev)))))