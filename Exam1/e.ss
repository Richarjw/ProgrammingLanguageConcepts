;Jack Richard Section 3
;Exam 1
;(1) Range of numbers finds the range of a list of numbers
(define min-of-list
	(lambda (lst)
		(if (null? (cdr lst))
			(car lst)
			(min (car lst) (min-of-list (cdr lst))))))

(define max-of-list
	(lambda (lst)
		(if (null? (cdr lst))
			(car lst)
			(max (car lst) (max-of-list (cdr lst))))))

(define range-of-numbers
	(lambda (lon)
		(- (max-of-list lon) (min-of-list lon))))

;(2) los->ms returns a multiset given a list of symbols
(define count-symbol
	(lambda (los sym count)
		(if (null? los)
			(append (list sym) (list count))
			(if (eq? (car los) sym)
				(count-symbol (cdr los) sym (+ count 1))
				(count-symbol (cdr los) sym count)))))

(define los->ms
	(lambda (los)
		(if (null? los)
			'()
			(cons (count-symbol los (car los) 0) (los->ms (remove (car los) los))))))

;(3) Symbols-with-n-count returns the number of different symbols that occur n times in a list.
(define symbols-with-n-count
	(lambda (los n)
		(if (null? los)
			0
			(if (eq? (cadr (count-symbol los (car los) 0)) n)
				(+ 1 (symbols-with-n-count (remove (car los) los) n))
				(symbols-with-n-count (remove (car los) los) n)))))