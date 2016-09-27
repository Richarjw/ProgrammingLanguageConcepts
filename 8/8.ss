;Jack Richard CSSE371
;Homework 8

;(1a) (slist-map) Applies procedure to each element of slist


(define slist-map
	(lambda (proc slist)
		(if (null? slist)
			'()
			(if (list? slist)
				(cons (slist-map proc (car slist)) (slist-map proc (cdr slist)))
				(proc slist)))))
;(1b) Slist-reverse reverses a list and its sublists

(define slist-reverse
	(lambda (slist)
		(if (null? slist)
			'()
			(if (list? slist)
				(slist-reverse-helper (reverse slist))
			slist))))
(define slist-reverse-helper
	(lambda (slist)
		(if (null? slist)
			'()
			(if (list? (car slist))
				(cons (reverse (car slist)) (slist-reverse-helper (cdr slist)))
				(cons (car slist) (slist-reverse-helper (cdr slist)))))))
;(1c) Slist-paren-count counts the number of parentheses (sublist depth)
(define slist-paren-count
	(lambda (slist)
		(if (null? slist)
			2
			(if (list? slist)
				(+ (slist-paren-count (cdr slist)) (slist-paren-count (car slist)))
				0))))
;(1d) slist-depth returns the maximum depth of the deepest sublist
(define slist-depth
	(lambda (slist)
		(if (null? slist)
			1
			(if (list? (car slist))
				(max (+ 1 (slist-depth (car slist))) (slist-depth (cdr slist)))
			(slist-depth (cdr slist))))))
;(1e) slist-symbols-at-depth returns the symbols at a given depth
(define slist-symbols-at-depth
	(lambda (slist d)
		(if (null? slist)
			'()
			(cond 
				((and (eq? d 1) (not (list? (car slist))))
					(cons (car slist) (slist-symbols-at-depth (cdr slist) d)))
				((and (not (eq? d 1)) (list? (car slist)))
					(append (slist-symbols-at-depth (car slist) (- d 1)) (slist-symbols-at-depth (cdr slist) d)))
				(else
					(slist-symbols-at-depth (cdr slist) d))))))

;(2) Groups a given list into a list of 2lists
(define group-by-two
	(lambda (ls)
		(if (null? ls)
			'()
			(if (null? (cdr ls))
				(list (list (car ls)))
				(cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))))))

(define group-by-n
	(lambda (ls n)
		(if (null? ls) 
			'()
		 	(let ((chunk (take-up-to ls n))
		 		(rest (drop-n ls n)))
		 		(cons chunk (group-by-n rest n))))))



(define take-up-to
	(lambda (ls n)
		(if (or (zero? n) (null? ls))
			'()
			(cons (car ls) (take-up-to (cdr ls) (- n 1))))))

(define drop-n
	(lambda (ls n)
		(cond
			((null? ls) '())
			((zero? n) ls)
			(else
				(drop-n (cdr ls) (- n 1))))))


;(define subst-leftmost changes old symbol with new symbol on leftmost occurence)
(define subst-leftmost
	(lambda (new old lst pred)
		(cond
			((pair? lst)
				(let ((next (subst-leftmost new old (car lst) pred)))
					(cons next
						(if (eq? next (car lst))
							(subst-leftmost new old (cdr lst) pred)
							(cdr lst)))))
			((pred lst old) new)
			(else lst))))