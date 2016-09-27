;Jack Richard Section 3
;Homework 9

;(load "stack.ss")
;(load "iter.ss")

;(1) snlist-recur acts like list-recur, but for snlists


(define snlist-recur
	(lambda (base-value procedure procedure2)
		(letrec
			([helper
				(lambda (snlist)
					(cond
						((null? snlist) base-value)
						((list? (car snlist))
							(procedure (helper (car snlist))
							(helper (cdr snlist))))
						(else
							(procedure2 (car snlist)
								(helper (cdr snlist))))))])
			helper)))

;(a) Sums the snlist
(define sn-list-sum
	(snlist-recur 0 + +))
;(b) Maps a procedure to an snlist. base value '(), if its a list
; cons the first helper to the second, and use procedure on the car if symbol/num
(define sn-list-map
	(lambda (f snlist)
		((snlist-recur '() (lambda (x y) (cons x y))
			(lambda (x y) (cons (f x) y))) 
		snlist)))

(define sn-list-paren-count
	(snlist-recur 2 + (lambda (x y) y)))

(define sn-list-reverse
	(snlist-recur '() 
		(lambda (x y) (append y (list x))) 
		(lambda (x y) (append y (list x)))))


(define sn-list-occur
	(lambda (sym snlist)
		((snlist-recur 0 +
			(lambda (x y)
				(if (eq? sym x)
					(+ y 1)
					y))) snlist)))

(define sn-list-depth
	(snlist-recur 1 
		(lambda (x y) (+ (max x (- y 1)) 1)) 
		(lambda (x y) y)))

;(2) Recurrence function for Binary Trees
(define bt-recur
	(lambda (base-value procedure procedure2)
		(letrec 
			([helper
				(lambda (T)
					(cond
						((null?  T) base-value)
						((number? T) (procedure T))
						(else
							(cond
								((null? (cdr T))
									(procedure2 (car T) '() '()))
								((null? (cddr T))
									(procedure2 (car T) (helper (cadr T)) '()))
								(else
									(procedure2 (car T) (helper (cadr T)) (helper (caddr T))))))))])
							
			helper)))


(define bt-sum
	(bt-recur 0 + (lambda (x y z) (+ y z))))


(define bt-inorder
	(bt-recur '() 
		(lambda (x) '()) 
		(lambda (x y z) (append y (list x) z))))

;(3) Calculates the composition of cars and cdrs given
(define compose
	(case-lambda
		[() (lambda (x) x)]
		[(first . rest)
			(let ([composed-rest (apply compose rest)])
			(lambda (x) (first (composed-rest x))))]))

(define make-c...r
	(lambda (lst)
		(apply compose
			(map eval
				(map string->symbol
					(map
						(lambda (check)
							(list->string
								(list #\c check #\r)))
						(string->list lst)))))))

;(4) Leaf Iterator. Iterates through slist each call.

;Stack given:
(define make-stack
	(lambda ()
 		(let ([stk '()])
 			(lambda (msg . args )
 				(case msg ; Scheme's case is a similar to switch in some other languages.
 					[(empty?) (null? stk)]
 					[(push) (set! stk (cons (car args) stk))]
 					[(pop) (let ([top (car stk)])
 								(set! stk (cdr stk))
 								top)]
 					[else (errorf 'stack "illegal message to stack object: ~a" msg)])))))


(define make-slist-leaf-iterator
	(lambda (slist)
		(let ([stack (make-stack)])
			[define find-first
				(lambda (slist)
					(cond
						((pair? slist)
							(stack 'push (cdr slist))
							(find-first (car slist)))
						((null? slist) (find-next))
						(else
							slist)))]
			[define find-next
				(lambda ()
					(if (stack 'empty?)
						#f
						(find-first (stack 'pop))))]
			(let ([current (find-first slist)])
				(lambda ()
					(let ([return-val current])
						(set! current (find-next))
						return-val))))))