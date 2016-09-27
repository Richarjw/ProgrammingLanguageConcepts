;(4) Leaf Iterator. Iterates through slist each call.


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