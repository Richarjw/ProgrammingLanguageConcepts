





(define list-recur
	(lambda (base-value list-proc)
		(letrec
			([helper
				(lambda (ls)
					(if (null? ls)
						base-value
						(list-proc (car ls)
							(helper (cdr ls)))))])
		helper)))