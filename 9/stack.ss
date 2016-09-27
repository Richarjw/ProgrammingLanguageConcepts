;Stack.ss

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