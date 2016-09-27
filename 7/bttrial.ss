(define bt-max-interior (lambda (T)
	(car (cadr (bt-max-helper T)))))		
	
(define bt-max-helper 
	(lambda (T) 
		(cond 
			[(and (list? (cadr T)) (list? (caddr T)))
				(bt-max-filter (car T) (bt-max-helper (cadr T)) (bt-max-helper (caddr T)))]
			[(and (list? (cadr T))(number? (caddr T)))(bt-max-filter-r (car T) (caddr T) (bt-max-helper (cadr T)))]
			[(and (number? (cadr T)) (list? (caddr T)))(bt-max-filter-l (car T) (cadr T) (bt-max-helper (caddr T)))]
			[else (list 
						(+ (cadr T) (caddr T))
						(list (car T) (+ (cadr T) (caddr T))))]
			)))
			
(define bt-max-filter
	(lambda (midname llst rlst)
		(cond 
			[(> (cadr (cadr llst)) (cadr (cadr rlst))) 
				(if (> (+ (car llst) (car rlst)) (cadr (cadr llst))) 
					(list (+ (car llst) (car rlst)) (list midname (+ (car llst) (car rlst)))) 
					(list (+ (car llst) (car rlst)) (cadr llst)))]
			[(< (cadr (cadr llst)) (cadr (cadr rlst)))
				(if (> (+ (car llst) (car rlst)) (cadr (cadr rlst))) 
					(list (+ (car llst) (car rlst)) (list midname (+ (car llst) (car rlst)))) 
					(list (+ (car llst) (car rlst)) (cadr rlst)))]
			[else 
				(if (> (+ (car llst) (car rlst)) (cadr (cadr rlst)))
					(list (+ (car llst) (car rlst)) (list midname (+ (car llst) (car rlst))))
					(list (+ (car llst) (car rlst)) (cadr llst)))]
			)))
		
(define bt-max-filter-l
	(lambda (midname num lst)
		(cond 
			[(< num 0) (list (+ (car lst) num) (cadr lst))]
			[(> (+ (car lst) num) (cadr (cadr lst))) (list (+ (car lst) num) (list midname (+ (car lst) num)))]
			[else (list (+ (car lst) num) (list midname (+ (car lst) num)))]
			)))

(define bt-max-filter-r
	(lambda (midname num lst)
		(cond 
			[(< num 0) (list (+ (car lst) num) (cadr lst))]
			[(> (+ (car lst) num) (cadr (cadr lst))) (list (+ (car lst) num) (list midname (+ (car lst) num)))]
			[else (list (+ (car lst) num) (cadr lst))]
			)))