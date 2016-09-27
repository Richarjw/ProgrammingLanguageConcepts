;Jack Richard CSSE304
;Homework 7

;(1) Appends a list to a vector and returns a vector
(define vector-append-list
 (lambda (vec ls)
	 (let ([new-vector (make-vector (+ (vector-length vec) (length ls)))])
 		(copy-from-vector new-vector vec 0)
 		(copy-from-list new-vector ls (vector-length vec) 0)
 		new-vector)))


(define copy-from-vector
	(lambda (cpy orig index)
		(if (eq? index (vector-length orig))
			cpy
			(begin
			(vector-set! cpy index (vector-ref orig index))
			(copy-from-vector cpy orig (+ index 1))))))

(define copy-from-list
	(lambda (cpy lst index n)
		(if (eq? index (vector-length cpy))
			cpy
			(begin
				(vector-set! cpy index (list-ref lst n))
				(copy-from-list cpy lst (+ index 1) (+ n 1))))))

;(2) Quicksort
;qsort function seperates into 3 sublists for <, =, > than the pivot point


(define qsort
	(lambda (pred lst)
		(cond
			((eq? pred <) (up lst))
			((eq? pred >) (down lst))
			((eq? pred string<?) (upString lst))
			(else (downString lst)))))
(define part
	(lambda (pred lst)
		(cond
			((null? lst) '())
			((pred (car lst)) (cons (car lst) (part pred (cdr lst))))
			(else (part pred (cdr lst))))))

(define up
	(lambda (lst)
		(cond
			((or (null? lst) (= (length lst) 1)) lst)
			(else
				(let ((pivot (car lst)))
					(append (up (part (lambda (x) (< x pivot)) lst))
						(append (part (lambda (x) (= x pivot)) lst)
							(up (part (lambda (x) (> x pivot)) lst)))))))))
	
(define down 
	(lambda (lst)
		(reverse (up lst))))

(define upString
	(lambda (lst)
		(cond
			((or (null? lst) (= (length lst) 1)) lst)
			(else
				(let ((pivot (car lst)))
					(append (upString (part (lambda (x) (string<? x pivot)) lst))
						(append (part (lambda (x) (string=? x pivot)) lst)
							(upString (part (lambda (x) (string>? x pivot)) lst)))))))))
(define downString
	(lambda (lst)
		(reverse (upString lst))))


;(3) Connected, Returns if the graph is connected or not
;Breaking the problem down into multiple helpers


(define connected?
	(lambda (lst)
		(if (or (null? (cdr lst)) (null? lst))
			#t
			(= (size lst) (numNodes lst (car lst) lst)))))
(define size
	(lambda (lst)
		(if (null? lst)
			0
			(+ 1 (size (cdr lst))))))
(define find
	(lambda (lst item)
		(cond
			((null? lst) '())
			((eq? (caar lst) item) (car lst))
			(else (find (cdr lst) item)))))

(define contains
	(lambda (item lst)
		(cond
			((null? lst) #f)
			((eq? (car lst) item) #t)
			(else (contains item (cdr lst))))))
(define remove
	(lambda (item lst)
		(cond
			((null? lst) '())
			((eq? item (car lst)) (cdr lst))
			(else (cons (car lst) (remove item (cdr lst)))))))

(define numNodes
	(lambda (lst current nodes)
		(if (null? (cadr current))
			1
			(let [(nextNode (find lst (caadr current)))
				(remaining (remove current nodes))]
				(cond 
					((not (contains current nodes)) 0)
					(else
						(+ (numNodes lst (cons (car current) (list (cdadr current))) remaining) (numNodes lst nextNode remaining))))))))
		


;(4) Reverses a list without using the built in function

(define reverse-it
	(lambda (lst)
		(if (eq? lst '())
			'()
			(append (reverse-it (cdr lst)) (list (car lst))))))


;(5) BST problem. Define multiple helpers

(define empty-BST 
	(lambda ()
		'()))

(define empty-BST? 
	(lambda (obj)
		(null? obj)))

;Gets root
(define BST-element
	(lambda (BST)
		(if (eq? BST '())
			'()
			
				
				(car BST))))

;Gets left Sub Tree

(define BST-left
	(lambda (BST)
		(if (eq? BST '())
			'()
			(cadr BST))))


;Gets Right Sub Tree

(define BST-right
	(lambda (BST)
		(if (eq? BST '())
			'()
			(caddr BST))))


;Checks if node is a leaf

(define leaf?
	(lambda (BST)
		(and (null? (BST-right BST)) (null? (BST-left BST)))))

;Makes BST
(define make-bst
	(lambda (root left right)
		(list root left right)))

;Recursively checks subtrees for correct location to insert value
(define BST-insert
	(lambda (val bst)
		(cond
			((empty-BST? bst) (make-bst val (empty-BST) (empty-BST)))
			((< val (BST-element bst))
				(make-bst (BST-element bst) (BST-insert val (BST-left bst)) (BST-right bst)))
			((> val (BST-element bst))
				(make-bst (BST-element bst) (BST-left bst) (BST-insert val (BST-right bst))))
			(else
				bst))))
;Inserts a list of nodes
(define BST-insert-nodes
	(lambda (bst lst)
		(if (eq? lst '())
			bst
			(BST-insert-nodes (BST-insert (car lst) bst) (cdr lst)))))
;Returns inorder BST
(define BST-inorder
	(lambda (bst)
		(if (empty-BST? bst) 
			'()
			(append (BST-inorder (BST-left bst)) (list (BST-element bst)) (BST-inorder (BST-right bst))))))
;Checks if BST
(define BST?
	(lambda (bst)
		(cond
			((empty-BST? bst) #t)
			((not (list? bst)) #f)
			((not (eq? (length bst) 3)) #f)
			((or (not (list? (BST-left bst))) (not (list? (BST-right bst)))) #f)
			((not (number? (BST-element bst))) #f)
			((and (not (null? (BST-left bst)))
				(not (greater-than-all? (BST-element bst) (BST-inorder (BST-left bst))))) #f)
			((and (not (null? (BST-right bst)))
				(not (less-than-all? (BST-element bst) (BST-inorder (BST-right bst))))) #f)
			(else (and (BST? (BST-left bst)) (BST? (BST-right bst)))))))
			

(define less-than-all?
	(lambda (val lst)
		(if (null? lst)
			#t
			(if (< val (car lst))
				(less-than-all? val (cdr lst))
				#f))))
(define greater-than-all?
	(lambda (val lst)
		(if (null? lst)
			#t
			(if (> val (car lst))
				(greater-than-all? val (cdr lst))
				#f))))


;Checks if BST contains a value
(define BST-contains?
	(lambda (bst val)
		(cond
			((empty-BST? bst) #f)
			((= val (BST-element bst)) #t)
			((< val (BST-element bst))
				(BST-contains? (BST-left bst) val))
			(else
				(BST-contains? (BST-right bst) val)))))


(define map-by-position
	(lambda (fn-list arg-list)
		(map (lambda (fn ar)
			(fn ar)) fn-list arg-list)))


(define bt-leaf-sum
	(lambda (t)
		(if (number? t)
			t
			(+ (bt-leaf-sum (cadr t)) (bt-leaf-sum (caddr t))))))


(define bt-inorder-list
	(lambda (t)
		(if (number? t)
			'()
			(append (bt-inorder-list (cadr t)) (cons (car t) (bt-inorder-list (caddr t)))))))

(define bt-max
	(lambda (t)
		(if (number? t)
			t
			(max (bt-max (cadr t)) (bt-max (caddr t))))))

(define bt-max-interior (lambda (T)
	(car (cadr (bt-helper T)))))		
	
(define bt-helper 
	(lambda (T) 
		(cond 
			((and (list? (cadr T)) (list? (caddr T)))
				(bt-max-filter (car T) (bt-helper (cadr T)) (bt-helper (caddr T))))
			((and (list? (cadr T))(number? (caddr T)))(bt-left (car T) (caddr T) (bt-helper (cadr T))))
			((and (number? (cadr T)) (list? (caddr T)))(bt-right (car T) (cadr T) (bt-helper (caddr T))))
			(else (list 
						(+ (cadr T) (caddr T))
						(list (car T) (+ (cadr T) (caddr T))))
			))))
			
(define bt-max-filter
	(lambda (node left right)
		(cond 
			((> (cadr (cadr left)) (cadr (cadr right))) 
				(if (> (+ (car left) (car right)) (cadr (cadr left))) 
					(list (+ (car left) (car right)) (list node (+ (car left) (car right)))) 
					(list (+ (car left) (car right)) (cadr left))))
			((< (cadr (cadr left)) (cadr (cadr right)))
				(if (> (+ (car left) (car right)) (cadr (cadr right))) 
					(list (+ (car left) (car right)) (list node (+ (car left) (car rlst)))) 
					(list (+ (car left) (car right)) (cadr right))))
			(else 
				(if (> (+ (car left) (car right)) (cadr (cadr right)))
					(list (+ (car left) (car right)) (list node (+ (car left) (car right))))
					(list (+ (car left) (car right)) (cadr left))))
			)))
(define bt-max-left
	(lambda (node num lst)
		(cond 
			((< num 0) (list (+ (car lst) num) (cadr lst)))
			((> (+ (car lst) num) (cadr (cadr lst))) (list (+ (car lst) num) (list node (+ (car lst) num))))
			(else (list (+ (car lst) num) (cadr lst)))
			)))	
(define bt-right
	(lambda (node num lst)
		(cond 
			((< num 0) (list (+ (car lst) num) (cadr lst)))
			((> (+ (car lst) num) (cadr (cadr lst))) (list (+ (car lst) num) (list node (+ (car lst) num))))
			(else (list (+ (car lst) num) (list node (+ (car lst) num))))
			)))

