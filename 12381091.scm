;;;returns the number of elements in the set s.
(define set-cardinality
	(lambda (s)
		(define my-len
			(lambda (s l) ;;;takes in the set and the current length
				(cond
					((empty? s) l) ;;;when list is empty, return count
					(else
						(my-len (cdr s) (+ l 1)))))) ;;;if list isn't empty, call again with 1st elem removed from list
		(my-len s 0))) ;;start with the whole list and count 0

;;returns true if the list l contains the element e
(define contains?
	(lambda (l e)
		(cond
			((empty? l) #f);;;shortcut for empty list
			((equal? (car l) e) #t);;;check if e equals the first elem in the list
			(else
			(contains? (cdr l) e)))));;;call again with the first element removed from the list


(define set-union
	(lambda (s1 s2)
		(cond ((null? s1) s2)
			((member (car s1) s2)
				(set-union (cdr s1) s2))
			(else (cons (car s1)
				(set-union (cdr s1) s2))))))

;;;returns a list (set) that is the intersection of set s1 and s2
(define set-intersection
	(lambda (s1 s2)
		(define intersect ;;;helper inline function
			(lambda (l s1 s2) ;;;takes in the intersected set of s1 and s2
				(cond
					((empty? s2) l) ;;;when all the elements in s2 have been checked, return l
					((contains? s1 (car s2)) ;;;if the first elem of s2 is in s1
						(intersect (cons (car s2) l) s1 (cdr s2))) ;;;intersect again adding first elem of s2 to l, and removing the same elem from s2
					(else ;;otheriwse intersect again with the first elem of be removed
						(intersect l s1 (cdr s2))))))
	(intersect '() s1 s2)));;;call intersect on s1 and s2 starting with an empty intersected set

;;;returns a list (set) that is the difference a less b
(define set-difference
	(lambda (a b)
  		(cond
  			((empty? a) '()) ;;;if a is empty, result is empty
        	((contains? b (car a)) ;;;if top elem of a is in b
         		(set-difference (cdr a) b)) ;;; remove top elem of b
         	(else (cons (car a) (set-difference (cdr a) b))))));;otherwise put top elem of a into b

;;;returns #t or #f to indicate whether or not s1=s2
(define set-equal?
	(lambda (s1 s2)
		(define set-eq-iter ;;; define function to iterate s2 and check for each elem in s1
			(lambda (s1 s2)
				(cond
					((empty? s2) #t) ;;;s2 will be empty when we've traversed it, all elements will have been found in s1 so s1=s2
					((contains? s1 (car s2))
						(set-eq-iter s1 (cdr s2))) ;;;if top elem of s2 is in s1, call again with top elem removed
					(else #f)))) ;;;if s2 not empty and the elem not in s1, the sets can't be equal
		(cond
			((equal? (set-cardinality s1) (set-cardinality s2))
				(set-eq-iter s1 s2)) ;;;only call set-eq-iter if s1 and s2 have same length
			(else #f))));;;if not the same length, sets cannot be equal


;;;applies the function f to each element in s
;;; this is extremely horrible
(define set-map-join
	(lambda (f s)
		(define my-map ;;;function to apply f to each elem, returning a set of the result sets
			(lambda (f s r)
				(cond
					((empty? s) r);;;when all elems in s are done, return r
					(else (my-map f (cdr s) (append (f (car s)) r))))))
		(define remove-duplicates
			(lambda (l)
			  (cond
			  	((empty? l) '());; -
		        ((contains? (cdr l) (car l)) ;; if top elem in l is present elsewhere in l
		        	(remove-duplicates (cdr l))) ;;;pop it off and move on
		        (else
		        	(cons (car l) (remove-duplicates (cdr l)))))))
		(remove-duplicates (my-map f s '())))) ;;;remove dups from result of function map

;;;  We represent λ-calculus terms as Scheme s-expressions, with variables as symbols, an application E1 E2 as (E1E2), and a λ expression λ v . E as the s-expression (λvE).
;;;  Define (free-variables e) which returns the set (represented as a list without duplicates) of free variables in the term represented by e.
(define free-variables
	(lambda (e)
		(cond
			((symbol? e) (list e))
			((or (equal? 'lambda (car e)) (equal? 'λ (car e)))
				(set-difference (free-variables  (car (cddr e))) (list (cadr e))))
			(else (set-union (free-variables (car e)) (free-variables (cadr e)))))))
