;-----------------------------------------------------------------------------------------------
;Write a function f1 that decides whether a list has an atom inside.


(defun f1 (L)
  (cond
    ((null L) nil) ;if list is null, return nil
    ((atom (car L)) T) ;if the first element in the list is an atom, return true
    (T (f1 (cdr L))) ;else, check if cdr of L has atom inside
  )
)
;-----------------------------------------------------------------------------------------------
;Write a function f2 that counts the number of lists of length 1 in a list.



(defun my_length (L)
  (cond
    ((null L) 0) ; if L is empty, then length is 0
    (t (+ 1 (my_length (cdr L)))))) ; else, find length of cdr of L and add 1

(defun f2 (L)
  (cond
    ((null L) 0) ; if L is empty, return 0
    ((and (listp (car L)) (equal (my_length (car L)) 1)) (+ 1 (f2 (cdr L)))) ; if the first element is a list of length 1, increment the count and proceed with the rest
    (t (f2 (cdr L))) ; else just proceed with the rest
  )
) 



;-----------------------------------------------------------------------------------------------
;Write a function f3 that takes a list of integers and returns a list containing only odd integers.


(defun f3 (L)
  (cond
   ((null L) nil) ;if list is empty, return nil
   ((oddp (car L))(cons (car L) (f3 (cdr L)))) ;if the first element of L is odd, add it to the rest odd list
   (T (f3 (cdr L))) ; if the first element is not odd, find the odd numbers in the rest of the L
  ) 
)



;-----------------------------------------------------------------------------------------------
;Write a function f4 that returns the minimum value of an integer list.


(defun f4 (L)
  (cond
   ((null L) nil) ;if the list is empty, reutn nil
   ((null (cdr L)) (car l))  ;if there is one element in the list, return car L
   ((< (car L)(f4 (cdr L)))(car L)) ; if car L < the minimum of cdr L, return car L
   (T (f4(cdr L))) ; otherwise return the minimum of cdr L
  )
)


;-----------------------------------------------------------------------------------------------
;Write a function f5 that returns the reverse of a list.

(defun f5(L)
  (cond
   ((null L) nil) ;if the list is empty, reutn nil
   (T (append (f5(cdr L)) (list(car L)))) ;otherwise, put the reversed cdr L infront of car L
  )
)



;-----------------------------------------------------------------------------------------------
;Write a function f6 that returns the list containing every other element of a list.


(defun f6 (L)
  (cond 
    ((null L) nil)  ; if the list is empty, return nil
    ((null (cdr L)) (list (car L)))  ; if there's only one element, return it in a list
    (t (cons (car L) (f6 (cddr L)))) ; otherwise, cons the first element with the result of the recursive call on the rest
  )
)


;-----------------------------------------------------------------------------------------------
;Write a function f7 that returns the element at a given location of a list. The locations start at 1.

(defun f7(L n)
  (cond
    ((null L) nil)  ; if the list is empty, return nil
    ((equal 1 n)(car L)) ;if n = 1, return car L
    (T (f7 (cdr L)(- n 1))) ;else, make a recursive call to the rest of list, with n-1
  )
)



;-----------------------------------------------------------------------------------------------
;Write a function f8 that returns the sum of all integers everywhere in a list.


(defun f8 (L)
  (cond 
   ((null L) 0)	;if L is null, return zero
   ((listp (car L)) (+ (f8 (car L)) (f8 (cdr L)))) ;else if car L is a list, add sum of car L and sum of cdr L
   (t (+ (car L) (f8(cdr L))))	;else, add integer car L and sum of cdr L
  )
)


;-----------------------------------------------------------------------------------------------
;Write a function f9 that removes duplicates from a list.


(defun my_member (x L)
  (cond 
   ((null L) nil)  ; if the list is empty, return nil
   ((equal x (car L)) t)  ; if the item is equal to the first element, return true
   (t (my_member x (cdr L)))  ; otherwise, recurse on the rest of the list
  )
) 

(defun f9 (L)
  (cond 
   ((null L) nil)  ; if the list is empty, return nil
   ((my_member (car L) (cdr L)) (f9 (cdr L)))  ; if the element is a duplicate, skip it
   (t (cons (car L) (f9 (cdr L))))  ; otherwise, include the element
  )
)



;-----------------------------------------------------------------------------------------------
;Write a function f10 that finds the intersection of two lists. The intersection means the common elements of the two lists.

(defun f10(L1 L2)
  (cond
   ((null L1) nil) ;if L1 is null, return nil
   ((null L2) nil) ;if L2 is null, return nil
   ((my_member (car L1) L2)(cons (car L1) (f10 (cdr L1) L2))) ;if car L1 is part of L2, add car L1 to the intersection between cdr L1 and L2
   (T (f10 (cdr L1) L2)) ; otherwise check intersection between cdr L1 and L2
  )
)


;-----------------------------------------------------------------------------------------------