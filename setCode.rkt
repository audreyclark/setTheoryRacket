;The four main returns of this file are:
; 1. Return the Union of two sets
; 2. Return the Intersection of two sets
; 3. Return of the Difference between two sets
; 4. Return the Symmetric-Difference of two sets

#lang racket
;Helper function: returns if element is in the given list.
;Used by Intersection, Difference and Symmetric-Difference.
(define Member?
  (lambda (e L)
    (cond ((null? L) #f)
         ((eq? e (car L)) #t)
          (else (Member? e (cdr L)))
          )
    ))
;Helper function: returns list without any duplicates.
;Used by Union
(define remove-duplicates
  (lambda (L)
    (cond ((null? L) '())
          ((Member? (car L) (cdr L)) (remove-duplicates (cdr L)))
          (else (cons (car L) (remove-duplicates (cdr L))))
          )
    )
  )
;1. Union. Returns everything in both lists, L and M, without any duplicates.
(define Union
  (lambda (L M)
    ;add all members of L to M
    (define F (append L M))
    ;call remove duplicates of M
    (remove-duplicates F)
    )
  )
;2. Intersection. Returns everything in common between two lists, L and M.
(define Intersection
  (lambda (L M)
    (cond ((null? L) '()) ;if either L or M is null, return null
          ((null? M) '())
          ((Member? (car L) M) ;if the first member of L is a member of M, add it to the result
          (cons (car L) (Intersection (cdr L) M)))
          (else (Intersection (cdr L) M))
          )
    )
  )
;3. Difference. Returns everything in L that is not in M.
(define Difference
  (lambda (L M)
    (cond ((null? L) '()) ;if L is null, return null
          ((null? M) L) ;if M is null, return L
          ((not (Member? (car L) M)) (cons (car L) (Difference (cdr L) M)))
          (else (Difference (cdr L) M))
          )
    )
  )
;4. Symmetric-Difference. Returns everything in L and M except for the elements that are in the intersection.
(define Symmetric-Difference 
  (lambda (L M)
    (Difference (Union L M) (Intersection L M))
    )
  )


