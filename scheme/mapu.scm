#lang racket

(define atom (lambda (x) (not (pair? x))))
(define mapu (lambda (lst mapfn)
    (cond
      ((null? lst) '())
      (else (cons (mapfn (car lst)) (mapu(cdr lst) mapfn)))
    )
    )
)

(define firsts (lambda (lstOfLst)
                (mapu lstOfLst ( lambda (lst) (car lst) ))
                 ))

(define insertR (lambda (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons rightOf (cons inserted (cdr lst))))
                  (else (cons (car lst) (insertR inserted rightOf (cdr lst))))
                  )
                  ))

(define insertL (lambda (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons inserted (cons rightOf (cdr lst))))
                  (else (cons (car lst) (insertL inserted rightOf (cdr lst))))
                  )
                  ))

(define subst2 (lambda (new o1 o2 lst)
                 (cond
                   ((null? lst) '())
                   ((eq? o1 (car lst)) (cons new (cdr lst)))
                   ((eq? o2 (car lst)) (cons new (cdr lst)))
                   (else (cons (car lst) (subst2 new o1 o2 (cdr lst))))
                  )
                ))

(define rember (lambda (a lst)
                   (cond
                     ((null? lst) '())
                     ((eq? a (car lst)) (cdr lst))
                     (else (cons (car lst) (rember a (cdr lst))))
                   )))

(define multirember (lambda (a lst)
                   (cond
                     ((null? lst) '())
                     ((eq? a (car lst)) (multirember a (cdr lst)))
                     (else (cons (car lst) (multirember a (cdr lst))))
                   )))

(define multiinsertR (lambda (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons rightOf (cons inserted (multiinsertR inserted rightOf (cdr lst)))))
                  (else (cons (car lst) (multiinsertR inserted rightOf (cdr lst))))
                  )
                  ))

(define multiinsertL (lambda (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons inserted (cons rightOf (multiinsertL inserted rightOf (cdr lst)))))
                  (else (cons (car lst) (multiinsertL inserted rightOf (cdr lst))))
                  )
                  ))
(define o+ (lambda (lhs rhs)
             (cond
              ((zero? rhs) lhs)
              (else (add1 (o+ lhs (sub1 rhs))))
             )
             ))
(define reconstruct* 
  (lambda (structure)
    (cond
      ((atom? structure) structure)
      (else (cons (reconstruct* (car structure)) (reconstruct* (cdr structure))))
      )
    )
  )

(define rember* 
  (lambda (toDelete structure)
    (cond
      ((atom? structure) structure)
      ((null? structure) '())
      ((eq? toDelete (car structure)) (rember* toDelete (cdr structure)))
      (else (cons (rember* toDelete (car structure)) (rember* toDelete (cdr structure))))
      )
    )
  )
  
