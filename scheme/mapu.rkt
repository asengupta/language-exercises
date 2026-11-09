#lang racket
(define atom? (lambda (x) (not (pair? x))))
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
  
(define two-in-a-row (lambda (lst)
                     (cond
                      ((null? lst) #f)
                      (else (two-in-b-row (car lst) (cdr lst)))
                      ) 
                      ))

(define two-in-b-row (lambda (preceding lst)
                     (cond
                      ((null? lst) #f)
                      ((eq? preceding (car lst)) #t)
                      (else (two-in-b-row (car lst) (cdr lst)))
                      ) 
                      ))

(define sum-of-prefixes (lambda (lst)
                            (sop-a '0 lst)
                         ))

(define sop-a (lambda (acc lst)
                (cond
                  ((null? lst) '())
                  (else (cons (+ acc (car lst)) (sop-a (+ acc (car lst)) (cdr lst))))
                  )
                ))

(define scramble (lambda (lst)
                    (scrimble '() lst)
                  ))

(define scrimble (lambda (stack lst)
                   (cond
                    ((null? lst) lst)
                    (else (cons (peeku (sub1 (car lst)) (cons (car lst) stack)) (scrimble (cons (car lst) stack) (cdr lst))))
                    )
                  ))

(define peeku (lambda (index stack)
               (cond
                 ((zero? index) (car stack))
                 (else (peeku (sub1 index) (cdr stack)))
                 ) 
                ) 
  )

(define lenu (lambda (lst)
              (cond
                ((null? lst) '0)
                (else (add1 (lenu (cdr lst))))
               ))
  )

(define looking (lambda (a lst)
                  (keep-looking a (pick 1 lst) lst)))

(define keep-looking (lambda (a elem lst)
                        (cond
                          ((number? elem) (keep-looking a (pick elem lst) lst))
                          (else (eq? a elem))
                          )
                       ))

(define pick (lambda (index lst)
               (cond
                 ((eq? index 1) (car lst))
                 (else (pick (sub1 index) (cdr lst)))
                 )
               ))

(define shift (lambda (x)
                  (cons (car (car x)) (cons (cons (car (cdr (car x))) (cdr x)) '()))
              )
  )

