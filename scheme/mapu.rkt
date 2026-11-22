#lang racket
(define atom? (λ (x) (not (pair? x))))
(define mapu (λ (lst mapfn)
    (cond
      ((null? lst) '())
      (else (cons (mapfn (car lst)) (mapu(cdr lst) mapfn)))
    )
    )
)

(define firsts (λ (lstOfLst)
                (mapu lstOfLst ( λ (lst) (car lst) ))
                 ))

(define insertR (λ (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons rightOf (cons inserted (cdr lst))))
                  (else (cons (car lst) (insertR inserted rightOf (cdr lst))))
                  )
                  ))

(define insertL (λ (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons inserted (cons rightOf (cdr lst))))
                  (else (cons (car lst) (insertL inserted rightOf (cdr lst))))
                  )
                  ))

(define subst2 (λ (new o1 o2 lst)
                 (cond
                   ((null? lst) '())
                   ((eq? o1 (car lst)) (cons new (cdr lst)))
                   ((eq? o2 (car lst)) (cons new (cdr lst)))
                   (else (cons (car lst) (subst2 new o1 o2 (cdr lst))))
                  )
                ))

(define rember (λ (a lst)
                   (cond
                     ((null? lst) '())
                     ((eq? a (car lst)) (cdr lst))
                     (else (cons (car lst) (rember a (cdr lst))))
                   )))

(define multirember (λ (a lst)
                   (cond
                     ((null? lst) '())
                     ((eq? a (car lst)) (multirember a (cdr lst)))
                     (else (cons (car lst) (multirember a (cdr lst))))
                   )))

(define multiinsertR (λ (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons rightOf (cons inserted (multiinsertR inserted rightOf (cdr lst)))))
                  (else (cons (car lst) (multiinsertR inserted rightOf (cdr lst))))
                  )
                  ))

(define multiinsertL (λ (inserted rightOf lst)
                  (cond
                  ((null? lst) '())
                  ((eq? rightOf (car lst)) (cons inserted (cons rightOf (multiinsertL inserted rightOf (cdr lst)))))
                  (else (cons (car lst) (multiinsertL inserted rightOf (cdr lst))))
                  )
                  ))
(define o+ (λ (lhs rhs)
             (cond
              ((zero? rhs) lhs)
              (else (add1 (o+ lhs (sub1 rhs))))
             )
             ))
(define reconstruct* 
  (λ (structure)
    (cond
      ((atom? structure) structure)
      (else (cons (reconstruct* (car structure)) (reconstruct* (cdr structure))))
      )
    )
  )

(define rember* 
  (λ (toDelete structure)
    (cond
      ((atom? structure) structure)
      ((null? structure) '())
      ((eq? toDelete (car structure)) (rember* toDelete (cdr structure)))
      (else (cons (rember* toDelete (car structure)) (rember* toDelete (cdr structure))))
      )
    )
  )
  
(define two-in-a-row (λ (lst)
                     (cond
                      ((null? lst) #f)
                      (else (two-in-b-row (car lst) (cdr lst)))
                      ) 
                      ))

(define two-in-b-row (λ (preceding lst)
                     (cond
                      ((null? lst) #f)
                      ((eq? preceding (car lst)) #t)
                      (else (two-in-b-row (car lst) (cdr lst)))
                      ) 
                      ))

(define sum-of-prefixes (λ (lst)
                            (sop-a '0 lst)
                         ))

(define sop-a (λ (acc lst)
                (cond
                  ((null? lst) '())
                  (else (cons (+ acc (car lst)) (sop-a (+ acc (car lst)) (cdr lst))))
                  )
                ))

(define scramble (λ (lst)
                    (scrimble '() lst)
                  ))

(define scrimble (λ (stack lst)
                   (cond
                    ((null? lst) lst)
                    (else (cons (peeku (sub1 (car lst)) (cons (car lst) stack)) (scrimble (cons (car lst) stack) (cdr lst))))
                    )
                  ))

(define peeku (λ (index stack)
               (cond
                 ((zero? index) (car stack))
                 (else (peeku (sub1 index) (cdr stack)))
                 ) 
                ) 
  )

(define lenu (λ (lst)
              (cond
                ((null? lst) '0)
                (else (add1 (lenu (cdr lst))))
               ))
  )

(define looking (λ (a lst)
                  (keep-looking a (pick 1 lst) lst)))

(define keep-looking (λ (a elem lst)
                        (cond
                          ((number? elem) (keep-looking a (pick elem lst) lst))
                          (else (eq? a elem))
                          )
                       ))

(define pick (λ (index lst)
               (cond
                 ((eq? index 1) (car lst))
                 (else (pick (sub1 index) (cdr lst)))
                 )
               ))

(define shift (λ (x)
                  (cons (car (car x)) (cons (cons (car (cdr (car x))) (cdr x)) '()))
              )
  )

(define insertL* (λ(new old lst)
                   (cond
                     ((null? lst) '())
                     ((not (atom? (car lst))) (cons (insertL* new old (car lst)) (insertL* new old (cdr lst))))
                     ((eq? (car lst) old) (cons new (cons (car lst) (insertL* new old (cdr lst)))))
                     (else (cons (car lst) (insertL* new old (cdr lst))))
                      )
                   ))

(define member* (λ(a l)
                  (cond 
                    ((null? l) #f)
                    ((not (atom? (car l))) (or (member* a (car l)) (member* a (cdr l))) )
                    ((eq? (car l) a) #t)
                    (else (member* a (cdr l)))
                    )
                  ))

(define leftmost (λ(l)
                    (cond
                      ((null? l) '())
                      ((atom? (car l)) (car l))
                      (else (leftmost (car l)))
                      )
                   ))

(define eqlist? (λ(l1 l2)
                  (cond
                  ((and (null? l1) (null? l2)) #t)
                  ((or (null? l1) (null? l2)) #f)
                  ((and (atom? (car l1)) (atom? (car l2))) (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
                  ((and (not (atom? (car l1))) (not (atom? (car l2)))) (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
                  (else #f)
                  )
                  ))
