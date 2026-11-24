#lang racket
(define atom? (λ (x) (not (pair? x))))
(define mapu (λ (lst mapfn)
               (cond
                 ((null? lst) '())
                 (else (cons (mapfn (car lst)) (mapu (cdr lst) mapfn)))
                 )
               )
  )

(define firsts (λ (lstOfLst)
                 (mapu lstOfLst (λ (lst) (car lst)))
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

(define insertL* (λ (new old lst)
                   (cond
                     ((null? lst) '())
                     ((not (atom? (car lst))) (cons (insertL* new old (car lst)) (insertL* new old (cdr lst))))
                     ((eq? (car lst) old) (cons new (cons (car lst) (insertL* new old (cdr lst)))))
                     (else (cons (car lst) (insertL* new old (cdr lst))))
                     )
                   ))

(define member* (λ (a l)
                  (cond
                    ((null? l) #f)
                    ((not (atom? (car l))) (or (member* a (car l)) (member* a (cdr l))))
                    ((eq? (car l) a) #t)
                    (else (member* a (cdr l)))
                    )
                  ))

(define leftmost (λ (l)
                   (cond
                     ((null? l) '())
                     ((atom? (car l)) (car l))
                     (else (leftmost (car l)))
                     )
                   ))

(define equal? (λ (l1 l2)
                 (cond
                   ((and (atom? l1) (atom? l2)) (eq? l1 l2))
                   ((or (atom? l1) (atom? l2)) #f)
                   (else (eqlist? l1 l2))
                   )
                 ))

(define eqlist? (λ (l1 l2)
                  (cond
                    ((and (null? l1) (null? l2)) #t)
                    ((or (null? l1) (null? l2)) #f)
                    (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
                    )
                  ))

(define anyrember (λ(s ls)
                    (cond
                      ((null? ls) '())
                      ((equal? s (car ls)) (cdr ls))
                      (else (cons (car ls) (anyrember s (cdr ls))))
                    ))
)

(define value (λ(n)
                (cond
                    ((number? n) n)
                    ((atom? n) '())
                    (else 
                      (let
                            ((op (car (cdr n))) (lhs (car n)) (rhs (car (cdr (cdr n)))))
                            (cond
                              ((eq? '+ op) (+ (value lhs) (value rhs)))
                              ((eq? '* op) (* (value lhs) (value rhs)))
                              (else (expt (value lhs) (value rhs)))
                              )
                           )
                      )
                  )
                ))

(define value-prefix-op (λ(n)
                (cond
                    ((number? n) n)
                    ((atom? n) '())
                    (else 
                      (let
                            ((op (car n)) (lhs (car (cdr n))) (rhs (car (cdr (cdr n)))))
                            (cond
                              ((eq? '+ op) (+ (value-prefix-op lhs) (value-prefix-op rhs)))
                              ((eq? '* op) (* (value-prefix-op lhs) (value-prefix-op rhs)))
                              (else (expt (value-prefix-op lhs) (value-prefix-op rhs)))
                              )
                           )
                      )
                  )
                ))

(define value-pattern (λ(n 1st-sub-expr 2nd-sub-expr op-expr)
                (cond
                    ((number? n) n)
                    ((atom? n) '())
                    (else 
                      (let
                            ((op (op-expr n)) (lhs (1st-sub-expr n)) (rhs (2nd-sub-expr n)))
                            (cond
                              ((eq? '+ op) (+ (value-pattern lhs 1st-sub-expr 2nd-sub-expr op-expr) (value-pattern rhs 1st-sub-expr 2nd-sub-expr op-expr)))
                              ((eq? '* op) (* (value-pattern lhs 1st-sub-expr 2nd-sub-expr op-expr) (value-pattern rhs 1st-sub-expr 2nd-sub-expr op-expr)))
                              (else (expt (value-pattern lhs 1st-sub-expr 2nd-sub-expr op-expr) (value-pattern rhs 1st-sub-expr 2nd-sub-expr op-expr)))
                              )
                           )
                      )
                  )
                ))

(define prefix-1st-sub-expr (λ(n)
                              (car (cdr n))
                              ))

(define prefix-2nd-sub-expr (λ(n)
                              (car (cdr (cdr n)))
                              ))

(define prefix-operator (λ(n)
                              (car n)
                              ))

(define zero* (λ(n)
                 (null? n)
                 ))

(define add1* (λ(n)
               (cons '() n)
               ))

(define sub1*(λ(n)
               (cdr n)
               ))

(define plus* (λ(lhs rhs)
             (cond
               ((zero* lhs) rhs)
               (else (plus* (sub1* lhs) (add1* rhs)))
               )
             ))

(define member? (λ(a l)
                  (cond
                    ((null? l) #f)
                    ((eq? a (car l)) #t)
                    (else (member? a (cdr l)))
                    )
                  ))

(define set? (λ(l)
               (cond
                 ((null? l) #t)
                 (else (and (not (member? (car l) (cdr l))) (set? (cdr l)) ))
                 )
               ))

(define makeset (λ(l)
                  (cond
                    ((null? l) '())
                    ((member? (car l) (cdr l)) (makeset (cdr l)))
                    (else (cons (car l) (makeset (cdr l))))
                    )
                  ))

(define makeset-multirember (λ(l)
                  (cond
                    ((null? l) '())
                    (else (cons (car l) (makeset-multirember (multirember (car l) (cdr l)))))
                  ))
)

(define my-subset? (λ(l1 l2)
                  (cond
                    ((null? l1) #t)
                    (else (and (member? (car l1) l2) (my-subset? (cdr l1) l2)))
                    )
                  ))

(define eqset? (λ(l1 l2)
                 (and (my-subset? l1 l2) (my-subset? l2 l1))
                 ))

(define intersect? (λ(l1 l2)
                     (cond
                      ((null? l1) #f)
;;                      ((member? (car l1) l2) #t)
;;                      (else (intersect? (cdr l1) l2))
                      (else (or (member? (car l1) l2) (intersect? (cdr l1) l2)))
                     )
                     ))


(define intersect (λ(l1 l2)
                     (cond
                      ((null? l1) '())
                      ((member? (car l1) l2) (cons (car l1) (intersect (cdr l1) l2)))
                      (else (intersect (cdr l1) l2))
                     )
                     ))


