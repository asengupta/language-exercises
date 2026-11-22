#lang racket

(define sum-of-n (λ (n)
                  (cond
                    ((zero? n) 0)
                    (else (+ n (sum-of-n (sub1 n))))
                    )
                   )
  )

(define sum-of-n-taking-foo (λ (foo n)
                  (cond
                    ((zero? n) 0)
                    (else (+ n (sum-of-n-taking-foo foo (sub1 n))))
                    )
                   )
  )

(define sum-of-n-taking-foo-no-ref (λ (foo n)
                  (cond
                    ((zero? n) 0)
                    (else (+ n (foo foo (sub1 n))))
                    )
                   )
  )

(define sum-of-n-explicit-1
(λ (foo n) (cond ((zero? n) 0) (else (+ n (foo foo (sub1 n)))))))

; Let's break up the outer lambda into two separate curried functions.
(define sum-of-n-explicit-2
(λ (foo) (λ(n) (cond ((zero? n) 0) (else (+ n ((foo foo) (sub1 n))))))))

; Now we wish to inject `(foo foo)` into the `λ(n)`. This means we "create" `λ(n)` by calling an intervening factory lambda which takes in a `foo-recur` which is bound to a `(foo foo)` passed into the `λ(n)` factory lambda. The factory lambda returns the `λ(n)` function.
; HOWVEVER, the following line won't work because it recurses forever, owing to eager evaluation of (foo foo)
(define sum-of-n-explicit-3
(λ (foo) ((λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n)))))))(foo foo))))

; INTERLUDE: Why does it recurse?
; Let's evaluate:

;((sum-of-n-explicit-3 sum-of-n-explicit-3) 1)
;Assume that:

;LADDER = (λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n)))))))

; so that we can write:
;E = (λ (foo) (LADDER (foo foo)))

; STEP 1: We will now evaluate:
;`((E E) 1)`
; STEP 2: So we need to evaluate `(E E)` first, let's substitute the second `E` which is the parameter to `foo`, so:
;foo = E

; We unwrap one level of λ and put `E` wherever `foo` is, to get:

;(E E) = (LADDER (E E))

; STEP 3: To evaluate the above, we need to evaluate `(E E)`. This is the same exact step as STEP 2, so we are now in an infinite recursion.
; One fix is to delay the evaluation of (foo foo), by wrapping it in a λ, evaluate it inside, like (foo-recur), and then apply it to `n-1`.
; This works, but the sum-of-n logic has to remember to evaluate the λ, then apply it.
(define sum-of-n-explicit-no-arg-recur
(λ (foo) ((λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n ((foo-recur) (sub1 n)))))))(lambda() (foo foo)))))

; To make the λ oblivious of worrying about evaluating the lambda and then applying it to 'n-1', ehance the `(foo foo)` lambda to also take in the `n-1` parameter and evaluate it. Now the sum-of-n logic just assumes the recursion lambda is a plain function it can just apply on `n-1`.
(define sum-of-n-explicit-1-arg-recur
(λ (foo) ((λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n)))))))(λ(n) ((foo foo) n)))))

; INTERLUDE: Why does this not recurse infinitely?
;(E E) = (LADDER (λ(n) ((E E) n)))
; The expression `(λ(n) ((E E) n))` does not get evaluated right then, and thus stays as a lambda.
; What is more instructive is to see the trace of what happens mmid-recursion, inside.

; Let E = sum-of-n-explicit-1-arg-recur, then evaluate ((E E) 2):

; Then, substituting putting `E` everywhere `foo` is, we get:
#|
((λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n))))))) (λ(n) ((E E) n) ))
=> P = (E E) = (λ(n) (cond ((zero? n) 0) (else (+ n ( (λ(n) ((E E) n) ) (sub1 n))))))

; Let's call P(2):
=> P(2) = (cond ((zero? 2) 0) (else (+ 2 ( (λ(n) ((E E) n)  ) (sub1 2)))))
=> P(2) = (+ 2 ( (λ(n) ((E E) n)  ) 1))
=> P(2) = (+ 2 ( (E E) 1))
=> P(2) = (+ 2 ( 
            (λ(n) (cond ((zero? n) 0) (else (+ n ( (λ(n) ((E E) n) ) (sub1 n))))))
                                              1))

=> P(2) = (+ 2
  (cond ((zero? 1) 0) (else (+ 1 ((λ(n) ((E E) n)) (sub1 1)))))
                          )

=> P(2) = (+ 2
  (cond ((zero? 1) 0) (else (+ 1 ((λ(n) ((E E) n)) 0))))
                          )

=> P(2) = (+ 2 
  (+ 1 ((λ(n) ((E E) n)) 0))
          )

=> P(2) = (+ 2 
  (+ 1 ((E E) 0))
          )


=> P(2) = (+ 2 
  (+ 1 (
            (λ(n) (cond ((zero? n) 0) (else (+ n ( (λ(n) ((E E) n) ) (sub1 n))))))
              0))
          )

=> P(2) = (+ 2 
  (+ 1
            (cond ((zero? 0) 0) (else (+ 0 ( (λ(n) ((E E) n) ) (sub1 0)))))
              )
          )

=> P(2) = (+ 2 (+ 1 0))
|#

; You can run `((sum-of-n-explicit-1-arg-recur sum-of-n-explicit-1-arg-recur) 10)` for the above. If we went in, and blindly expanded `sum-of-n-explicit-1-arg-recur`, we get the following:
(
 (λ(foo) ((λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n)))))))(λ(n) ((foo foo) n))))
  (λ(foo) ((λ (foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n)))))))(λ(n) ((foo foo) n))))
)

; Let's refactor by pulling out the sum-of-n logic, which is the thing which differs in recursive walks; the recursion itself is always a standard pattern.
(define sum-of-n-step-factory
  (λ(foo-recur) (λ(n) (cond ((zero? n) 0) (else (+ n (foo-recur (sub1 n)))))))
)

(define sum-of-n-explicit-5
(
 (λ(foo) (sum-of-n-step-factory (λ(n) ((foo foo) n))))
  (λ(foo) (sum-of-n-step-factory (λ(n) ((foo foo) n))))
)
)

; `sum-of-n-step-factory` is a hardcoded step function in `sum-of-n-explicit-5`, we can make this more general by passing a (generic) step function into a factory function. This factory function is the Y Combinator. It is called a step function because it returns a function which tells us how to take the next step; this can be either termination or further recursion.
(define y-combinator (λ (step-fn-factory)
(
 (λ(foo) (step-fn-factory (λ(n) ((foo foo) n))))
  (λ(foo) (step-fn-factory (λ(n) ((foo foo) n))))
)
))

; We can call this like:

((y-combinator sum-of-n-step-factory) 10)

(define factorial (lambda (n)
                    (cond ((zero? n) 1)
                          (else (* n (factorial (sub1 n))))
                      )
                    ))

(define fib (lambda (n)
          (cond
            ((or (eq? n 1) (eq? n 2)) 1)
            (else (+ (fib (sub1 n)) (fib (- n 2)))
            )
            )
           ))
