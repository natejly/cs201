#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
)

; Please do not change lines above this one.

;************************************************************
; CS 201 HW #8  DUE Monday December 11th at 11:59 pm, 
; via the submit system on the Zoo. 
;************************************************************
; Name:Nate
; Email address:nate.ly@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.
;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;******************************(******************************

(define (getrandom bound)
  (random 0 bound)
  )
(define (lorint count bound)
  (lorinth count bound empty))

(define (lorinth count bound rlst)
  [cond
    [(equal? 0 count) rlst]
    [else
     (lorinth ( - count 1) bound (cons (getrandom bound) rlst))
     ]
    ]
  )

(define (time-calls reps proc args)
    (time-callsh reps proc args (current-inexact-milliseconds)))

(define (time-callsh reps proc args start)
  (if (zero? reps)
      (/ (- (current-inexact-milliseconds) start) 1000)
      (begin
        (apply proc args)
        (time-callsh (- reps 1) proc args start))))
;**************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For the procedures length, take, and drop, replace length-runtime,
; take-runtime, and drop-runtime with either O(1) or O(n) to most accurately
; reflect each procedure's respective running time as a function of the length n
; of the list argument.

; Compare the times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest? Replace length-take-drop-ordering with an ordering of these
; procedures. Then, in the space below, explain *why* on the basis of how lists and
; their operations are implemented. (Complex statistical analysis is not
; necessary.)
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; Please report measurements here.
;length 
;0.013969970703125
;0.028891845703125
;0.0445849609375
;0.05693017578125

;0.01181689453125
;0.02914404296875
;0.042421875
;0.0572041015625

;take
;0.040909912109375
;0.0696669921875
;0.168568115234375
;0.353281982421875

;0.046966064453125
;0.079179931640625
;0.33414599609375
;0.79527490234375

;drop
;0.009
;0.019450927734375
;0.027633056640625
;0.040123046875

;0.008671142578125
;0.018991943359375
;0.026508056640625
;0.038309814453125

(define length-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
;time increase over length increase produces a function that is linear implying O(n) complexity
(define take-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
;The time increases slightly more than linearly with increasing list size
;This is clearly not constant time so this implies O(n) is the better choice
(define drop-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.
;runtime increases linearly with list size implying O(n) complexity

; Please do not use commas, and please order from fastest to slowest
; e.g. (define length-reverse-powerset-ordering '(length reverse powerset))
(define length-take-drop-ordering '(drop length take))
; Please explain your ordering here.
;looking at the times, drop consistently was the fastest, take was consistently and by far the slowest, and length was in the middle
;

;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
;           (compare? x y) 
;           #t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
;           (equal? x y) 
;           #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
;           (compare? x z) 
;           #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))

; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct. Replace "replace" in total-order-runtime
; with your answer.

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define total-order-runtime "O(n^3)")
; Explain your answer here.
;Have to check every possible combination of tripples, so that would imply O(n^3) complexity
(define (total-order? compare? domain)
  (define (reflex x y)
    (or (not (equal? x y)) (compare? x y)

        )
    )

  (define (asym x y)
    (or (not (and (compare? x y) (compare? y x)))
        (equal? x y)
        )
    )

  (define (total x y)
    (or (compare? x y) (compare? y x)
        )
    )
  (define (trans x y z)
    (if (and (compare? x y)(compare? y z))
        (compare? x z)
        #t)
    )
  (define (check-pairs property)
    (for*/and ([x domain] [y domain]) ; ands every combination of pairs
      (property x y)
      )
    )
  (define goodpair?
    (and (check-pairs reflex)
         (check-pairs asym)
         (check-pairs total)
         )
    )

  (define goodtripple?
    (and
     (for*/and ([x domain] [y domain] [z domain]) ; ands every combo of tripples
       (trans x y z)
       )
     )
    )
  (and goodpair? goodtripple?)
  )

;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangement"))
;'("the" "hello" "best" "arrangement")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (sorted? compare? lst)
  [cond 
  [(>= 1 (length lst)) #t]
  [(not (compare? (car lst) (cadr lst))) #f]
  [else (sorted? compare? (cdr lst))]
  ]
  
  )

(define (insert compare? item lst)
  (inserth compare? item empty lst))


(define (inserth compare? item left right)
  [cond
    [(empty? right) (append left (list item))]
    [(compare? item (car right)) (append left (list item) right)]
    [else
     (inserth compare? item (append left (list (car right))) (cdr right))
     ]
    ]
  )
(define (merge compare? lst1 lst2)
 [cond
 [(empty? lst2) lst1]
  [(empty? lst1) lst2]
      [(compare? (first lst1) (first lst2))
     (cons (first lst1) (merge compare? (rest lst1) lst2))]
     [else
     (cons (first lst2) (merge compare? lst1 (rest lst2)))]
 ])
;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare lst)
  (if (null? lst)
      '()
      (insert compare (car lst) (isort compare (cdr lst)))))

(define (msort compare lst)
  (if (or (empty? lst) (empty? (rest lst)))
      lst
        (merge compare (msort compare (take lst (quotient (length lst) 2)))
               (msort compare (drop lst (quotient (length lst) 2))))))

;************************************************************
; ** problem 6 ** (20 points)
; (1)(a) Give empirical evidence that your implementation of insertion sort
;        (isort, above) has best case time Omega(n) and worst case time of
;        O(n^2).
        ;best case already sorted yields a linear time complexity, 
        ;where the time increases linearly as the list size increases linearly
        ;for y = 0.02634x + 0.01078 r^2 is 0.987 with length on x and time on y
        ;(time-calls 1000 isort (list < (inclst 1000)))
        ;(time-calls 1000 isort (list < (inclst 2000)))
        ;(time-calls 1000 isort (list < (inclst 3000)))
        ;(time-calls 1000 isort (list < (inclst 4000)))

        ;0.03606201171875
        ;0.062284912109375
        ;0.0954541015625
        ;0.112807861328125
        
        ;0.03765380859375
        ;0.061282958984375
        ;0.094953857421875
        ;0.111819091796875

        ;worst case reverse sorted yields  O(n^2)
        ; for y = 0.6358 - 1.017x + 0.4634x2 with length on x and time on y
        ;R^2 is 0.9993
        ;(time-calls 100 isort (list > (inclst 100)))
        ;(time-calls 100 isort (list > (inclst 200)))
        ;(time-calls 100 isort (list > (inclst 300)))
        ;(time-calls 100 isort (list > (inclst 400)))

        ;0.06376806640625
        ;0.509860107421875
        ;1.675905029296875
        ;3.968201171875

        ;0.06373095703125
        ;0.5020859375
        ;1.716650146484375
        ;4.1092431640625

;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.
        ;average case is theta (n^2)
        ; for y = 2.126 - 3.242x + 1.458x2 with length on x and time on y
        ; r^2 is 0.9991
        ;implies theta (n^2) time complexity
        ;(time-calls 1000 isort (list < (lorint 100 110)))
        ;(time-calls 1000 isort (list < (lorint 200 220)))
        ;(time-calls 1000 isort (list < (lorint 300 330)))
        ;(time-calls 1000 isort (list < (lorint 400 440)))

        ;0.276427001953125
        ;1.668135009765625
        ;5.32218701171875
        ;12.544380859375
                
        ;0.22639599609375
        ;1.7151162109375
        ;6.3048388671875
        ;13.995800048828125
; (2)(a) Give empirical evidence that your implementation of merge sort
;        (msort, above) has best case and worst case times of Theta(n log n).
          ;y=0.2151+0.0242⋅ln(x) r^2 is 0.997 for length on x and time/n on y
          ;this implies a nlogn time complexity
          ;Best case is 
          ;(time-calls 500 msort (list < (inclst 1000)))
          ;(time-calls 500 msort (list < (inclst 2000)))
          ;(time-calls 500 msort (list < (inclst 3000)))
          ;(time-calls 500 msort (list < (inclst 4000)))

          ;0.2151708984375
          ;0.4647041015625
          ;0.7201640625
          ;0.998784912109375

          ;0.21918896484375
          ;0.468385009765625
          ;0.720834228515625
          ;1.01380712890625

          ;Worst case
          ;y=0.2047+0.0344⋅ln(x) r^2 = 0.995 for length on x and time/n on y
          ;implying a nlog n time complexity
          ;(time-calls 500 msort (list > (inclst 1000)))
          ;(time-calls 500 msort (list > (inclst 2000)))
          ;(time-calls 500 msort (list > (inclst 3000)))
          ;(time-calls 500 msort (list > (inclst 4000)))

          ;0.20489892578125
          ;0.454510986328125
          ;0.73289111328125
          ;1.00596484375

          ;0.20743603515625
          ;0.461887939453125
          ;0.75589501953125
          ;1.021989990234375
          ;looks linnear but by the nature of the msort can't be so nlogn is the best answer
;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.
        ;logarithmic with average case running time is theta (nlogn)
        ;y=0.4095+0.0642⋅ln(x) r^2 is 0.989 with length on x and time/n on y
        ;shows in form nlogn
        ;average case
        ;(time-calls 500 msort (list < (lorint 1000 1100)))
        ;(time-calls 500 msort (list < (lorint 2000 2200)))
        ;(time-calls 500 msort (list < (lorint 3000 3300)))
        ;(time-calls 500 msort (list < (lorint 4000 4400)))

        ;0.4081259765625
        ;0.917285888671875
        ;1.425369873046875
        ;1.9995009765625

        ;0.413375
        ;0.925598876953125
        ;1.4480859375
        ;2.0108349609375

; Be sure to use sufficiently long lists of integers and possibly repeat/average
; measurements.

; (3) Please identify inputs that give best and worst cases for your
; implementations of (a) isort and (b) msort. Be sure that you use sufficiently
; long lists of randomly chosen integers in a range larger than the length of
; the list, so that there are unlikely to be many duplicate values.
;made function to create a list of increasing values of length n

;worst case would be a revrse sorted list
;best is a sorted list
;average is a randomly generated list with lorint

;sorted increasing list given below
;(define (inclst n [rlst empty])
;  [cond
;    [(equal? 0 n) rlst]
;    [
;     (inclst (- n 1) (append (list n) rlst ))
;     ]
;    ]
;  )
; (4) Roughly what is the longest list of random integers that your (a) isort
; procedure can sort in 10 seconds?  Same question for your (b) msort procedure?

;for I sort equation for random ints is y = 2.126 - 3.242x + 1.458x2
;setting y to 10 x is around 3.688e3 but this is a bit slow at 10.2 ish s
;rounding down a bit (time-calls 1 isort (list < (lorint 3650 3651)))
;gives 9.94s so the max ints is around 3650

;(time-calls 1 msort (list < (lorint 3600000 3600000)))
;gives 9.9s so max is around 3.6e6
; Because of memory caching and other effects, the timing behaviors will not
; necessarily be uniform over the whole range of feasible input lengths.
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; 3a) Please briefly describe best case inputs for your implementation of isort here.
;best case inputs for isort have an already sorted list
        ;(time-calls 1000 isort (list < (inclst 1000)))
        ;(time-calls 1000 isort (list < (inclst 2000)))
        ;(time-calls 1000 isort (list < (inclst 3000)))
        ;(time-calls 1000 isort (list < (inclst 4000)))

        ;0.03606201171875
        ;0.062284912109375
        ;0.0954541015625
        ;0.112807861328125
        
        ;0.03765380859375
        ;0.061282958984375
        ;0.094953857421875
        ;0.111819091796875
; 3a) Please briefly describe worst case inputs for your implementation of isort here.
;worst case inputs have a reverse sorted list
        ;(time-calls 100 isort (list > (inclst 100)))
        ;(time-calls 100 isort (list > (inclst 200)))
        ;(time-calls 100 isort (list > (inclst 300)))
        ;(time-calls 100 isort (list > (inclst 400)))

        ;0.06376806640625
        ;0.509860107421875
        ;1.675905029296875
        ;3.968201171875

        ;0.06373095703125
        ;0.5020859375
        ;1.716650146484375
        ;4.1092431640625
; 1b) Please indicate average case running time of your implementation of isort here.
      ;average running time uses random generated lists
        ;(time-calls 1000 isort (list < (lorint 100 110)))
        ;(time-calls 1000 isort (list < (lorint 200 220)))
        ;(time-calls 1000 isort (list < (lorint 300 330)))
        ;(time-calls 1000 isort (list < (lorint 400 440)))

        ;0.276427001953125
        ;1.668135009765625
        ;5.32218701171875
        ;12.544380859375
                
        ;0.22639599609375
        ;1.7151162109375
        ;6.3048388671875
        ;13.995800048828125
; 3b) Please briefly describe best case inputs for your implementation of msort here.
        ;Best case is already sorted list
          ;(time-calls 500 msort (list < (inclst 1000)))
          ;(time-calls 500 msort (list < (inclst 2000)))
          ;(time-calls 500 msort (list < (inclst 3000)))
          ;(time-calls 500 msort (list < (inclst 4000)))

          ;0.2151708984375
          ;0.4647041015625
          ;0.7201640625
          ;0.998784912109375

          ;0.21918896484375
          ;0.468385009765625
          ;0.720834228515625
          ;1.01380712890625
; 3b) Please briefly describe worst case inputs for your implementation of msort here.
      ;Worst case reverse sorted list
          ;(time-calls 500 msort (list > (inclst 1000)))
          ;(time-calls 500 msort (list > (inclst 2000)))
          ;(time-calls 500 msort (list > (inclst 3000)))
          ;(time-calls 500 msort (list > (inclst 4000)))

          ;0.20489892578125
          ;0.454510986328125
          ;0.73289111328125
          ;1.00596484375

          ;0.20743603515625
          ;0.461887939453125
          ;0.75589501953125
          ;1.021989990234375
; 2b) Please indicate average case running time of your implementation of msort here.
        ;average case using random generated lists
        ;(time-calls 500 msort (list < (lorint 1000 1100)))
        ;(time-calls 500 msort (list < (lorint 2000 2200)))
        ;(time-calls 500 msort (list < (lorint 3000 3300)))
        ;(time-calls 500 msort (list < (lorint 4000 4400)))

        ;0.4081259765625
        ;0.917285888671875
        ;1.425369873046875
        ;1.9995009765625

        ;0.413375
        ;0.925598876953125
        ;1.4480859375
        ;2.0108349609375
; 1a, 2a) Please provide evidence for the above claims here.
;Isort
  ;best
   ;best case already sorted yields a linear time complexity, 
        ;where the time increases linearly as the list size increases linearly yielding
        ;for y = 0.02634x + 0.01078 r^2 is 0.987 with length on x and time on y

  ;worst
          ;worst case reverse sorted yields  O(n^2)
        ; for y = 0.6358 - 1.017x + 0.4634x^2 with length on x and time on y
        ;R^2 is 0.9993

  ;average
          ;average case is theta (n^2)
        ; for y = 2.126 - 3.242x + 1.458x2 with length on x and time on y
        ; r^2 is 0.9991
        ;implies theta (n^2) time complexity
;msort
  ;best
            ;y=0.2151+0.0242⋅ln(x) r^2 is 0.997 for length on x and time/n on y
          ;this implies a nlogn time complexity
  ;worst
            ;y=0.2047+0.0344⋅ln(x) r^2 = 0.995 for length on x and time/n on y
          ;implying a nlog n time complexity
  ;average
          ;logarithmic with average case running time is theta (nlogn)
        ;y=0.4095+0.0642⋅ln(x) r^2 is 0.989 with length on x and time/n on y
        ;shows theta(nlogn)

; 4a, 4b) Please indicate the longest list of random integers that a) your isort
; procedure can sort in 10 seconds and b) your msort procedure can sort
; in 10 seconds.

;for I sort equation for random ints is y = 2.126 - 3.242x + 1.458x2
;setting y to 10 x is around 3.688e3 but this is a bit slow at 10.2 ish s
;rounding down a bit (time-calls 1 isort (list < (lorint 3650 3651)))
;gives 9.94s so the max ints is around 3650

;(time-calls 1 msort (list < (lorint 3600000 3600000)))
;gives 9.9s so max is around 3.6e6
;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************

(define (count-compares sort compare? lst)
  (let ((count 0)) 
    (define (wrap a b)
      (set! count (+ count 1)) 
      (compare? a b)) 
    (sort wrap lst) 
    count)) 

;************************************************************
; ** problem 8 ** (20 points)

; In the Runtime lecture notes, we present a stack data structure.

(define (make-stack name (data empty))
  (let ((stack data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? stack))
        ((copy)
         (if (null? args)
             'Error:usage:copy_stack
	     (make-stack (first args) stack)))
        ((show)
	 stack)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_stack
	     (equal? stack ((first args) 'show))))
	
        ((push)
         (if (null? args)
             'Error:usage:push_element
             (begin
               (set! stack (cons (first args) stack))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? stack)
             'Error:stack-empty
	     (car stack)))
        ((pop)
         (if (null? stack)
             'Error:stack-empty
             (let ((result (car stack)))
               (set! stack (cdr stack))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))))


; Write a queue data structure, similar to the stack above.
; Whereas a stack is LIFO (last in first out), a queue is 
; FIFO = first in, first out

; Your queue data structure should implement all the same methods
; as the stack data structure.  However, push is called enqueue,
; and pop is called dequeue.  Here are examples.

; (define q1 (make-queue 'queue1))
; (q1 'name) => 'queue1
; (q1 'empty) => 'invalid-method
; (q1 'show) => '()
; (q1 'enqueue) => 'Error:usage:enqueue_element
; (q1 'enqueue 4) => 4
; (q1 'enqueue 5) => 5
; (q1 'enqueue 6) => 6
; (q1 'peek) => 4
; (q1 'enqueue '(1 2 3)) => '(1 2 3)
; (q1 'size) => 4
; (define q2 (q1 'copy 'queue2))
; (q2 'name) => 'queue2
; (q2 'empty?) => #f
; (q2 'show) => '((1 2 3) 6 5 4)
; (q1 'equal? q2) => #t
; (q2 'equal q1) => 'invalid-method
; (q2 'equal? q1) => #t
; (q1 'equal? q1) => #t
; (q1 'dequeue) => 4
; (q1 'dequeue) => 5
; (q1 'dequeue) => 6
; (q1 'dequeue) => '(1 2 3)
; (q1 'dequeue) => 'Error:queue-empty
; (q1 'size) => 0

(define (make-queue name (data empty))
  (let ((queue data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? queue))
        ((copy)
         (if (null? args)
             'Error:usage:copy_queue
             (make-queue (first args) queue)))
        ((show)
          queue)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_queue
             (equal? queue ((first args) 'show))))

        ((enqueue)
         (if (null? args)
             'Error:usage:enqueue_element
             (begin
               (set! queue (append  (list (first args)) queue))
               (set! size (+ size 1))
               (first args))))
        ((size) size)

        ((peek)
         (if (null? queue)
             'Error:queue-empty
             (last queue)))
        ((dequeue)
         (if (null? queue)
             'Error:queue-empty
             (let ((result (last queue)))
               (set! queue (drop-right queue 1))
               (set! size (- size 1))
               result)))
        (else 'invalid-method)
        ))))


;; ANSWER THIS QUESTION:
;; What is the Big-O complexity of enqueue, dequeue, size, and peek?
;
;************************************************************
; This is where the test code normally appears.
; For this assignment, write your own tests.  

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))



;; Here are some tests for the procedures with predictable results


(test 'total-order?  (total-order? <= '(1 3 5 4)) #t)
(test 'total-order?  (total-order? < '(1 3 5 4)) #f)
(test 'total-order?  (total-order? >= '(3 2 4 5 1)) #t)
(test 'total-order?  (total-order? string<=? (list "hi" "hey" "hello")) #t)
(test 'total-order?  (total-order? equal? (list "hi" "hey" "hello")) #f)

(test 'sorted? (sorted? <= '(1 4 5 8 10)) #t)
(test 'sorted? (sorted? >= '(10 9 4 7 6)) #f)
(test 'insert (insert <= 3 '(1 2 4 5)) '(1 2 3 4 5))
(test 'insert (insert string>=? "hello" (list "the" "best" "arrangement")) '("the" "hello" "best" "arrangement"))
(test 'merge (merge >= '(10 7 4 2 1) '(22 9 5)) '(22 10 9 7 5 4 2 1))
(test 'merge (merge string<=? (list "a" "novel" "thought") (list "more" "predictive")) '("a" "more" "novel" "predictive" "thought"))

(test 'isort (isort string<=? (list "predictive" "novel" "a" "more" "thought")) '("a" "more" "novel" "predictive" "thought"))
(test 'msort (msort string>=? (list "predictive" "novel" "a" "more" "thought")) '("thought" "predictive" "novel" "more" "a"))

(define q1 (make-queue 'queue1))

(test 'make-queue (q1 'name)  'queue1)
(test 'make-queue (q1 'empty)  'invalid-method)
(test 'make-queue (q1 'show)  '())
(test 'make-queue (q1 'enqueue)  'Error:usage:enqueue_element)
(test 'make-queue (q1 'enqueue 4)  4)
(test 'make-queue (q1 'enqueue 5)  5)
(test 'make-queue (q1 'enqueue 6)  6)
(test 'make-queue (q1 'peek)  4)
(test 'make-queue (q1 'enqueue '(1 2 3))  '(1 2 3))
(test 'make-queue (q1 'size)  4)
(define q2 (q1 'copy 'queue2))
(test 'make-queue (q2 'name)  'queue2)
(test 'make-queue (q2 'empty?)  #f)
(test 'make-queue (q2 'show)  '((1 2 3) 6 5 4))
(test 'make-queue (q1 'equal? q2)  #t)
(test 'make-queue (q2 'equal q1)  'invalid-method)
(test 'make-queue (q2 'equal? q1)  #t)
(test 'make-queue (q1 'equal? q1)  #t)
(test 'make-queue (q1 'dequeue)  4)
(test 'make-queue (q1 'dequeue)  5)
(test 'make-queue (q1 'dequeue)  6)
(test 'make-queue (q1 'dequeue)  '(1 2 3))
(test 'make-queue (q1 'dequeue)  'Error:queue-empty )
(test 'make-queue (q1 'size)  0)


;************************************************************


;********* end of hw8, end of hws! **************************



