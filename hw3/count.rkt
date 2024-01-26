#lang racket

(define (count tree)
  (cond
    [(empty? tree) 0]
    [(not (list? tree))
     (if (number? tree) 1 0)
     ]
    [else
     (+ (count (car tree)) (count (cdr tree)))
     ]
    )
  )
(count '( 1 2 (3 (4)) 'l ffff ))