#lang racket

(provide concat concat? concat-arg1 concat-arg2
         either either? either-arg1 either-arg2
         repeat repeat? repeat-arg1
         ok-string? reg-exp?
         flip pick
         generate-string-from-reg-exp
         dfa dfa? dfa-alphabet dfa-states dfa-start-state dfa-accepting-states dfa-transitions
         entry entry? entry-key entry-value
         dfa-accepts?
         cfg cfg? cfg-terminals cfg-nonterminals cfg-start-symbol cfg-rules
         rule rule? rule-lhs rule-rhs
         leaf leaf? leaf-label
         node node? node-label node-children
         list-leaf-labels
         generate-parse-tree-from-cfg generate-string-from-cfg
         dfa-mcd
         exp-mcd
         my-cfg)

; Please do not change lines above this one.

;************************************************************
; CS 201 HW #7  DUE Wednesday, November 29th at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name: Nate Ly 
; Email address: nate.ly@yale.edu
;************************************************************

; Computer science topics: strings, languages, regular expressions,
; deterministic finite state acceptors and context free grammars.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The following structs are used in the representation of Regular Expressions.

(struct concat (arg1 arg2) #:transparent)
(struct either (arg1 arg2) #:transparent)
(struct repeat (arg1) #:transparent)

; A String is a list of Racket symbols.

; A Regular Expression is defined recursively as follows:
; (1) a String is a Regular Expression,
; (2) If exp1 and exp2 are Regular Expressions, then so are
; (concat exp1 exp2), (either exp1 exp2) and (repeat exp1).
; These correspond to concatenation, union ("or"), and Kleene star
; for regular expressions.

; Examples of Regular Expressions
; These are: the empty string, the string abbab, the expression ab(c|d),
; the expression (a|b)*, and the expression ((a|the)((mouse|cat)(ran|slept)))

(define exp1 '())                                ;; ""
(define exp2 '(a b b a b))                       ;; "abbab"
(define exp3 (concat '(a b) (either '(c) '(d)))) ;; "ab(c|d)" "ab[cd]"
(define exp4 (repeat (either '(a) '(b))))        ;; "(a|b)*" "[ab]*"
(define exp5 (concat (either '(a) '(the))        ;; "(a|the)(mouse|cat)(ran|slept)"
                     (concat (either '(mouse) '(cat)) 
                             (either '(ran) '(slept)))))


;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 15)

;************************************************************
; ** problem 1 ** (9 points)
; Write two procedures

; (ok-string? value)
; (reg-exp? value)

; The procedure (ok-string? value) takes an arbitrary Racket value
; and returns #t if it is a String (that is, a list of Racket symbols)
; and #f otherwise.
; The procedure (reg-exp? value) takes an arbitrary Racket value
; and returns #t if it is a Regular Expression according to the
; definition given above, and #f otherwise.

; Examples
; (ok-string? '()) => #t
; (ok-string? '(this is one)) => #t
; (ok-string? 'no) => #f
; (ok-string? '(0 1)) => #f
; (reg-exp? exp1) => #t
; (reg-exp? exp2) => #t
; (reg-exp? exp3) => #t
; (reg-exp? exp4) => #t
; (reg-exp? 'a) => #f
; (reg-exp? "abbab") => #f
; (reg-exp? '((a b))) => #f
;************************************************************

(define (ok-string? value) ;list of racket symbols
  [cond
  [(not (list? value)) #f]
  [(empty? value) #t]
  [(symbol? (car value)) (ok-string? (rest value))]
  [else
  #f]
  ])

(define (reg-exp? value)
  [cond
  [(ok-string? value) #t]
  [(concat? value) (and (reg-exp? (concat-arg1 value)) (reg-exp? (concat-arg2 value)))]
  [(either? value) (and (reg-exp? (either-arg1 value)) (reg-exp? (either-arg2 value)))]
  [(repeat? value) (reg-exp? (repeat-arg1 value))]
  [else
  #f]

  ])

;************************************************************
; ** problem 2 ** (10 points)
; Write two procedures

; (flip bias)
; (pick lst)

; For each of these procedures you should (probably) 
; use Racket's random function.
; 
; The procedure (flip bias) simulates flipping a biased coin, 
; where the bias is specified as a number between 0 and 1,
; and the result is #t with probability equal to the bias 
; and #f with probability (1 - bias).  You can test your procedure
; by making sure that 1000 calls to (flip bias) return about
; 1000*bias values of #t.

; The procedure (pick lst) takes a list lst and returns
; a randomly chosen element of lst.  If lst is empty, the
; value returned should be #f.  
; You can test it by picking 10000 times from
; a list with 10 elements, and making sure that each element is
; picked about 1000 times.

;; Note: in the test section, we set the random-seed to guarantee
;; the same set of random numbers each time.

; Examples

; (flip .5) => #t
; (flip .5) => #f
; (map (lambda (x) (flip .5)) '(1 2 3 4 5 6 7 8 9 10)) => '(#t #t #t #f #f #f #t #f #f #t)
; (map (lambda (x) (flip .1)) '(1 2 3 4 5 6 7 8 9 10)) => '(#f #f #f #f #f #f #f #f #f #f)
; (map (lambda (x) (flip .2)) '(1 2 3 4 5 6 7 8 9 10)) => '(#t #f #f #f #f #f #t #f #f #f)
; (map (lambda (x) (flip .2)) '(1 2 3 4 5 6 7 8 9 10)) => '(#f #f #f #f #f #t #f #f #f #f)

; (pick '(1 2 3 4 5 6 8 9 10)) => 8
; (pick '(1 2 3 4 5 6 8 9 10)) => 10
; (pick '(1 2 3 4 5 6 8 9 10)) => 3
; (pick '(1 2 3 4 5 6 8 9 10)) => 2

;************************************************************

(define (flip bias)
  (equal? 0 (random 0 
  
  
  (if (exact? (/ 1 bias))
  (round (/ 1 bias))
  (inexact->exact (round (/ 1 bias)))
  )
  ))
  
    )

(define (pick lst)
  (if (empty? lst)
  #f
  (list-ref lst (random 0 (length lst)))
  
  
  ))

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (generate-string-from-reg-exp exp)

; that takes a Regular Expression exp and
; returns a random element of the language
; denoted by exp.  Every string in the language
; must have a positive probability of being chosen,
; and every string not in the language must have a
; probability of 0 of being chosen.

; Examples (yours may randomly differ):
; (generate-string-from-reg-exp exp1) => '()
; (generate-string-from-reg-exp exp2) => '(a b b a b)
; (generate-string-from-reg-exp exp3) => '(a b c)
; (generate-string-from-reg-exp exp4) => '(a)
; (generate-string-from-reg-exp exp4) => '(b a)
; (generate-string-from-reg-exp exp5) => '(the cat slept)
;************************************************************

(define (generate-string-from-reg-exp exp)
  [cond
    [(ok-string? exp) exp]
    [(concat? exp) (append
                    (generate-string-from-reg-exp (concat-arg1 exp))
                    (generate-string-from-reg-exp (concat-arg2 exp)))]
    [(either? exp) (if (flip .5)
                       (generate-string-from-reg-exp (either-arg2 exp))
                       (generate-string-from-reg-exp (either-arg1 exp)))]
    [(repeat? exp) (generate-string-from-reg-exp (repeath exp))]
    [else
     #f]
    ])
(define (repeath exp)
  (if (flip .5)
      (append (generate-string-from-reg-exp (repeat-arg1 exp)) (repeath exp))
      (generate-string-from-reg-exp (repeat-arg1 exp)))
  )
;************************************************************
; A (possibly incomplete) Deterministic Finite State Acceptor (DFA)
; is represented by the following struct.

(struct dfa (alphabet states start-state accepting-states transitions) #:transparent)

; where alphabet is a list of Racket symbols
; states is a list of Racket symbols
; start-state is one of the elements of states
; accepting-states is a list containing some of the elements of states
; and transitions is a table whose entries
;    have a key that is a list containing a state and a member of the alphabet
;         a value that is a state

(struct entry (key value) #:transparent)

; Examples of DFAs.
; Here is a DFA for the language of all strings of a's and b's with
; an even number of a's and any number of b's.

(define even-as
  (dfa
    '(a b)
    '(even odd)
    'even
    '(even)
    (list
     (entry '(even a) 'odd)
     (entry '(even b) 'even)
     (entry '(odd a) 'even)
     (entry '(odd b) 'odd))))

; Here is an (incomplete) DFA to accept the language of the
; regular expression c(a|d)(a|d)*r

(define car-cdr
  (dfa
   '(a c d r)
   '(start saw-c saw-a-or-d saw-r)
   'start
   '(saw-r)
   (list
    (entry '(start c) 'saw-c)
    (entry '(saw-c a) 'saw-a-or-d)
    (entry '(saw-c d) 'saw-a-or-d)
    (entry '(saw-a-or-d a) 'saw-a-or-d)
    (entry '(saw-a-or-d d) 'saw-a-or-d)
    (entry '(saw-a-or-d r) 'saw-r))))

;************************************************************
; ** problem 4 ** (10 points)
; Write a procedure 
;(struct dfa (alphabet states start-state accepting-states transitions) #:transparent)

; (dfa-accepts? mach str)

; to take a DFA mach and a String str and determine whether the
; DFA accepts the String.

; Examples
; (dfa-accepts? even-as '(a b b a b)) => #t
; (dfa-accepts? even-as '(b b a b b b)) => #f
; (dfa-accepts? car-cdr '(c a d a r)) => #t
; (dfa-accepts? car-cdr '(c a r d)) => #f
;************************************************************

(define (dfa-accepts? mach str)
  (sublist? (list (dfah mach str (dfa-transitions mach) (dfa-start-state mach))) (dfa-accepting-states mach))
  )
(define (dfah mach str translst state)
  [cond
    [(empty? str) state]
    [else
     (dfah mach (cdr str) translst (findnextrans translst state (car str))
           )
     ]
    ]
  )
 (define (sublist? sublist lst)
  (if (null? sublist)
      #t
      (if (in? (car sublist) lst)
          (sublist? (cdr sublist) lst)
          #f)))

(define (findnextrans translst current symbol)
  [cond
    [(empty? translst) empty]
    [(equal? (entry-key (car translst)) (list current symbol)) (entry-value (car translst))]
    [else
     (findnextrans (cdr translst) current symbol)
     ]
    ]
  )
;************************************************************
; A Context Free Grammar (CFG) is represented using the following.

(struct cfg (terminals nonterminals start-symbol rules) #:transparent)

(struct rule (lhs rhs) #:transparent)
; where
; terminals is a list of Racket symbols
; nonterminals is a list of Racket symbols
; (these two lists should have no elements in common)
; start-symbol is an element of the nonterminals list
; rules is a list of rule structs -- each of which has
; a lhs that is an element of the nonterminals list, and
; a rhs that is a list of elements from the terminals or nonterminals list
; Examples of CFGs.
; Here is the first example CFG from lecture.
(define grammar-mcd
  (cfg
   '(a the mouse cat dog it slept swam chased evaded dreamed believed that)
   '(s np vp det n pn vi vt v3)
   's
   (list
    (rule 's '(np vp))
    (rule 'np '(det n))
    (rule 'np '(pn))
    (rule 'det '(a))
    (rule 'det '(the))
    (rule 'n '(mouse))
    (rule 'n '(cat))
    (rule 'n '(dog))
    (rule 'pn '(it))
    (rule 'vp '(vi))
    (rule 'vp '(vt np))
    (rule 'vp '(v3 that s))
    (rule 'vi '(slept))
    (rule 'vi '(swam))
    (rule 'vt '(chased))
    (rule 'vt '(evaded))
    (rule 'v3 '(dreamed))
    (rule 'v3 '(believed)))))

; Here is the grammar for the set of strings consisting of
; n a's followed by n b's, for all nonnegative integers n.

(define grammar-anbn
  (cfg
   '(a b)
   '(s)
   's
   (list
    (rule 's '())
    (rule 's '(a s b)))))

;************************************************************
; A labeled Tree is defined using the following two structs.

(struct node (label children) #:transparent)
(struct leaf (label) #:transparent)

; A labeled Tree is either a leaf with a label
; or a node with a label and a list of labeled Trees as its children.

; Example of a tree.

(define tree1
  (node 'a
        (list
         (node 'b
               (list
                (leaf 'c)
                (leaf 'd)))
         (node 'e
               (list
                (node 'f
                      (list
                       (leaf 'g)
                       (leaf 'h)))
                (leaf 'i))))))

;************************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (list-leaf-labels tree)

; to return a list of the leaf labels of a labeled Tree.

; Example
; (list-leaf-labels tree1) => '(c d g h i)
;************************************************************

(define (list-leaf-labels tree)
  [cond
  [(list? tree) (listh tree)]
  [(symbol? tree) (list tree)]
  [(leaf? tree) (list-leaf-labels (leaf-label tree))]
  [(node? tree) (list-leaf-labels (node-children tree))]
  ])
(define (listh lst)
[cond 
[(empty? lst) empty]
[else
(append (list-leaf-labels (car lst)) (listh (cdr lst)))
]

]
)
;************************************************************
; ** problem 6 ** (15 points)
; Write two procedures 

; (generate-parse-tree-from-cfg grammar)
; (generate-string-from-cfg grammar)

; The procedure (generate-parse-tree-from-cfg grammar)
; takes a CFG grammar and produces a randomly chosen parse Tree from grammar,
; in which all of the node labels are nonterminal symbols and all of
; the leaf labels are terminal symbols.  Every possible such parse tree
; should have a positive probability of being generated, and every other
; parse tree should have probability 0 of being generated.

; The procedure (generate-string-from-cfg grammar)
; takes a CFG grammar and produces a randomly chosen String from grammar.
; Every String in the language of the grammar should have a non-zero
; probability of being generated, and every String not in the language
; should have probability 0 of being generated.

; (Hint: if you can generate a random parse tree, what procedure
; could you apply to generate a String from it?)

; Example (yours may randomly differ):
;> (generate-parse-tree-from-cfg grammar-mcd)
;(node
; 's
; (list
;  (node 'np (list (node 'pn (list (leaf 'it)))))
;  (node
;   'vp
;   (list
;    (node 'v3 (list (leaf 'dreamed)))
;    (leaf 'that)
;    (node
;     's
;     (list
;      (node 'np (list (node 'pn (list (leaf 'it)))))
;      (node 'vp (list (node 'vi (list (leaf 'swam)))))))))))

; (generate-string-from-cfg grammar-mcd) => '(it swam)
; (generate-string-from-cfg grammar-anbn) => '(a b)
; (generate-string-from-cfg grammar-anbn) => '(a a b b)
; (generate-string-from-cfg grammar-anbn) => '()
;************************************************************
(define (generate-parse-tree-from-cfg grammar)
  (node
   (cfg-start-symbol grammar)
   (genh grammar (rule-rhs (pick (ruleslst (cfg-start-symbol grammar) (cfg-rules grammar)))))
   )
   )

(define (genh grammar right)
  [cond
    [(empty? right) '()]
    [(in? (car right) (cfg-terminals grammar)) (cons (leaf (car right)) (genh grammar (cdr right)))]
    [else
     (cons (node (car right) (genh grammar (rule-rhs (pick (ruleslst (car right) (cfg-rules grammar))))))
           (genh grammar (cdr right)))]
  ]
           )
(define (ruleslst symbol rules)
  (cond
    [(null? rules) '()]
    [(equal? (rule-lhs (car rules)) symbol)
     (cons (car rules) (ruleslst symbol (cdr rules)))]
    [else (ruleslst symbol (cdr rules))]
    )
  )
(define (in? value lst)
  (cond
    [(null? lst) #f]
    [(equal? value (car lst)) #t]
    [else (in? value (cdr lst))]
    )
  )

(define (generate-string-from-cfg grammar)
(list-leaf-labels (generate-parse-tree-from-cfg grammar)))
;************************************************************
; ** problem 7 ** (10 points)
; Define a DFA named dfa-mcd to recognize the language of 
; the CFG grammar-mcd.
;************************************************************

(define dfa-mcd
  (dfa
   '(a the mouse cat dog it slept swam chased evaded dreamed believed that)
   '(s np vp det n pn vi vt v3) 
   's 
   '(vi pn n) 
   (list
    (entry '(s it) 'vp)
    (entry '(s a) 'sn)
    (entry '(s the) 'sn)
    (entry '(sn mouse) 'vp)
    (entry '(sn cat) 'vp)
    (entry '(sn dog) 'vp)
    (entry '(vp slept) 'vi)
    (entry '(vp swam) 'vi)   
    (entry '(vp chased) 'np)
    (entry '(vp evaded) 'np) 
    (entry '(np it) 'pn)  
    (entry '(np a) 'det)
    (entry '(np the) 'det)
    (entry '(det mouse) 'n)
    (entry '(det cat) 'n)
    (entry '(det dog) 'n)
    (entry '(vp dreamed) 'tht)
    (entry '(vp believed) 'tht)
    (entry '(tht that) 's)
   )))

;************************************************************
; ** problem 8 ** (10 points)
; Define a Regular Expression named exp-mcd to denote the language of 
; the CFG grammar-mcd.
;************************************************************
(define exp-mcd
(let* (
[np (either (concat (either '(a) '(the)) (either '(mouse) (either '(cat) '(dog)))) '(it))]
[vp (either (either '(slept) '(swam)) (concat (either '(chased) '(evaded)) np))]
[vt  (repeat (concat (concat (either '(believed) '(dreamed)) '(that)) np))]
)
  (concat (either np (concat np vt)) vp)
))

  
  
;************************************************************
; ** problem 9 ** (15 points)
; Define your own CFG named my-cfg of complexity at least as great as 
; that of grammar-mcd.  Give (as comments) some examples of sentences 
; generated by your grammar. 
; Note: your grammar should have a recursive element.
; (Please do more than just copy grammar-mcd with a few changes.)
;************************************************************
;example sentences below
;a trhyard fortnite player quickly eliminated Renegade Raider
;a good fortnite player easily revived Renegade Raider and the noob
;Skull Trooper and a good fortnite player quickly ran away
;Renegade Raider and a trhyard fortnite player slowly healed and Skull Trooper quickly revived Skull Trooper
(define my-cfg
 (cfg
   '(a the good fortnite player
    trhyard fortnite player quickly
     noob Renegade Raider Skull Trooper bravely easily
    and slept swam eliminated revived healed shielded up bush camped ran away slowly)
   '(s np vp det n pn vi vt v3 adv)
   's
   (list
    (rule 's '(np adv vp))
    (rule 'adv '(slowly))
    (rule 'adv '(quickly))
    (rule 'adv '(bravely))
    (rule 'adv '(easily))
    (rule 'np '(det n))
    (rule 'np '(pn))
    (rule 'np '(pn and det n))
    (rule 'det '(a))
    (rule 'det '(the))
    (rule 'n '(good fortnite player))
    (rule 'n '(trhyard fortnite player))
    (rule 'n '(noob))
    (rule 'pn '(Renegade Raider))
    (rule 'pn '(Skull Trooper))
    (rule 'vp '(vi))
    (rule 'vp '(vt np))
    (rule 'vp '(v3 and s))
    (rule 'vi '(bush camped))
    (rule 'vi '(ran away))
    (rule 'vt '(eliminated))
    (rule 'vt '(revived))
    (rule 'v3 '(healed))
    (rule 'v3 '(shielded up)))))

;************************************************************



; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

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
	
(test 'hours hours (lambda (x) (> x 0)))

(test 'ok-string? (ok-string? '()) #t)
(test 'ok-string? (ok-string? '(this is one)) #t)
(test 'ok-string? (ok-string? 'no) #f)
(test 'ok-string? (ok-string? '(0 1)) #f)
(test 'reg-exp? (reg-exp? exp1) #t)
(test 'reg-exp? (reg-exp? exp2) #t)
(test 'reg-exp? (reg-exp? exp3) #t)
(test 'reg-exp? (reg-exp? exp4) #t)
(test 'reg-exp? (reg-exp? 'a) #f)
(test 'reg-exp? (reg-exp? "abbab") #f)
(test 'reg-exp? (reg-exp? '((a b))) #f)

;; NOTE: test results would normally vary due to random elements
;; However, by setting the random-seed, we generate the same sequence of 
;; pseudo random numbers every time.

;(random-seed 100)

; (test 'flip (map (lambda (x) (flip .5)) '(1 2 3 4 5 6 7 8 9 10)) '(#t #f #t #f #f #t #t #f #t #f))
; (test 'pick (pick '(1 2 3 4 5 6 7 8 9 10)) 4)

(test 'flip-a-lot (let ((nots (length (filter not (map flip (build-list 1000 (lambda (x) 0.5))))))) (and (< 450 nots) (> 550 nots))) #t)

(test 'pick-a-lot (let ((total (apply + (map pick (build-list 1000 (lambda (x) '(1 2 3 4 5 6 7 8 9 10))))))) (and (< 5000 total) (> 6000 total))) #t)

(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp1) '())
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp2) '(a b b a b))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp3) '(a b d))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp4) '(b b a))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp4) '(b))
(test 'generate-string-from-reg-exp (generate-string-from-reg-exp exp5) '(the cat ran))

(test 'dfa-accepts? (dfa-accepts? even-as '(a b b a b)) #t)
(test 'dfa-accepts? (dfa-accepts? even-as '(b b a b b b)) #f)
(test 'dfa-accepts? (dfa-accepts? car-cdr '(c a d a r)) #t)
(test 'dfa-accepts? (dfa-accepts? car-cdr '(c a r d)) #f)

(test 'list-leaf-labels (list-leaf-labels tree1) '(c d g h i))

(test 'generate-parse-tree-from-cfg (generate-parse-tree-from-cfg grammar-mcd)
 (node
  's
  (list
   (node 'np (list (node 'pn (list (leaf 'it)))))
   (node 'vp (list (node 'vi (list (leaf 'swam))))))))

(test 'generate-string-from-cfg (generate-string-from-cfg grammar-mcd) '(a cat evaded it))
(test 'generate-string-from-cfg (generate-string-from-cfg grammar-anbn) '(a a a b b b))
(test 'generate-string-from-cfg (generate-string-from-cfg grammar-anbn) '(a a a b b b))
(test 'generate-string-from-cfg (generate-string-from-cfg grammar-anbn) '(a a b b))


;********************** end of hw7.rkt **********************

