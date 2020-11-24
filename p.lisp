#|
Cmput 325 Winter 2019 Assignment 2
Anton Shlakhter
|#


;----------------------Question 1


#|
solvebasic is a relatively simple function that has a single parameter, which is an A2expr exclusively  of the form (operator atom1 atom2)

If the operator is not '+, '- or '*, we return the expression, unchanged.
If the operator is '+ and either atom is 0, we return just the other atom. Else, we return the entire expression, unchanged
If the operator is '* and either atom is 1, we return just the other atom. Else, we return the entire expression, unchange
If the operator is '- and atom2 is 0, we return atom1. Else, we return the entire expression, unchanged.

|#



(defun solvebasic (e)
  (cond 
   ((eq (car e) '*)
    (cond ((eq (cadr e) '1)
	   (caddr e))
	  
	  ((eq (caddr e) '1)
	   (cadr e))
	  
	   (T
	    e)))
   
   ((eq (car e) '+)
    (cond ((eq (cadr e) '0)
	   (caddr e))

	   ((eq (caddr e) '0)
	   (cadr e))
	  
	   (T
	   e)))
	
   ((eq (car e) '-)
    (if (eq (caddr e) '0)
	(cadr e)
      e)
    )

   (T
    e)))

#| 
remove-identites takes a single parameter, an a2-expresion. There are 3 possible cases.
Case 1: entire expression is a single atom, ex 5. It obviously cannot be simplified, so is returned as is

Case 2: expression is of form (operator e1 e2), where both e1 and e2 are atoms. We call solvebasic to see if it can simplify it

Case 3: expression is of form (operator e1 e2), where either e1 or e2 or both are not atoms. That means that there is a possiblity that
they can be reduced further, which the function does by recursive calling itself with e1 or e2 as input, as needed.

Since the needed depth of recursion is unknown, and we do not want an infinite loop of attempts to reduce the expression, we stop
the recursion when an additional run of the program produces the same result E as it started with.

|#

(defun remove-identities (e)
  (cond ((atom e) e) 
	
	((and  (not (atom (cadr e))) (not (atom (caddr e))))	 
	 (if (equal e (append (list(car e)) (list (remove-identities (cadr e))) (list(remove-identities (caddr e)))))
	     e
	   (remove-identities (append (list(car e)) (list (remove-identities (cadr e))) (list(remove-identities (caddr e)))))))

	
	((not (atom (cadr e))) 
	 (if (equal e (append (list(car e)) (list(remove-identities (cadr e))) (list(caddr e))))
	     e
	   (remove-identities (append (list(car e)) (list(remove-identities (cadr e))) (list(caddr e))))))

	((not (atom (caddr e)))
	 (if (equal e (append (list(car e)) (list(cadr e)) (list(remove-identities (caddr e)))))
	     e
	   (remove-identities (append (list(car e)) (list(cadr e)) (list(remove-identities (caddr e)))))))

	(T
	 (solvebasic e))))


;----------------------------------Question 2

#|
simplifybasic works almost identically to solvebasic from question 1. Input is strictly (operator atom1 atom2).
if operator is not '- or '*, return E, unchanged.
If operator is '- and atom1 = atom1, return 0. Else, return E unchanged
If operator is '* and either atom1 or atom2 is 0, return 0. Else, return E unchanged.


|#


(defun simplifybasic (e)
(cond 
 ((eq (car e) '*)
  (cond ((eq (cadr e) '0)
	 '0)
	
	((eq (caddr e) '0)
	 '0)
	
	(T
	 e)))
 
 ((eq (car e) '-)
  (if (equal (caddr e) (cadr e))
      '0
    e)
  )
 
 (T
  e)))


#| 
simplify-zeroes works nearly identically to remove-identities in question 1. Only addition/change other than names of function and its helper function
is that we take into account the fact that sometimes we can make an entire outer expression equal to 0 immediately, without needing any deeper recursion. 
These cases are the three highest ones in the cond statement.
|#

(defun simplify-zeroes (e)
  (cond ((atom e) e)  
	
	((and (eq (car e) '-) (equal (cadr e) (caddr e))) 0)
	
	((and (eq (car e) '*) (equal (cadr e) 0) 0))

	((and (eq (car e) '*) (equal (caddr e) 0) 0))

	((and  (not (atom (cadr e))) (not (atom (caddr e))))
	 (if (equal e (append (list(car e)) (list (simplify-zeroes(cadr e))) (list(simplify-zeroes (caddr e)))))
	     e
	   (simplify-zeroes (append (list(car e)) (list (simplify-zeroes(cadr e))) (list(simplify-zeroes (caddr e)))))))	
	
	((not (atom (cadr e))) 
	 (if (equal e (append (list(car e)) (list(simplify-zeroes(cadr e))) (list(caddr e))))
	     e
	   (simplify-zeroes (append (list(car e)) (list(simplify-zeroes(cadr e))) (list(caddr e))))))
	
	((not (atom (caddr e)))
	 (if (equal e (append (list(car e)) (list(cadr e)) (list(simplify-zeroes (caddr e)))))
	     e
	   (simplify-zeroes(append (list(car e)) (list(cadr e)) (list(simplify-zeroes (caddr e)))))))
	
	(T
	 (simplifybasic e))))


;-------------------------Question 3

#| 
Fairly simple implementation. We do not know the needed recursion depth, aka number of times we need
to use remove-identities or simplify-zeroes, or in what order. 

Therefore function recursively calls itself on output of remove-identities or simplify-zeroes. 
If it cannot be simplified by remove-identities we try to simplify it by simplify-zeroes. 
If neither function gives an output that is different to the e that that iteration of simplify 
started with, we know we are done, so we return that e as the answer.
|#

(defun simplify (e)
  (cond 
   ((not (equal e (remove-identities e)))
    (simplify (remove-identities e)))
   ((not (equal e (simplify-zeroes e)))
    (simplify (simplify-zeroes e)))
   (T
     e)))



#|
------------------------ Question 3. ANSER TO BONUS QUESTION:
-Does the order of calls to remove-identities and simplify-zeroes in simplify matter?
-Could we get a different final result in simplify if we called these two functions in a different order?

First, let us answer the second part of the question:

No, we cannot get a differnet final result in simplify if we called those two functions in a different order.
The reason for that is, there isn't a single possible expression that can be simplified by BOTH functions, except (- 0 0),
which both functions evaluate to 0. Since the 2 functions cannot both change any one a2expr into one different from one
another, and we keep performing reductions until we no longer can perform EITHER reduction, any simplifiable A2expr, will 
eventually be evaluated by the function that can simplify it, until we are left with just the terms that cannot be simplified
by either function. 



However, the answer to the first part of the question is actually, yes. At least, in terms of performance/number of
function calls. Simplify-zeroes does not always need to go into the nested sub-expressions to reduce the outer expressions, 
while remove-identities always does. 

For example, let EXPR be some complicated, deeply nested A2expr. If we have:

an a2exp: (* 0 EXPR) or (* EXPR 0) or (- EXPR EXPR), we do not need to perform simplification of EXPR to know that our 
final result will always be 0, regardless of what EXPR may simplify into, by properties of multiplication and subtraction.

In an analogous situation for remove-identities, the first outer operation would only tell us that our final result
will be whatever EXPR simplifies to, and we would still need to perform all the operations needed to simplify EXPR
into it's final form, thus, assuming roughly equal distribution of terms simplifiable by each function, we are more
likely to reach the final result faster by evaluating simplify-zeroes first.

|#

   

;-------------------------------- Question 4

#|
Takes a Pexp that is has been already sorted. Return the same P, minus any terms that have O-coefficents. If the entire P is just (0 . 0), returns nil instead
Works by recursively calling itself with a (rest P) as new P. Recursion stops if there is just one term left in P. 
|#

(defun zeroes (P)
  (cond ((equal P '((0 . 0)))
	 nil)
	((null (cdr P))
	 (if (equal (caar P) 0)
	     nil
	   p))
	((not(equal (caar P) '0))
	 (cons(car P) (zeroes (cdr P))))
	(t (zeroes (cdr P)))))
	
#| 
Custom sorting function. Sorts by highest cdr term, therefore dotted pairs with higher cdr, or power, end up first, followed by terms with lower cdr/power. 
|#

	
(defun exponent-order (term1 term2)
  (> (cdr term1) (cdr term2)))


#| 
Accepts as input 2 dotted pairs. It's assumed that p1 is a valid pair, but p2 might be null, in which case we just return p1 (any number + 0 = that number)
If the two pair have the same cdr term or power, we return a single dotted pair in a list that is the sum of their car terms (coefficents), and their shared
cdr term (power). Else return both, unchanged.

some tests cases to explain better:

(combine-like-terms-helper '(2 . 5) '()          -> '((2 . 5))
(combine-like-terms-helper '(2 . 5) '(-3 . 5)    -> '((-1 . 5))
(combine-like-terms-helper '(2 . 5) '(-3 . 2)    -> '((2. 5) (-3 . 2))

|#

(defun combine-like-terms-helper (p1 p2)
  (if (null p2)
      (list p1)
    (if (eq (cdr p1) (cdr p2))
	(list(cons (+ (car p1) (car p2)) (cdr p1)))
      (list p1 p2))))


#|
Given list of dotted pairs P, groups them in pairs (first and second, third and fourth, fifth and sixth, etc).
For each such pair, calls combine-like-terms-helper. 
There are two flaws here, that will need to be fixed in a later function:
flaw 1: we have more than 2 terms with the same power in a row. 
flaw 2: we have terms that have same power that are not grouped together (for example, term 2 and 3 wouldn't be called together
for combine-like-terms-helper.

This is quite confusing, so here are some sample outputs highlighting successes and flaws:

(combine-like-terms '((1 . 2) (1 . 2)(3 . 1) (2 . 1)))         ->         '((2 . 2)(5 . 1))
(combine-like-terms '((1 . 2) (1 . 1)(2 . 1)) -> '((2 . 2)))   ->         '((1 . 2) (2 . 1) (2. 1))
(combine-like-terms '((1 . 2)(1 . 2)(1 . 2)(1 . 2)))           ->         '((2 . 2)(2 . 2))
|#

(defun combine-like-terms (P)
  (if (null(cddr P))
      (if (equal 1 (length (combine-like-terms-helper (car P) (cadr P))))
	  (list(car(combine-like-terms-helper (car P) (cadr P))))
	(combine-like-terms-helper (car P) (cadr P)))
    (append (combine-like-terms-helper (car P) (cadr P)) (combine-like-terms (cddr P)))))



#| 
A fix to flaw 1 from my implementation of combine-like-terms. If performing an additional run of combine-like-terms changes
the P, do it. Else, just return P. 
|#

(defun flaw1 (P)
  (if (not(equal P (combine-like-terms P)))
      (flaw1 (combine-like-terms P))
    P))



#| 
A fix to flaw 2 from my implementation of combine-like-terms. This, as well as flaw1 require a long explanation. 

We need to run the code in flaw1 EXACTLY TWICE over our list to remove any possibility of error, the first time on the entire list,
the second on the entire list, excluding the first element. We can exclude the first element the second time around, because after 
the first execution of the code in flaw1 (technically, in flaw2 function, but it's the same code), the first element is guranteed
to have a different power than the second element. 

This shift removes the possibilities of same powers between 2nd and 3rd elements, 4th and 5th elements, 6th and 7th, etc...
We need to perform this shift only once, because pairings between 1st and 2nd, 3rd and 4th, 5th and 6th, etc, were already
handled by combine-like-terms and flaw1-like code. 

In summary, flaw2 takes a sorted list of dotted pairs, and continuously performs combine-like-terms on them, until doing so wouldn't
change the result. Then, it does the same thing, on rest(P). The output of flaw2 would be a sorted list of dotted pairs which have
unique powers each, but can still have zero-coefficients, that will need to be removed later by zeroes function.
|#

(defun flaw2 (P)
  (cond ((not(equal (combine-like-terms P) P))
	 (flaw2 (combine-like-terms P)))
	((null (cdr P))
	 P)
	(t
	 (cons (car (combine-like-terms P)) (flaw1 (cdr P))))))

	

#| 
First, sorts P according to highest power first using exponent-order
Then, combines any terms with the same power using flaw2
Finally, removes any terms with zero-coefficents with zeroes.
|#

(defun normalize (P)
  (zeroes (flaw2 (sort P 'exponent-order))))



;-------------------------------Question 5

#| 
Normalize already handles addition. Just append p1 and p2 and call normalize with the resulting P
|#

(defun poly-add (P1 P2)
  (normalize (append P1 P2)))


#| 
Given a dotted pair, negates the car element of it.
|#

(defun negate-helper (P)
  (cons (* -1 (car P)) (cdr P)))

#|
Recursively builds a new list of dotted pairs, with the help of negate-helper, the car of each dotted pair being * -1
that of the original P
|#

(defun negate (P)
  (if (null (cdr P))
      (list(negate-helper (car P)))
    (append (list(negate-helper (car P))) (negate (cdr P)))))


#| 
Since subtracting A from B is equal to adding -1 * A to B, that's what we do, using negate on P2
|#

(defun poly-subtract (P1 P2)
  (poly-add P1 (negate P2)))


#| 
Takes two dotted pairs and multiplies them. This is done by multiplying the car terms, and adding the cdr terms,
and creating a new base pair with the resulting car and cdr.
|#

(defun basic-multiply (P1 P2)
  (cons (* (car P1) (car P2)) (+ (cdr P1) (cdr P2))))


#| 
Given an single dotted pair P1, and a list of dotted pairs P2, multiplies every pair in P2 by P1
and returns the result as a new list of dotted pairs
|#

(defun multiply-left (P1 P2)
  (if (null (cdr P2))
      (list(basic-multiply P1 (car P2)))
    (append (list (basic-multiply P1 (car P2))) (multiply-left P1 (cdr P2)))))


#| 
Given a list of dotted pairs, multiplies every element of P1 by every element of P2
and returns the result a single list of dotted pairs
|#

(defun multiply-right (P1 P2)
   (if (null (cdr P1))
      (multiply-left (car P1) P2)
    (append (multiply-left (car P1)  P2) (multiply-right (cdr P1) P2))))


#| 
If either P is nil, return nil. Else, call multiply-right with the 2 Ps, then normalize the result
|#

(defun poly-multiply (P1 P2)
  (cond ((null P1) nil)
	((null P2) nil)
	(t (normalize (multiply-right P1 P2)))))


#| 
Recursively multiplies, subtracts or adds, depending on operator. Special base case for integer, and one for 'x. 
Returns P-exp
|#

(defun Polynomial (E)
  (cond ((integerp E)
	 (list(cons E 0)))
	
	((atom E)
	 (list(cons 1 1)))

	((equal (car E) '*)
	 (poly-multiply (Polynomial (cadr E)) (Polynomial (caddr E))))
	 
	((equal (car E) '-)
	 (poly-subtract (Polynomial (cadr E)) (Polynomial (caddr E))))

	((equal (car E) '+)
	 (poly-add (Polynomial (cadr E)) (Polynomial (caddr E))))
))







;----------------------------------Question 6

;helper function taken from hints on assignment page

(defun concat (s1 s2)
  (concatenate 'string s1 s2))


#| 
termstring and onlyterm-string are essentially the same function, but return a slightly different output
because you (1 . 1) is "x" if it's the only term, but "+ x" if there is one before it, for example. Termstring
will be called once for every term except the first in print-pexpr

Takes a single dotted pair and returns the output as a string in normal form
|#

(defun termstring (pair)
  (cond 
   
   ;zero
   ((eq pair nil)
    "")

   ;constant positive terms
   ((and (eq '0 (cdr pair)) (> (car pair) 0))
    (concat " + " (write-to-string(car pair))))

   ;constant negative terms
   ((and (eq 0 (cdr pair)) (< (car pair) 0))
    (concat " - " (write-to-string(* -1 (car pair)))))
   

   ;1 * x^1
   ((and (eq 1 (cdr pair)) (eq (car pair) 1))
    " + x")

   ;-1 * x^1
   ((and (eq 1 (cdr pair)) (eq (car pair) -1))
    " - x")


   ;c * x^1, c > 1
   ((and (eq 1 (cdr pair)) (> (car pair) 1))
    (concatenate 'string " + " (write-to-string (car pair))"x"))

   ;c * x^1, c < -1
   ((and (eq 1 (cdr pair)) (< (car pair) -1))
    (concatenate 'string " - " (write-to-string (* -1 (car pair)))"x"))


   ;1 * x^n, n > 1
   ((eq (car pair) 1)
    (concat " + x^" (write-to-string (cdr pair))))
    
   ;-1 * x^n, n > 1
   ((eq (car pair) -1)
    (concat " - x^" (write-to-string (cdr pair))))

   ;c * x^n, c > 1, n > 1 
   ((and (< 1 (cdr pair)) (> (car pair) 1))
    (concatenate 'string " + " (write-to-string(car pair)) "x^" (write-to-string(cdr pair))))

    ;c * x^n, c < -1, n > 1 
   ((and (< 1 (cdr pair)) (< (car pair) -1))
    (concatenate 'string " - " (write-to-string(* -1 (car pair))) "x^" (write-to-string(cdr pair))))

))


#| 
Almost identical to termstring, except called only for first (and/or only) term in manytermstring
|#

(defun onlytermstring (pair)
  (cond 
   
   ;zero
   ((eq pair nil)
    "0")

   ;constant positive terms
   ((and (eq '0 (cdr pair)) (> (car pair) 0))
    (write-to-string(car pair)))

   ;constant negative terms
   ((and (eq 0 (cdr pair)) (< (car pair) 0))
    (concat "-" (write-to-string(* -1 (car pair)))))
   

   ;1 * x^1
   ((and (eq 1 (cdr pair)) (eq (car pair) 1))
    "x")

   ;-1 * x^1
   ((and (eq 1 (cdr pair)) (eq (car pair) -1))
    "-x")

   ;c * x^1, c > 1
   ((and (eq 1 (cdr pair)) (> (car pair) 1))
    (concat (write-to-string (car pair))"x"))

   ;c * x^1, c < -1
   ((and (eq 1 (cdr pair)) (< (car pair) -1))
    (concatenate 'string "-" (write-to-string (* -1 (car pair)))"x"))
   

   ;1 * x^n, n > 1
   ((eq (car pair) 1)
    (concat "x^" (write-to-string (cdr pair))))
    
   ;-1 * x^n, n > 1
   ((eq (car pair) -1)
    (concat "-x^" (write-to-string (cdr pair))))

   ;c * x^n, c > 1, n > 1 
   ((and (< 1 (cdr pair)) (> (car pair) 1))
    (concatenate 'string (write-to-string(car pair)) "x^" (write-to-string(cdr pair))))

    ;c * x^n, c < -1, n > 1 
   ((and (< 1 (cdr pair)) (< (car pair) -1))
    (concatenate 'string "-" (write-to-string(* -1 (car pair))) "x^" (write-to-string(cdr pair))))
))

#|
called for every dotted pair except the first from P in print-pexpr. Creates a string with output in normal form using
recursion and termstring 
|#

(defun manytermstring (P)
  (if (null P)
      ""
    (concat (termstring (car P)) (manytermstring (cdr P)))))

#|
Calls onlytermstring for first (and/or only) dotted pair, and manytermstring for every other dotted pair in P.
Returns string containing normal form representation of P.
 
|#

(defun print-pexpr (P)
  (if (null(cdr P))
      (onlytermstring (car P))
    (concat (onlytermstring (car P)) (manytermstring (cdr P)))))    
