#|
Cmput 325 Winter 2019 Assignment 1
Anton Shlakhter
|#
#|
--------------------Question 1-------------------------------------------

The function issorted takes a single parameter, L, a list of numbers
and recursively itterates through it. This itteration only happens
if the second element (cadr L) is bigger than the first element (car L).
Otherwise, the function returns false. If the function hits the base case,
an empty list, that means that the entire list was sorted, and the 
function returns true.

|#


(defun issorted (L)
  (if (null (cdr L))
      t
    (if (< (car L) (cadr L))
	(issorted (cdr L)))))

#|
-------------------Question 2------------------------------------------
The function myreverse is basically translated from the one written in fun
in the slides, so I don't think needs any explaining. 
It is used in both question 2 and 3. 
|#


(defun myreverse (L)
  (if (null L)
      '()
    (append (myreverse(cdr L))(cons(car L) nil))))


#| 
Most of the work for question 2 is done by buildnlist. Given an integer,
it recursively builds a list, by adding N to the front of the list, and
calling itself with new N = old N - 1. This happens until we hit the base
case, where N is equal to 0, and the function returns just returns nil

in case this description is not clear, a test case for it is:

 (buildnlist 5) => (5 4 3 2 1)

|#

(defun buildnlist (N)
  (if (= N 0)
      '()
    (cons N (buildnlist (- N 1)))))

#| 
Since we want the list to start at 1 and not N, we just call buildnlist with N
and afterwards, use myreverse to reverse it.

|#


(defun numbers (N)
  (myreverse (buildnlist N)))



#| --------------------------------Question 3---------------------------
This question is very simple. How do we know that something is a palindrone?
If when read backwards and forwards, it is the same word. We just use the
myreverse function form Question 2 to reverse the list, and equal to compare
the list itself with the reversed list to see if they are equal, returning
true or false depending on if it is or not respectively. 


|#


(defun palindrone (L)
  (equal L (myreverse L)))




#| 
--------------------------------Question 4-----------------------------------
Replaceletter is a helper function for replace1. It is input with 3 atoms.
If the atom Letter is equal to atom1, it returns atom2. Otherwise it returns
Letter, unchanged. If Letter is not an atom, replaceletter will always return
it unchanged. 

|#

(defun replaceletter (atom1 atom2 Letter)
  (if (eq atom1 Letter)
      atom2 
    Letter))
#| 
Replace1 is identical to replace2, with the exception that it calls a
different helper function. If the inputed list is null, it returns null.
Otherwise, it recursively itterates through the list, each time calling itself
with rest(list), or in other words, with the list with removed first character.
Each iteration, it uses the helper function replaceletter to change the first
character, if needed. All the letters end up in a single list using cons.
|#


(defun replace1 (atom1 atom2 List)
  (if (null List)
    List
    (cons(replaceletter atom1 atom2 (car List)) (replace1 atom1 atom2(cdr List)))))

#| 
Works very similarly to replaceletter. The one difference, however, is that
it checks to see if Letter is an atom or not first. If it is, replaceletter2
does the same thing as replaceletter. 

If it isn't, then Letter must be a list,
and replacelist2 is called, with the same atom1, atom2, but the list Letter
as the parameter for list. Replace2 will call replaceletter2 again, during the
iterations of the Letter list. This allows for replacement of letters within
lists of lists of lists [...] of lists to maximum depth of lists. 


|#

(defun replaceletter2 (atom1 atom2 Letter)
  (if (not(atom Letter))
      (replace2 atom1 atom2 Letter)
    (if (eq atom1 Letter)
	atom2 
      Letter)))

#| 
Functionally identical to replace1. If list is null do nothing, else
recurse through the list, each time using replaceletter2 to replace atoms as 
needed. If (car List) is not an atom but is rather a list, replace2 will be 
called by replaceletter2, with that list in place of the List parameter. 
Thus, each level of depth for a given list requires a new call of replace2.
This seems like good design to me because with each pass, we simply the problem
a little, until it is simple enough to solve.


A special note here: because of the (allowed, as per response on forum by prof)
mutually recursive relationship with replaceletter2, the first time this file
is loaded by sbcl, there will be a warning, as the function replace2 will have 
not been declared when replaceletter2 is being scanned, however, if you load 
the file a second time, there will be no more warnings. I assume there is no 
way to load in mutually recursive functions without that warning. 
|#

(defun replace2 (atom1 atom2 List)
  (if (null List)
    List
    (cons(replaceletter2 atom1 atom2 (car List)) (replace2 atom1 atom2(cdr List)))))



#|------------------------------Question 5--------------------------------
The entirety of question 5 can be completed in just one function. 

common iteratively compares each single element of L1 against every element
of L2, using the member function. Since both lists are assumed to not have
multiple of the same element, we don't care what member returns, just if it is
nil or not.

If L1 is null, there can be no common elements between it and L2, thus we 
return 0. 

A second base case is if L1 has just 1 element, where we just
need to use member on the element and L2, returning 1 if true, and 0 if false

If L2 has more than one element, we will go with the third branch, which 
recursively iterates through L1 giving each element a score of 1 or 0, based
on if it is found in L2. The score of each element is added together at the 
end, and will equal the number of elements that L1 and L2 share in common.


|#

(defun common (L1 L2)
  (cond ((Null L1)
      0)
      ((Null (cdr L1))
      (if (not (null(member (car L1) L2)))
 	  1
 	0))
	
      (T ( + (common (cdr L1) L2) (if (not (null(member (car L1) L2)))
				      1
				    0)))))	




#|------------------Question 6------------------------------------------
This problem required very many new helper functions to be made, as well as
common and numbers functions from previous questions. Each one will explained
seperately, as well as how they all come together in mymain.




Callcommon is the first new function, the purpose of which is to take 2 lists, 
the first of which is a list of lists (S). The structure of this fuction is 
fairly simple. 2 bases cases of if S is empty of has just one list element,
and a recursive case for if it has more than 1 list element. Essentially, 
callcommon iterates through S, calling common with each element in S, and
finding how many elements it has in common with keylist, and returns a list
which contains the values of the outputs of common for each list element in S
respectively.

Using next few functions, we will use this list to determine which list element
of S is the best one to select for greedy cover.

Some test cases to show you have callcommon works:

 (callcommon '((1 2 3) (4 5) (1 5) (1 6)) '(1 2 3 4 5)) => (3 2 2 1)
 (callcommon '((1 2 4) (11 16) (3)) '(1 2 3) => (2 0 1)

|#

(defun callcommon (listlists keylist)
  (cond ((null listlists)
	 "no list given to compare to.")
	 ((null (cdr listlists))
	  (list(common (car listlists) keylist)))
	  (T (cons(common (car listlists) keylist) (callcommon (cdr listlists) keylist)))
)
)	


#| 
Function to remove first instance of an element in a list, and return complete list without it as outputlist
Outputlist starts as nil, grows with each iteration. If completelist (the list from which we are subtracting)
is null, return it. 

If first element of completelist is equal to element which we want to remove, append rest of list to 
outputlist and return outputlist. Otherwise, keep iterating through the list, recursively calling itself
with same element, outputlist updated with first element of completelist each time, and completelist
that is rest(completelist). 

some test cases:

 (myremove 'a '(b a c) nil) => (b c)
 (myremove 'a '(b a c a) nil) => (b c a)
 (myremove 'a '(b d a) nil) => (b d a)


|#


(defun myremove (element completelist outputlist)
  (if (null completelist)
      outputlist
    (if (equal element (car completelist))
	(append outputlist (cdr completelist))
      (myremove element (cdr completelist) (append outputlist (list(car completelist)))))))


#| 
Function to compute set difference. Given two lists, removelist list of elements to remove,
outputlist which starts as list to subtract from, and ends as list with no instances of any
elements in removelist. 

some tests cases:

 (setdiff '(a b) '(a b c d)) => (c d)
 (setdiff '(a d) '(a b c d)) => (b c)

|#

(defun setdiff (removelist outputlist)
  (if (null removelist)
      outputlist
    (setdiff (cdr removelist) (myremove (car removelist) outputlist nil))))
	

#| 
My implemntation of max function. mymax compares the first element of a list with the
current maximum value. If it is heigher, this element becomes the maximum value. 
Else, we keep the current maximum value. We iterate completely through list L, until we
reach the base case of L being empty. After that, we return maxval, which should be equal
to the highest value in the list. Note: when first calling mymax, initialize maxval with a
lower number than you expect to see in the list.

test cases below. Second test shows pitfalls of improperly setting low maxval.

 (mymax '(1 4 3 4 5 2) '0) => 5
 (mymax '(2 3 4 1 3 3) '10) => 10

|#

(defun mymax (L maxval)
  (if (null L)
      maxval
    (if (> (car L) maxval)
	(mymax (cdr L) (car L))
      (mymax (cdr L) maxval))))

#| 
As name suggests, select best_takes as input list of lists S, a list N and a number maxnum
It iterates through S, calling common with each list element of S, and N, returns the first
element that has maxnum elements in common with N. Because maxnum will be given by mymax
when called by mymain later, select_best will have selected the first element of S that has
the highest amount of elements in common with N.

some test cases:

 (select_best '((1 2 3) (4 5) (1 3 2)) '(1 2 3 4 5)) => (1 2 3)
 (select_best '((1 2 3) (4 5) (1 3 2)) '(4 5 6)) => (4 5)

|#

(defun select_best (S N maxnum)
  (if (= maxnum (common (car S) N))
      (car S)
    (select_best (cdr S) N maxnum)))

#| 
The one that brings all the peices together. Outputlist starts at () and grows by one list
element from S with each recursion. In the base case, when we have no more elements in list N
to cover, we just print the outputlist and are done.

Allow me to describe in detail how S, N and outputlist are updated:

S: Using setdiff, we get rid of the list element of S that was found to be the greediest cover for N.
The greediest cover is found using select_best with S, N, and maxnum.
Maxnum, in turn is found using mymax, which is given an initial value of 0 for maxval, and a list
This list is provided by callcommon, given parameters S and N, and is a list of numbers of common 
elements that each list element of S has with N.

N: We get rid of the exact same elements from N as we do from S, using the same methods with the same 
parameters up until setdiff, which takes the same first parameter, but N instead of S for the second.

outputlist: We append outputlist into the list of the removed elements from S and N (doing it the opposite
way would end up with the first element placed in output list be the last one appearing in the final outputlist


|#

(defun mymain (S N outputlist)
  (if (null N)
      outputlist
    (mymain (setdiff (list(select_best S N (mymax (callcommon S N) 0))) S) (setdiff (select_best S N (mymax (callcommon S N) 0)) N) (append outputlist (list(select_best S N (mymax (callcommon S N) 0)))))))

#|
Calls mymain with a nil for outputlist, unchanged S, and a list made with function numbers from question 2 and 
given N parameter.

|#    

    
(defun setcover (N S)
  (mymain S (numbers N) nil))
