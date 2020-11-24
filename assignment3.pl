
%base case 1, L1 <= L2
alternate([], L2, L2).

%base case 2, L1 > L2
alternate([F|R], [], [F|R]).


%extend L by first element of each list, call against with both lists shorter by 1.
alternate([First1 | Rest1], [First2|Rest2], L) :- 
    alternate(Rest1, Rest2, NewL),
    L = [First1|[First2|NewL]].
 




%base case, empty list has N = 0
counti([], 0).

%if integer, add 1, go to next element
counti([First|Rest], N):- 
    counti(Rest, N1), N is N1 + 1, integer(First).

%Next two are cases for if not integer, would be easier to use negation, but assign specs....
counti([First|Rest], N):- 
    counti(Rest, N1), N is N1, atom(First).


counti([First|Rest], N):- 
    counti(Rest, N1), N is N1, float(First).


%deals with lists, even nested lists. Adds it all up.
counti([First|Rest], N):- 
    counti(Rest, N1), counti(First, N2), N is N1 + N2, is_list(First). 
    




  
/**
---------------------------------------------Question 3------------------------------------------------

Find all members, discard duplicates. Feels like it's shorter than intended, but it seems to work and does
not break any rules regading cuts, negation, if/then/else, so....
*/
umem(X, L):-
    distinct(member(X, L)).



    
    

/**
----------------------------------------------Question 4.1-----------------------------------------------
*/

%finds prerequsites, as well as their prerequisites, until get pairs of element and list of their prerequisites
required_helper(Z,Y):-
    prerequisite(X,Z), required_helper(X,Y).

%base case
required_helper(X, Y):-
   prerequisite(Y, X).


%1st and 3rd lines needed for sorting as per requirement. 2nd line finds the answers
required(C, L):-
    findall(Z, course(Z), L2),
    findall(X, required_helper(C, X), L1),
    intersection(L2, L1, L).

    

/**

---------------------------------------------Question 4.2
*/

%finds all the prerequisites of all courses that exist
preq_list(L, C):-
    course(Z), required(Z, C), L = Z.



%not_member copied from class notes.
not_member(_, []).

not_member(X, [Y|L]):-
    X \== Y, not_member(X, L).


%not_member gets rid of courses already taken from the list. Using preqlist and subset finds all possible courses to take
can_take(L,C):-
    preq_list(X, Y), C = X, subset(Y,L), not_member(C, L).





/**
---------------------Question 4.3


I tried my best, but I have no idea how to do this. Infinite loops everywhere, no matter what I try to do. Without
using cut, I can't find a way to stop them.

I tried solving it by finding preq-list of size > number of courses, then I'd add element to the list, and cut off
anything after second instance of element from the list, but I ran out of time before I figured it out.

Here are a few functions that might have been useful, plus numerous other failed attempts that I left commented out below, 
hopefully that's worth at least some marks, I spent more time on this question than all the rest put together....
*/



firstelement(F, [F|_]).



lastelement(E, [E]).

lastelement(E, [_|R]):-
    lastelement(E1, R), E = E1.



firstequallast(L):-
    firstelement(E, L), lastelement(E, L).






/**

in_cycle(Course, Prereq, L, D):-
     prerequisite(Prereq, Course),  L =  [Prereq|D], 
    findall(Z, course(Z), L2), length(L2, LenT), length(L,LenC), LenC =< LenT.




*/





%in_cycle(Course, Cycle):-
 %   t(Course, Cycle), firsteqlast(Cycle).




%in_cycle(L1, Cycle):-
%    prerequisite(X, Cycle),     L1 = [L1|X].


%in_cycle(Course, Cycle):-
%    required(Cycle, Cycle).





%requiredTWO(Z,Y):-
%     prerequisite(X,Z), prerequisite(Y,X).


%requiredTWO(X, Y):-
%   prerequisite(Y, X).






%req2(Z, Y, Number):-
%    Number>0, prerequisite(X, Z), req2(X, Y, N), N is Number-1.
    

%req2(X, Y, _):-
%    prerequisite(Y, X).



%n(N, Total):-
%    N > 0, n(NewN, NewTotal), Total is NewTotal - 1, N is NewN + 1.

%n(0,0).


%newT(C, L):-
%    findall(X, prerequisite(X, C), L1), subtract(L1, [C], L) .




%in_cycle(Course, Cycle):-
%    not_member(Course, Cycle), preq_list(Course, Cycle). 





%p([F|R], L2):-
%    newT(F, L1), p([R|L1], [L1|L2]).
    


