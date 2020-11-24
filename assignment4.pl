%Question 1:


 
%we just try every single possibility. Between already does everything the suggested count function would do.

fourSquares(N, [S1, S2, S3, S4]):-
    between(0, N, S1), 
    between(S1, N, S2), 
    between(S2, N, S3), 
    between(S3, N, S4), 
    N is ((S1 * S1) + (S2 * S2) + (S3 * S3) + (S4 * S4)).  




%-----------------------------Question 2: ------------------------------







%add_to_list([],[],_).

%add_to_list([X | List], [X2 | List2], Value):-
    %add_to_list(List, List2, Value),
    %X2 is X + Value.
    

%helpersum([F|L1], L2):-
    %(add_to_list(L1, L2, F)).


%cartsum([_|[]], []).

%cartsum([F|L1], L2):-
    %append(C, C2, L2) ,  helpersum([F|L1], C)            ,cartsum(L1, C2)  ,! .




%calcpairs(L, Ans):-
     %findall(L2, (length(L, N), between(0, N, L2)), Ans).



%main helper function for appendsum, takes as input a list, the output list, and an element.
%Returns a list of 2-size lists with every possible pair with Element:
%add_to_append([1,2,3,4], X, 50) returns X= [[50,1],[50,2],[50,3],[50,4]].


add_to_append([],[],_).

add_to_append([X | List], [X2 | List2], Element):-
    add_to_append(List, List2, Element),
    X2 = [Element,X].



%calls add_to_append with first element separated from input list
helperappend([F|L1], L2):-
    (add_to_append(L1, L2, F)).



%The goal of this function is to take in a list, and return as output a list that has every possible pair:
%Ex: appendsum([1,2,3,4], X) returns X = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]

appendsum([_|[]], []).

appendsum([F|L1], L2):-
    L1 \= [], append(C, C2, L2) ,  helperappend([F|L1], C),appendsum(L1, C2)  ,! .





%firstmatch takes a list of single elements, a list of double elements and returns a match
%if there is one between first element of single elements list and any element of double elements list
%firstmatch([4,5,6],[[1,1],[1,2],[1,3],[0, 4]],X) returns X = [[4], [1,3]].

firstmatch(_,[],[]).


firstmatch(_,[_],[]).

firstmatch([F|_], [[F1,F2] |R], Answer):-
    Answer = [[F] ,[F1, F2]], F is F1 + F2, !;    
    firstmatch([F|_], R, Answer).
    
    
%removes first instance of an answer in format given by firstmatch from given 2 lists of single elements
%of which the second list is the list from which doubles list in firstmatch was made from
%remove_elements([[5],[2,3]], [2,4,5], [2,3,6], X, Y)
%returns X = [2,4], Y = [6].

remove_elements([[Singleton], [D1,D2]], Slist, DlistElements,  NewSlist, NewDlistElements ):-
    select(Singleton,Slist,NewSlist), 
    select(D1,DlistElements, DLE1),
    select(D2,DLE1, NewDlistElements), !.




%Combines all the functions above into one. Is the first step that is repeated over and over again:
%The first three lines are just calls to the above functions with appropriate paramters
%next 2 lines are for, if we have a match, we also want to remove it from the lists we build elements
%with.

%we use four lists: 2 for divisions A, (one to hold all elements still not disarmed, one to hold the
%"currently active elements", same for B. Four other lists to store the updated values of those lists
%and the last is one disarm combination answer


onestep1(Slist, Llist, NewSlist, NewLlist, Slist2, Llist2, NewSlist2, NewLlist2,  [[Singleton], [D1,D2]]):-
    appendsum(Llist, Dlist),
    firstmatch(Slist, Dlist, [[Singleton], [D1,D2]]),
    remove_elements([[Singleton], [D1,D2]], Slist, Llist, NewSlist, NewLlist),
    
    select(Singleton, Slist2, NewSlist2),
    NewLlist2 = Llist2.
    

  



%attempt to call previous step. If it return false, update params to reflect that.
%Added as a hotfix to a very confusing variables mess, was going to just update onestep1,
%but that proved to be a challenge, so I kept it as is.


onestep3([F|A], B, NewA, NewB, Abar, Bbar, NewAbar, NewBbar,  X):-
    onestep1([F|A], Bbar, NewA, NewBbar, Abar, B, NewAbar, NewB,   X)  ->
    %onestep1([F|A], Bbar, NewA, NewBbar, B, NewB,   X)  ->
    true;
    NewA = A,
    NewB = B,
    NewAbar = Abar,
    NewBbar = Bbar,
    X = [].





%decides from which list we will attempt to remove single element based on lowest "active" element.


choosestep1([F1|Llist1], [], NewList1, NewList2,  Abar, Bbar, NewAbar, NewBbar,    Ans):-
    onestep3([F1|Llist1], [], NewList1, NewList2,  Abar, Bbar, NewAbar, NewBbar,    Ans).


choosestep1([], [F2|List2], NewList1, NewList2,  Abar, Bbar, NewAbar, NewBbar,    Ans):-
    onestep3([F2|List2], [], NewList2, NewList1,  Bbar, Abar, NewBbar, NewAbar, Ans1), reverse(Ans1,Ans).



choosestep1([F1|List1], [F2|List2], NewList1, NewList2,  Abar, Bbar, NewAbar, NewBbar,    Ans ):-
    F1 < F2, onestep3([F1|List1], [F2|List2], NewList1, NewList2,  Abar, Bbar, NewAbar, NewBbar, Ans) .


choosestep1([F1|List1], [F2|List2], NewList1, NewList2,  Abar, Bbar, NewAbar, NewBbar,    Ans ):-
    F1 >= F2, onestep3([F2|List2], [F1|List1], NewList2, NewList1,  Bbar, Abar, NewBbar, NewAbar, Ans1), reverse(Ans1,Ans).


    


%recursively calls choostep1, until every step of the calculation is complete. Updates Ans as it goes.

topstep([],[],[],[],[]).

topstep(A, B, Abar, Bbar, Ans ):-
    
    choosestep1(A, B, NewA, NewB,  Abar, Bbar, NewAbar, NewBbar,    CurrAns),
    topstep(NewA, NewB, NewAbar,NewBbar, UpdatedAns),
    append(CurrAns,UpdatedAns,Ans) , !.


%a little formating function. Could not make top step return Ans in proper format, so have this inbetween function to take resulting list
%of elements and combine every two (because of append in topstep, instead of getting [  [[4], [1,3]], [[2,3],[5]]], we would be getting
% [[4],[1,3],[2,3],[5]], etc.

%So, natrually, combpairs([[4],[1,3],[2,3],[5]],X).
%returns X = [[[4], [1, 3]], [[2, 3], [5]]].


combpairs([],[]).

combpairs([F,S|R], OutL):-
    combpairs(R, CurList),
    append([[F,S]], CurList, OutL).



%first sorts Adivisions and Bdivisions so that we can disarm starting from the lowest values
%calls topstep, and formats the answer properly with combpairs

disarm(Adivisions, Bdivisions, Solution):-
    msort(Adivisions, A), msort(Bdivisions, B),
    topstep(A,B,A,B, S),
    combpairs(S, Solution).


