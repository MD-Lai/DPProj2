:- ensure_loaded(library(clpfd)).


/*
P = [[0,8,6],[5,_,3],[6,_,2]], puzzle_solution(P).
Puzzle_2x2 = [[0,8,6],[5,_,3],[6,_,2]], Soln = [[0,8,6],[5,2,3],[6,4,2]] 
Puzzle_3x3 = [[0,10,24,168],[96,_,_,8],[12,_,3,_],[10,5,2,_]], Soln = [[0,10,24,168],[96,3,4,8],[12,2,3,7],[10,5,2,3]]
P = [[0,224,21,168,20],[14,4,2,_,_],[24,1,4,_,3],[2016,7,_,_,8],[25,_,6,7,_]],puzzle_solution(P). 
Puzzle_4x4 = [[0,224,21,168,20],[14,4,2,_,_],[24,1,4,_,3],[2016,7,_,_,8],[25,_,6,7,_]], Soln = [[0,224,21,168,20],[14,4,2,3,5],[24,1,4,2,3],[2016,7,9,4,8],[25,8,6,7,4]]
*/

puzzle_solution(Puzzle) :-
    drop_headers(Puzzle, Soln),
    domain_check(Soln),
    diagonal_check(Soln),
    sumProduct_check(Puzzle),
    uniqueDigits_check(Soln),
    (not(ground_check(Soln)) ->
        true;
        true
    ).
    %if unbound then we need to loop through every bag entry with puzzle solution

selectLeastPossibleMoves([], _).
selectLeastPossibleMoves(Puzzle, LowestMove) :-
    Puzzle = [R | Rs],
    bagof(R, isRowCorrect(R), bag),
    length(LowestMove, A),
    length(bag, B),
    (A #> B ->
        selectLeastPossibleMoves(Rs, B)
        ;
        selectLeastPossibleMoves(Rs, A)
    ).

sumProduct_check(Puzzle):-
    drop_head_row(Puzzle, Rows1),
    areRowsSumOrMul(Rows1),
    transpose(Puzzle, Puzzle2),
    drop_head_row(Puzzle2, TRows),
    areRowsSumOrMul(TRows).

domain_check(Rows) :-
    append(Rows, Es), Es ins 1..9.

ground_check(Rows):-
    ground(Rows).

diagonal_check(Rows) :-
    Rows = [[A|_] | _],
    diagonals(Rows, 0, A).

diagonals([],_,_).
diagonals(Rows, Index, A):-
    Rows = [R | RS],
    nth0(Index, R, A),
    Index2 is Index + 1,
    diagonals(RS, Index2, A).

uniqueDigits_check(Rows):-
    isRowUnique(Rows),
    transpose(Rows, TRows),
    isRowUnique(TRows).

areRowsUnique(Rows):-
    maplist(isRowUnique, Rows).

isRowUnique([]).
isRowUnique(Row) :-
    is_set(Row).

% This grabs the tail of the list and says it's T
drop_head_row(Rows, Res) :-
    Rows = [_| Res].

% This gets rid of the left and the right headers of the puzzle
drop_headers(Rows, Res) :-
    drop_head_row(Rows, T1),
    transpose(T1, T2),
    drop_head_row(T2, T3),
    transpose(T3, Res).

areRowsSumOrMul([]).
areRowsSumOrMul(Rows):-
    maplist(isRowSumOrMul, Rows).
    % isRowSumOrMul(R),
    % areRowsSumOrMul(RS).

isRowSumOrMul([]).
isRowSumOrMul(Row):-
    isRowSum(Row);
    isRowMul(Row).

% first value in the list is what it should equal to
% this determines if the elements add up
isRowSum([E|Es]) :-
   sum(Es, #=, E). 

isRowMul([Prod|Es]):-
    getMultiple(Es, Prod).

getMultiple([Banana], Prod) :- Prod #= Banana.
getMultiple([E1, E2 | Es], Prod) :-
    getMultiple([E1*E2|Es], Prod).

isRowCorrect([R | Rs]) :-
    maplist(between(1,9),Rs),
    isRowSumOrMul([R | Rs]),
    isRowUnique(Rs).