% Declarative Programming Project 2
%
% Der Hann Marvin Lai 754672
%
% A Prolog program to solve a maths puzzle
% The puzzle is to solve a maths square where every row and column is EITHER :
%     - the sum of the corresponding header
%     - the multiple to the corresponding header
% Under the constraint that numbers cannot be repeated within the same row or column
% and the entire diagonal has to be the same number

% to use transpose/2 predicate
:- use_module(library(clpfd)).

% % Check and unify diagonal values, only really applies for 3x3 case.
% % Other dimensions simply checks that the same rows and headers are correct
% % 3x3 case
% correctDiagonal([[0,A,B,C],[A,N,_,_],[B,_,N,_],[C,_,_,N]]).
% % are there even diagonals on the 2x2 and 4x4 case?
% % 2x2 case, just check headers are correct
% correctDiagonal([[0,A,B], [A|_], [B|_]]).
% % 4x4 case, again, check headers are correct
% correctDiagonal([[0,A,B,C,D], [A|_], [B|_], [C|_], [D|_]]).

% % Check that each row doesn't contain repeated numbers
% % assume input is correct from this point
% validRows([_|[]]).
% validRows([_,[H|Rs]|TRs]) :- rowEqualSumOrProd(H, Rs), noRepeat(Rs), validRows([_|TRs]).

% rowEqualSumOrProd(Val, R) :- listSum(R,Val); listProd(R,Val).

% listSum([SumExpr], Sum) :- Sum is SumExpr.
% listSum([N1,N2|Tail], Sum) :- listSum([N1 + N2| Tail], Sum).

% listProd([ProdExpr], Prod) :- Prod is ProdExpr.
% listProd([N1,N2|Tail], Prod) :- listProd([N1 * N2|Tail], Prod).

% % Checks for no repeats
% noRepeat([]).
% noRepeat([H|T]) :- \+(member(H,T)), noRepeat(T).

% solve2x2([Headers, [HR1, R11, R12], [HR2, R21, R22]]).
%
% correct_puzzle(Puzzle) :-
%     format2x2(Puzzle);
%     format3x3(Puzzle);
%     format4x4(Puzzle)
% .
%
% format2x2(Puzzle) :-
%     RH =     [RH1, RH2],
%     R1 =     [RDi, R12],
%     R2 =     [R21, RDi],
%     Rs =      [R1,  R2],
%
%
%     Puzzle = [[0  | RH],
%               [CH1| R1],
%               [CH2| R2]],
%
%
%     transpose(Rs, Cs),
%
%     CH =     [CH1, CH2],
%     Cs =      [C1,  C2],
%
%     % Cs is the transpose of all just the non-header grid,
%     % can be treated as being the columns of the non-header grid
%     maplist(no_repeat, Rs),
%     maplist(no_repeat, Cs),
%
% /*
% */
%     list_sum(R1, CH1) ; list_prod(R1, CH1),
%     list_sum(R2, CH2) ; list_prod(R2, CH2),
%     list_sum(C1, RH1) ; list_prod(C1, RH1),
%     list_sum(C2, RH2) ; list_prod(C2, RH2),
%     all_ints_2d(Puzzle),
%     print(Puzzle)
% .
%
% format3x3(Puzzle) :-
%     RH = [RH1, RH2, RH3],
%     R1 = [RDi, R12, R13],
%     R2 = [R21, RDi, R23],
%     R3 = [R31, R32, RDi],
%
%     Puzzle = [[0  |   RH],
%               [CH1|   R1],
%               [CH2|   R2],
%               [CH3|   R3]],
%
%     all_ints_2d(Puzzle),
%
%     CH = [CH1, CH2, CH3],
%
%     transpose(Puzzle, PuzzleT)
% .
%
% format4x4(Puzzle) :-
%     RH = [RH1, RH2, RH3, RH4],
%     R1 = [RDi, R12, R13, R14],
%     R2 = [R21, RDi, R23, R24],
%     R3 = [R31, R32, RDi, R34],
%     R4 = [R41, R42, R43, RDi],
%
%     Puzzle = [[0  |        RH],
%               [CH1|        R1],
%               [CH2|        R2],
%               [CH3|        R3],
%               [CH4|        R4]],
%
%     all_ints_2d(Puzzle),
%
%     CH = [CH1, CH2, CH3, CH4],
%
%     transpose(Puzzle, PuzzleT)
% .
%
% no_repeat([]).
% no_repeat([H|T]) :- \+(member(H,T)), no_repeat(T).
%
% repeat([_|[]]).
% repeat([H|T]) :- member(H,T), repeat(T).
%
% valid_row_col(Rs,Cs) :- maplist(no_repeat, Rs), maplist(no_repeat, Cs).
%
% % list_sum(List, Total)
% list_sum([Total], Total).
% list_sum([Num1, Num2 | Tail], Total) :- integer(Num1), integer(Num2), integer(Sum), Sum #= Num1 + Num2, list_sum([Sum | Tail], Total).
%
% list_prod([Total], Total).
% list_prod([Num1, Num2 | Tail], Total) :- integer(Num1), integer(Num2), integer(Prod), Prod #= Num1 * Num2, list_prod([Prod | Tail], Total).
%
% all_ints_2d([]).
% all_ints_2d([Arr2dH | Arr2dT]) :- maplist(integer, Arr2dH), all_ints_2d(Arr2dT).
% -fin-
