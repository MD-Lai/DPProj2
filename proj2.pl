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
:- ensure_loaded(library(clpfd)).

% Check and unify diagonal values, only really applies for 3x3 case.
% Other dimensions simply checks that the same rows and headers are correct
% 3x3 case
correctDiagonal([[0,A,B,C],[A,N,_,_],[B,_,N,_],[C,_,_,N]]).
% are there even diagonals on the 2x2 and 4x4 case?
% 2x2 case, just check headers are correct
correctDiagonal([[0,A,B], [A|_], [B|_]]).
% 4x4 case, again, check headers are correct
correctDiagonal([[0,A,B,C,D], [A|_], [B|_], [C|_], [D|_]]).

% Check that each row doesn't contain repeated numbers
% assume input is correct from this point
validRows([_|[]]).
validRows([_,[H|Rs]|TRs]) :- rowEqualSumOrProd(H, Rs), noRepeat(Rs), validRows([_|TRs]).

rowEqualSumOrProd(Val, R) :- listSum(R,Val); listProd(R,Val).

listSum([SumExpr], Sum) :- Sum is SumExpr. 
listSum([N1,N2|Tail], Sum) :- listSum([N1 + N2| Tail], Sum).

listProd([ProdExpr], Prod) :- Prod is ProdExpr.
listProd([N1,N2|Tail], Prod) :- listProd([N1 * N2|Tail], Prod).

% Checks for no repeats
noRepeat([]).
noRepeat([H|T]) :- \+(member(H,T)), noRepeat(T).

solve2x2([Headers, [HR1, R11, R12], [HR2, R21, R22]]).