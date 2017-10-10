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

/*
Puzzle_2x2 = [[0,8,6],[5,_,3],[6,_,2]], Soln = [[0,8,6],[5,2,3],[6,4,2]] 
Puzzle_3x3 = [[0,10,24,168],[96,3,_,8],[12,_,3,_],[10,5,2,_]], Soln = [[0,10,24,168],[96,3,4,8],[12,2,3,7],[10,5,2,3]]
Puzzle_4x4 = [[0,224,21,168,20],[14,4,2,_,_],[24,1,4,_,3],[2016,7,_,_,8],[25,_,6,7,_]], Soln = [[0,224,21,168,20],[14,4,2,3,5],[24,1,4,2,3],[2016,7,9,4,8],[25,8,6,7,4]]
*/

% third time lucky
% attempt 3
solve(Puzzle) :-
    maplist(same_length(Puzzle), Puzzle), % each row should have n-elements = n-rows. 

    % extract the puzzle values (not headers)
    % drop first row of Puzzle, drop first column of puzzle
    drop_first(Puzzle, Rs_h), drop_heads(Rs_h, Rs),

    diagonals(Rs), 

    append(Rs, Dom), Dom ins 1..9, % check that it's possible to create a list composed of 1..9 which is the same as the concatenation of the puzzle elements (no headers)


    transpose(Puzzle, PuzzleT),

    drop_first(PuzzleT, Cs_h), drop_heads(Cs_h, Cs),

    maplist(all_distinct, Rs),
    maplist(all_distinct, Cs),

    maplist(list_sum_or_prod, Rs_h),
    maplist(list_sum_or_prod, Cs_h)

.

diagonals(Rs) :-
    Rs = [[RDi | _] | _ ], diagonal(Rs, RDi) 
.

diagonal([[Rdi]], Rdi).
diagonal(Rs, Rdi) :-
    Rs = [[Rdi|_] | Rt],
    drop_heads(Rt, Rtd),
    diagonal(Rtd, Rdi)
.

drop_heads([], []).
drop_heads([[_|T1] | Tr], [T1|Td]) :- 
    drop_heads(Tr, Td)
.

drop_first([_|T], T).

list_sum_or_prod(List) :-
    list_sum(List) ; list_prod(List)
.

list_sum([Sum | List]) :-
    sum(List, #=, Sum)
.

list_prod([Prod | List]) :-
    prod(List, Prod)
.

prod([Prod_expr], Prod) :- Prod #= Prod_expr.
prod([H1, H2 | T], Prod) :-
    HProd = H1 * H2, prod([HProd|T], Prod)
.

% % let's start again
% % attempt 2
% format_2x2(Puzzle) :-
%     Puzzle = [[0   | RH],
%               [CH1 | R1],
%               [CH2 | R2]],

%     length(Puzzle, 3), maplist(same_length(Puzzle), Puzzle), % ensure each row in the puzzle has the same number of elements as there are number of rows

%     R1 = [RDi,   _], % extract just the values that require solving
%     R2 = [  _, RDi],
%     RH = [RH1, RH2],
%     Rv =  [R1, R2],  % Rv for Row values i.e. values not in headers

    
%     in_domain_1to9(Rv),

%     maplist(all_ints_row, Puzzle),

%     maplist(all_distinct, Rv),

%     % now we do the checking

%     row_sum(R1, CH1) ; row_prod(R1, CH1),
%     row_sum(R2, CH2) ; row_prod(R2, CH2),

%     transpose(Rv, Cv), 

%     Cv = [C1, C2],

%     maplist(all_distinct, Cv),

%     row_sum(C1, RH1) ; row_prod(C1, RH1),
%     row_sum(C2, RH2) ; row_prod(C2, RH2),

%     % PuzzleT = [[0   | CH],
%     %            [RH1 | C1],
%     %            [RH2 | C2]],



%     fin(END)
% .

% fin(_) :- true.

% in_domain_1to9(Pv) :-
%     append(Pv, Vs), Vs ins 1..9
% .

% all_ints_row(Row) :-
%     maplist(integer, Row)
% .

% row_sum(Row, Sum) :- 
%     Row ins 1..9, sum(Row, #=, Sum)
% .

% row_prod(Row, Prod) :-
%     Dom ins 1..9, scalar_product(Dom, Row, #=, Prod)
% .

% row_sum([SumExpr], Sum) :- Sum #= SumExpr.
% row_sum([H1, H2 | T], Sum) :-
%     HeadTotal = H1 + H2, row_sum([HeadTotal | T], Sum)
% .

% row_prod([ProdExpr], Prod) :- Prod #= ProdExpr.
% row_prod([H1, H2 | T], Prod) :-
%     HeadProd = H1 * H2, row_prod([HeadProd | T], Prod)
% .

% attempt 1
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

% puzzle2x2(Puzzle) :- Puzzle = [[0,5,6], [8,4,2], [4,1,4]].

% correct_puzzle(Puzzle) :-
%     format2x2(Puzzle);
%     format3x3(Puzzle);
%     format4x4(Puzzle)
% .

% format2x2(Puzzle) :-
%     RH =     [RH1, RH2],
%     R1 =     [RDi, R12],
%     R2 =     [R21, RDi],
%     Rs =      [R1,  R2],


%     Puzzle = [[0  | RH],
%               [CH1| R1],
%               [CH2| R2]],


%     transpose(Rs, Cs),

%     CH =     [CH1, CH2],
%     Cs =      [C1,  C2],

%     % Cs is the transpose of all just the non-header grid,
%     % can be treated as being the columns of the non-header grid
%     maplist(all_distinct, Rs),
%     maplist(all_distinct, Cs),

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
% list_sum(List, Total)
% list_sum([Total], Total).
% list_sum([Num1, Num2 | Tail], Total) :- integer(Num1), integer(Num2), integer(Sum), Sum #= Num1 + Num2, list_sum([Sum | Tail], Total).

% list_prod([Total], Total).
% list_prod([Num1, Num2 | Tail], Total) :- integer(Num1), integer(Num2), integer(Prod), Prod #= Num1 * Num2, list_prod([Prod | Tail], Total).

% all_ints_2d([]).
% all_ints_2d([Arr2dH | Arr2dT]) :- maplist(integer, Arr2dH), all_ints_2d(Arr2dT).
% -fin-
