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
% and the domain of the values that aren't headers is in 1..9 

% to make use of the constraint programming predicates
:- ensure_loaded(library(clpfd)).

/*
Puzzle_2x2 = [[0,8,6],[5,_,3],[6,_,2]], 
      Soln = [[0,8,6],[5,2,3],[6,4,2]] 
Puzzle_3x3 = [[0,10,24,168],[96,_,_,8],[12,_,3,_],[10,5,2,_]], 
      Soln = [[0,10,24,168],[96,3,4,8],[12,2,3,7],[10,5,2,3]]
Puzzle_4x4 = [[0,224,21,168,20],[14,4,2,_,_],[24,1,4,_,3],[2016,7,_,_,8],[25,_,6,7,_]], 
      Soln = [[0,224,21,168,20],[14,4,2,3,5],[24,1,4,2,3],[2016,7,9,4,8],[25,8,6,7,4]]
*/

/*
bagof(X, goal(Using, X), X_values_that_make_predicate_true).
*/
% third time lucky
% attempt 3
puzzle_solution(Puzzle) :-
    /***********************************
     * Checks that puzzle is a square  *
     ***********************************/
    maplist(same_length(Puzzle), Puzzle), % each row should have n elements = n rows

    /************************************
     * Extract Row values (not headers) *
     ************************************/
    drop_first(Puzzle, Rs_h), % drop first row of Puzzle, 
    drop_heads(Rs_h, Rs),     %drop first column of puzzle, leaving only the puzzle values

    /*******************************************
     * Check that values are within the domain *
     *******************************************/
    append(Rs, Dom), Dom ins 1..9, % check that the concatenation of Rows can be composed of 1..9

    /*********************
     * Unifies diagonals *
     *********************/
    diagonals(Rs), 

    /********************************************
     * Transpose Puzzle so Columns are now rows *
     ********************************************/
    transpose(Puzzle, PuzzleT),

    /***************************
     * Extract Column values   *
     * Same Procedure as above *
     ***************************/
    drop_first(PuzzleT, Cs_h), drop_heads(Cs_h, Cs),

    /*************************************************************************
     * Ensure that Row and column values are all composed of distinct values *
     *************************************************************************/
    maplist(all_distinct, Rs),
    maplist(all_distinct, Cs),
    
    /***************************************************************************
     * Ensure that the Sum Or Product of each row and column equals the header *
     ***************************************************************************/
    maplist(list_sum_or_prod, Rs_h),
    maplist(list_sum_or_prod, Cs_h)

    /*
    if unbound, bag, apply a row, repeat check until all rows are bound and valid
    All it needs is !1! correctly ground row and it's entirely set to solve it
    */
    % (ground(Puzzle) ->
    %     % do nothing, puzzle is solved
    %     true
    % ;
    %     % not done yet, bind some variables
    %     false
    % )
.

apply_bag(_, []).
apply_bag(Row, [Row | Bag_t]) :-
    apply_bag(_, Bag_t)
.

in_domain_1to9(Row) :-
    maplist(between(1,9), Row)
.
valid_row(Row) :-
    drop_first(Row, Row_nh), in_domain_1to9(Row_nh), all_distinct(Row_nh), list_sum_or_prod(Row)
.

/* Primer function for diagonal, passes first value and row through to diagonal */
diagonals(Rs) :-
    Rs = [[RDi | _ ] | _ ], diagonal(Rs, RDi) 
.

/* Checks that diagonals have the same value as the value supplied (Rdi) */
diagonal([], _). % Base case, bottom right value is same as value supplied
diagonal([[Rdi|_] | Rt], Rdi) :- % 
    drop_heads(Rt, Rtd),
    diagonal(Rtd, Rdi)
.

/* Drops the first value of a list */
drop_first([_|T], T).

% /* Drops the first value of every row in a list of lists */
% drop_heads([], []). % Base Case, second argument "returned" as empty list
% drop_heads([[_|T1] | Tr], [T1|Td]) :- % Gets tail of first row, uses tail as new head
%     drop_heads(Tr, Td) % Td is the List of the tails of Lists
% .

/* Drops the first value of every row in a list of lists */
drop_heads([],[]).
drop_heads(R, R_h) :-
    transpose(R, R_t), % transpose it so first column is now first row
    drop_first(R_t, R_tf), % drop the new first row (column)
    transpose(R_tf, R_h) % transpose back and now the first column is gone
.

/* Checks that the sum OR product of a row is equal to the Header value */
list_sum_or_prod(List) :-
    list_sum(List) ; list_prod(List)
.

/* Checks that the sum of the Values of a row is equal to the Header value */
list_sum([Sum | List]) :-
    sum(List, #=, Sum)
.

/* Checks that the product of the Values of a row is equal to the Header value*/
list_prod([Prod | List]) :-
    prod(List, #=, Prod)
.

/* Checks that the product of the list is equal to the Prod value supplied
 * Written to look like sum/3 from clpfd, rel is the clpfd relation to use (i.e. #=, #<, etc) */
prod([], _, 1).
% prod([Prod_expr], Rel, Prod) :- call(Rel, Prod_expr, Prod). % executes the accumulated expression
prod([H | T], Rel, Prod) :-
    prod(T, Rel, Rest), call(Rel, Prod, H*Rest) % Builds up an expression for the product
.

% -fin-
