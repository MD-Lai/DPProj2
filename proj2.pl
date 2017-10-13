% Declarative Programming Project 2
%
% Der Hann Marvin Lai 754672
%
% A Prolog program to solve a maths puzzle
% The puzzle is to solve a maths square where every row and column is EITHER :
%     - the sum of the corresponding header
%     - the multiple to the corresponding header
% Under constraint that numbers within the same row or column are distinct
% and the entire diagonal has to be the same number
% and the domain of the values that aren't headers is in 1..9

% to make use of the constraint programming predicates
:- ensure_loaded(library(clpfd)).

/* Main Driver, Generates a solution for the Puzzle */
puzzle_solution(Puzzle) :-
    /***********************************
     * Checks that puzzle is a square  *
     ***********************************/
    % each row should have n elements = n rows
    maplist(same_length(Puzzle), Puzzle),

    /************************************
     * Extract Row values (not headers) *
     ************************************/
    % drop first row of Puzzle,
    tail(Puzzle, Rs_h),
    %drop first column of puzzle, leaving only the puzzle values
    drop_heads(Rs_h, Rs),

    /*******************************************
     * Check that values are within the domain *
     *******************************************/
    % check that the concatenation of Rows can be composed of 1..9
    append(Rs, Dom), Dom ins 1..9,

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
    tail(PuzzleT, Cs_h), drop_heads(Cs_h, Cs),

    /*************************************************************************
     * Ensure that Row and column values are all composed of distinct values *
     *************************************************************************/
    maplist(all_distinct, Rs),
    maplist(all_distinct, Cs),

    /*************************************************************************
     * Ensure that the Sum Or Product of each row and column equals the head *
     *************************************************************************/
    maplist(list_sum_or_prod, Rs_h),
    maplist(list_sum_or_prod, Cs_h),

    /*****************************************************************
     * Grounds variables according to the above constraints          *
     * IF there is only one solution,                                *
     * there will be enough constraints to produce only one solution *
     *****************************************************************/
    maplist(label, Puzzle)
.

/* Initialises check, passes first value and row through to diagonal */
diagonals(Rs) :-
    Rs = [[RDi | _ ] | _ ], diagonal(Rs, RDi)
.

/* Checks that diagonals have the same value as the value supplied (Rdi) */
diagonal([], _). % only reaches this case if everything before it is correct
diagonal([[Rdi|_] | Rt], Rdi) :-
    drop_heads(Rt, Rtd),
    diagonal(Rtd, Rdi)
.

/* Drops the first value of a list */
tail([_|T], T).

/* Drops the first value of every row in a list of lists */
drop_heads([],[]).
drop_heads(R, R_h) :-
    transpose(R, R_t), % transpose it so first column is now first row
    tail(R_t, R_tf), % drop the new first row (column)
    transpose(R_tf, R_h) % transpose back and now the first column is gone
.

/* Checks that the sum OR product of a row is equal to the Header value */
list_sum_or_prod(List) :-
    list_sum(List) ; list_prod(List)
.

/* Checks that the sum of the Tail of a row is equal to the Head value */
list_sum([Sum | List]) :-
    sum(List, #=, Sum)
.

/* Checks that the product of the Tail of a row is equal to the Head value*/
list_prod([Prod | List]) :-
    prod(List, #=, Prod)
.

/* Checks that the product of the list is equal to the Prod value supplied
 * Written to look like sum/3 from clpfd,
 * rel is the clpfd relation to use (i.e. #=, #<, etc) */
prod([], _, 1). % Base case, ASSUMING the previous lists were non-empty
prod([H | T], Rel, Prod) :-
    % Builds up an expression for the product
    prod(T, Rel, Rest), call(Rel, Prod, H*Rest)
    % equivalent to: Prod (Rel) H*Rest, e.g for #=: Prod #= H*Rest

% -fin-
