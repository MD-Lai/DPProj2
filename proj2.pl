/*****************************************************************************
 * Declarative Programming Project 2                                         *
 *                                                                           *
 * Der Hann Marvin Lai                                                       *
 *                                                                           *
 * A set of Prolog predicates to solve a maths puzzle                        *
 * The puzzle is to solve a maths square where each row and column is EITHER:*
 *    - the sum of the corresponding header                                  *
 *    - the multiple to the corresponding header                             *
 * Under constraint that numbers within the same row or column are distinct  *
 * and the entire diagonal has to be the same number                         *
 * and the domain of the values that aren't headers is in 1..9               *
 *****************************************************************************/

% to make use of the constraint programming predicates
:- ensure_loaded(library(clpfd)).

/*****************************************************************************
 * Main Driver, Generates a solution for the Puzzle.                         *
 * Works by specifying the constraints and domain,                           *
 * essentially creating a system of linear equations.                        *
 * Once all constraints are set,                                             *
 * binds variables that satisfy those constraints                            *
 *****************************************************************************/
puzzle_solution(Puzzle) :-

    % Checks that puzzle is a square
    % each row should have n elements = n rows
    maplist(same_length(Puzzle), Puzzle),

    % Extract Row values (not headers)
    % drop first row of Puzzle,
    tail(Puzzle, Rs_h),
    % drop first column of puzzle, leaving only the puzzle values
    drop_heads(Rs_h, Rs),

    % Check that values are within the required domain of 1..9
    % true if concatenation of Rows (no headers) can be composed of 1..9
    append(Rs, Dom), Dom ins 1..9,

    % Unifies diagonals
    diagonals(Rs),

    % Transpose Puzzle so Columns are now rows
    transpose(Puzzle, PuzzleT),

    % Extract Column values
    % Same Procedure as Puzzle and Rs above
    tail(PuzzleT, Cs_h), drop_heads(Cs_h, Cs),

    % Ensure that Row and column values are all composed of distinct values
    maplist(all_distinct, Rs),
    maplist(all_distinct, Cs),

    % Ensure that the Sum Or Product of each row and column equals the head
    maplist(list_sum_or_prod, Rs_h),
    maplist(list_sum_or_prod, Cs_h),

    % Grounds variables according to the above constraints and domain
    % IF there is only one solution,
    % there will be enough constraints to produce only one solution
    maplist(label, Puzzle)
.

/* Initialiser for diagonal, passes first value and row through to diagonal */
diagonals(Rs) :-
    Rs = [[R_d | _ ] | _ ], diagonal(Rs, R_d)
.

/* Checks that diagonals have the same value as the value supplied (R_d) */
diagonal([], _). % only reaches this case if everything before it is correct
diagonal([[R_d | _ ] | Rt], R_d) :- % True if Head of current row is same as R_d
    drop_heads(Rt, Rtd), % Remove first Col of the remaining lists
    diagonal(Rtd, R_d) % Check that sub square also has correct diagonals
.

/* Drops the first value of a list */
tail([_ | T], T). % no case for empty list because empty list has no "tail"

/* Drops the first value of every list in a list of lists */
drop_heads([], []).
drop_heads(R, R_nh) :-
    transpose(R, RT), % transpose it so first column is now first row
    tail(RT, RT_t), % drop the new first row
    transpose(RT_t, R_nh) % transpose back and now the first column is gone
.

/* Checks that the sum OR product of a row is equal to the Header value */
list_sum_or_prod(List) :-
    list_sum(List) ; list_prod(List)
.

/* Checks that the sum of the Tail of a row is equal to the Head value */
list_sum([Sum | List]) :-
    sum(List, #=, Sum) % sum is defined in clpfd
.

/* Checks that the product of the Tail of a row is equal to the Head value*/
list_prod([Prod | List]) :-
    prod(List, #=, Prod) % prod is not defined in clpfd
.

/* Checks that the product of the list is equal to the Prod value supplied
 * Written to look like sum/3 from clpfd,
 * rel is the clpfd relation to use (i.e. #=, #<, etc) */
prod([], _, 1). % Base case, ASSUMING the previous lists were non-empty
prod([H | T], Rel, Prod) :-
    % Builds up an expression for the product
    prod(T, Rel, Rest), call(Rel, Prod, H*Rest)
    % equivalent to: Prod (Rel) H*Rest, e.g Rel = #=: Prod #= H * Rest
.

% -fin-
