:- use_module(library(clpfd)).

sudoku(Rows):-
    length(Rows, 9), maplist(same_length(Rows), Rows), % checks for 9 rows, same_length/2 true if both lists are lists with same number of elems. In short, checks that each Row in Rows is same length as Rows (9)
    append(Rows, Vs), Vs ins 1..9, % append/2 true if Vs is a concatenation of all arrays in Rows. Checks that the domain of Rows is strictly 1..9. In short, checks that all of Rows is in domain 1..9.
    maplist(all_distinct, Rows), % checks that each row in Rows is composed of distinct items
    transpose(Rows, Columns), % changes rows to colums
    maplist(all_distinct, Columns), % checks each column in Columns is composed of distinct items
    Rows = [A,B,C,D,E,F,G,H,I], % Specifies format for Rows, i.e. Row A, Row B, Row C etc..
    blocks(A,B,C), blocks(D,E,F), blocks(G,H,I). % checks the blocks in each triplet of rows

blocks([],[],[]).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :- % break it up into the first 3 items of each row
    all_distinct([A,B,C,D,E,F,G,H,I]), % checks that the "block" composed of the first 3 items of each row is composed of distinct items
    blocks(Bs1, Bs2, Bs3). % checks the next blocks with the remainder of the lists. 