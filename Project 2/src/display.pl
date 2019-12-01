:- use_module(library(lists)).

draw_header(0).
draw_header(N) :-
    write(' ---'),
    NN is N - 1,
    draw_header(NN).

draw_board_row(_, _, L, L) :- write(' | '), nl.
draw_board_row(Board, Row, Delta, L) :-
    Pos is Row * L + Delta,
    nth0(Pos, Board, Number),
    write(' | '), write(Number),
    NewDelta is Delta + 1,
    draw_board_row(Board, Row, NewDelta, L).

draw_board(_, L, L) :-
    write(' '), draw_header(L).

draw_board(Board, Row, L) :-
    write(' '), draw_header(L), nl,
    draw_board_row(Board, Row, 0, L),
    NextRow is Row + 1,
    draw_board(Board, NextRow, L).

display_solution(Board, L) :-
    nl, write('Solution:'), nl, nl,
    draw_board(Board, 0, L), nl, nl.
    