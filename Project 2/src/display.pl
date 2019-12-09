:- use_module(library(lists)).

draw_header(0).
draw_header(N) :-
    write(' ---'),
    NN is N - 1,
    draw_header(NN).

draw_board_row([]) :- 
    write(' | '), nl.
draw_board_row([Element | RestRow]) :-
    write(' | '), write(Element),
    draw_board_row(RestRow).

draw_board(L, Row) :-
    write(' '), draw_header(L), nl,
    draw_board_row(Row).

display_solution(Board, L) :-
    nl, write('Solution:'), nl, nl,
    maplist(draw_board(L), Board),
    write(' '), draw_header(L), nl, nl, !.
    