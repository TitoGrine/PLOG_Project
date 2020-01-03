:- use_module(library(lists)).

% Writes in the terminal the number in Element, or an 'X' if Element is a variable or a 0.
write_element(Element) :-
    (((var(Element) ; Element is 0), write(' ')) ; write(Element)).

% Displays a line used to separate rows.
draw_line_across(0).
draw_line_across(N) :-
    write(' ---'),
    NN is N - 1,
    draw_line_across(NN).

% Writes all elements in a row properly separating them.
draw_board_row([]) :- 
    write(' | '), nl.
draw_board_row([Element | RestRow]) :-
    write(' | '), write_element(Element),
    draw_board_row(RestRow).

% Displays the given row with a line to separate it.
draw_board(L, Row) :-
    write(' '), draw_line_across(L), nl,
    draw_board_row(Row).

% Displays the given board in a friendly format.
display_board(Board, Message) :-
    length(Board, L),
    nl, write(Message), write(':'), nl, nl,
    maplist(draw_board(L), Board),
    write(' '), draw_line_across(L), nl, nl, !.
    