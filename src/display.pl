% :- include('board.pl').
% :- include('board1.pl').
% :- include('board2.pl').
:- include('board3.pl').
:- include('logic.pl').
:- include('draw.pl').

% Display
opposite(black, white).
opposite(white, black).

display_board :-
    nl,
    draw_top_boundary,
    draw_column_coordinates,
    \+display_board([6, 6], 0),
    draw_bottom_boundary.

display_board([R, C], X) :-
    X < R, 
    draw_separator(C),
    draw_line_coordinate(X),
    \+display_row([X, C], 0), nl,
    N is X + 1, display_board([R, C], N).
   
display_row([R, C], X) :-
    X < C, display_piece(R, X),
    N is X + 1, display_row([R, C], N).

display_piece(R, C) :-
    cell(C, R, Color, Piece),
    piece(Piece, Color, Char),
    opposite(Color, Opposite),
    draw_piece(Char, Color, Opposite).

display_piece(R, C) :-
    \+cell(C, R, Color, Piece),
    draw_blank.
