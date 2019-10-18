% :- include('board.pl').
% :- include('board1.pl').
% :- include('board2.pl').
 :- include('board3.pl').
:- include('logic.pl').
:- include('draw.pl').

opposite(black, white).
opposite(white, black).

% Pieces
piece(king, black, '♚').
piece(king, white, '♔').
piece(queen, black, '♛').
piece(queen, white, '♕').
piece(bishop, black, '♝').
piece(bishop, white, '♗').
piece(rook, black, '♜').
piece(rook, white, '♖').
piece(knight, black, '♞').
piece(knight, white, '♘').
piece(pawn, black, '♟').
piece(pawn, white, '♙').

display_board :-
    nl, draw_space, draw_title, nl,
    draw_space, draw_top_boundary,
    draw_space, draw_column_coordinates,
    \+display_board([6, 6], 0),
    draw_space, draw_bottom_boundary, nl.

display_board([R, C], X) :-
    X < R, 
    draw_space, draw_separator(C),
    draw_space, draw_line_coordinate(X),
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
