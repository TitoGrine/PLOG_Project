:- ensure_loaded('draw.pl').
:- ensure_loaded('logic.pl').

% Pieces internal representation.
% These facts associate color to name and finally character.
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

% ====================================================================================
% NOTE: All the display predicates make heavy use of the draw predicates in draw.pl.
% This file handles the displaying at an high level. Only the draw.pl predicates actually writes things on the screen.

% Predicate called every turn to display the current board state.
display_board :-
    nl, draw_space, draw_title, nl,
    draw_space, draw_top_boundary,
    draw_space, draw_column_coordinates,
    \+display_board([6, 6], 0),             % Actual board display with pieces is made here.
    draw_space, draw_bottom_boundary, nl.

% Displays the whole board (by rows).
% Calls itself recursively until it reaches the X row (usually 0).
display_board([R, C], X) :-
    X < R, 
    draw_space, draw_separator(C),
    draw_space, draw_line_coordinate(X), % Drawing the line number so the user can more easily play a piece.
    \+display_row([X, C], 0), nl,
    N is X + 1, !, display_board([R, C], N).

% Displays a row. Only displays the actual content (pieces) of a row.
% Calls itself recursively until it reaches the X column (usually 0).
display_row([R, C], X) :-
    X < C, display_piece(R, X),
    N is X + 1, !, display_row([R, C], N).

% Displays a piece. The piece's character is drawn if the owner's color over a background of the opposite color.
display_piece(R, C) :-
    cell(C, R, Color, Piece),
    piece(Piece, Color, Char),
    opposite(Color, Opposite),
    draw_piece(Char, Color, Opposite).

% If there is no piece on a given possition, then this predicate draws a blank space.
display_piece(R, C) :-
    \+cell(C, R, _, _),
    draw_blank.
