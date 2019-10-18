% This module holds everything related to game logic

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

% Set the board back to the starting board
reset_board :-
    retractall(cell(_,_,_,_)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).

% Shifts all pieces to the right by 1 column
shift_right(0).
shift_right(C) :-
    C > 0, \+ cell(C, R, Color, Piece),
    Cleft is C-1, shift_right(Cleft).
shift_right(C) :-
    C > 0, Cleft is C-1, Cright is C+1,
    retract(cell(C, R, Color, Piece)),
    assertz(cell(Cright, R, Color, Piece)),
    shift_right(Cleft).

play_piece(R, C, Color,Piece) :-
    assertz(cell(R, C, Color, Piece)).