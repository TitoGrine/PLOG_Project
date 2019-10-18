:- dynamic(cell/4).
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

% Starting board

% cell(3, 2, white, king).
% cell(2, 2, black, king).

% Example board 1
cell(3, 2, white, king).
cell(2, 2, black, king).
cell(4, 3, white, queen).
cell(2, 3, black, horse).
cell(3, 3, white, horse).
cell(3, 1, black, pawn).


shift_right(0).
shift_right(C) :-
    C > 0, \+ cell(C, R, Color, Piece),
    Cleft is C-1, shift_right(Cleft).
shift_right(C) :-
    C > 0, Cleft is C-1, Cright is C+1,
    retract(cell(C, R, Color, Piece)),
    assertz(cell(Cright, R, Color, Piece)),
    shift_right(Cleft).


reset_board :-
    retractall(cell(_,_,_,_)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).

play_piece(R, C, Color,Piece) :-
    assertz(cell(R, C, Color, Piece)).