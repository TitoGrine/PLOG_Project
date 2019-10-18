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

cell(2, 2, black, king).
cell(3, 2, white, king).
% cell(0,0, black, pawn).
% cell(4, 5, white, bishop).

reset_board :-
    retractAll(cell(_,_,_)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).