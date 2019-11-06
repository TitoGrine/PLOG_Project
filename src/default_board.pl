:- dynamic(cell/4).

add_to_database(X, Y, Color, Piece) :-
    assertz(cell(X, Y, Color, Piece)).

% Starting board

cell(3, 2, white, king).
cell(2, 2, black, king).
