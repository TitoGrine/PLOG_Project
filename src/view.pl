% Default board

cell(2, 2, black, king).
cell(3, 2, white, king).
cell(0,0, black, pawn).
cell(4, 5, white, bishop).

% Pieces

piece(king, black, 'K').
piece(king, white, 'K').
piece(queen, black, 'Q').
piece(queen, white, 'Q').
piece(bishop, black, 'B').
piece(bishop, white, 'B').
piece(rook, black, 'R').
piece(rook, white, 'R').
piece(knight, black, 'H').
piece(knight, white, 'H').
piece(pawn, black, 'P').
piece(pawn, white, 'P').


% Display

display_board :-
    nl,
    display_coordinates,
    \+display_board([6, 6], 0).

display_board([R, C], X) :-
    \+write_line(C, 0), nl,
    X < R, 
    display_coordinates(X),
    \+display_row([X, C], 0), nl,
    N is X + 1, display_board([R, C], N).
   
display_row([R, C], X) :-
    X < C, display_piece(R, X),
    N is X + 1, display_row([R, C], N).

display_piece(R, C) :-
    cell(C, R, Color, Piece),
    display(Piece, Color).

display_piece(R, C) :-
    \+cell(C, R, Color, Piece),
    write('     |').

display_coordinates :-
    write('     |  0  |  1  |  2  |  3  |  4  |  5  |'),
    nl.

display_coordinates(R) :-
    write('  '),
    write(R),
    write('  |').

write_line(Length, X) :-
    X =< Length, write('-----+'),
    N is X + 1, write_line(Length, N).

display(Piece, Color) :-
    piece(Piece, Color, Char),
    write('  '),
    write(Char),
    write('  |').

