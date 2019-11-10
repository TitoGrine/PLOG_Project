:- dynamic(cell/4).
:- dynamic(dead/3).
:- dynamic(castling_available/1).

add_to_database(X, Y, Color, Piece) :-
    assertz(cell(X, Y, Color, Piece)).

kill_from_database(X, Y, Color, queen) :-
    retractall(cell(X, Y, Color, queen)),!,
    retractall(dead(Color, queen, _)),!,
    asserta(dead(Color, queen, true)).   

delete_from_database(X, Y, Color, Piece) :-
    retractall(cell(X, Y, Color, Piece)).

change_database(X, Y, Color, Piece) :-
    retractall(cell(_, _, Color, Piece)),!,
    assertz(cell(X, Y, Color, Piece)).

castling_done(Color) :-
    retractall(castling_available(Color)).

% Starting board

dead(white, queen, false).
dead(black, queen, false).

castling_available(white).
castling_available(black).

cell(3, 2, white, king).
cell(2, 2, black, king).
