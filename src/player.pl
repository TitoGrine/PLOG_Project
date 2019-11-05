:- include('display.pl').
:- include('utils.pl').

choose_piece(Player, Piece) :-
    repeat,
    format('Player ~s choose a piece: ', [Player]),
    read(Piece),
    piece(Piece, Player, _).
