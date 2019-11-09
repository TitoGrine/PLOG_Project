:- include('display.pl').
:- include('utils.pl').
:- include('movement.pl').
:- include('placement.pl').

player_turn(Player) :-
    choose_piece(Player, Piece),
    (move(Player, Piece); place(Player, Piece)).

choose_piece(Player, Piece) :-
    repeat,
    format('Player ~s choose a piece: ', [Player]),
    read(Piece),
    piece(Piece, Player, _).
