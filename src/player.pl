:- include('display.pl').
:- include('utils.pl').
:- include('movement.pl').
:- include('placement.pl').
:- include('logic.pl').

player_turn(Player) :-
    choose_piece(Player, Piece),
    (move(Player, Piece); place(Player, Piece)),
    readjust_board.

choose_piece(Player, Piece) :-
    repeat,
    format('Player ~s choose a piece: ', [Player]),
    read(Piece),
    piece(Piece, Player, _).
