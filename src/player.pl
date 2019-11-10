:- include('display.pl').
:- include('utils.pl').
:- include('movement.pl').
:- include('placement.pl').
:- include('logic.pl').

player_turn(Player) :-
    repeat,
    (choose_piece(Player, Piece),
     player_action(Player, Piece)
    ),!,
    readjust_board,
    check_queens_death.

player_action(Player, Piece) :-
    move(Player, Piece);
    (place(Player, Piece) , special_power_on_placement(Piece, Player)).