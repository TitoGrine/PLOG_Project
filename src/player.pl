:- ensure_loaded('display.pl').
:- ensure_loaded('movement.pl').
:- ensure_loaded('placement.pl').

player_turn(Player) :-
    repeat,
    (choose_piece(Player, Piece),
     display_board,
     player_action(Player, Piece)
    ),!,
    readjust_board,
    check_queens_death.

player_action(Player, Piece) :-
    move(Player, Piece);
    (place(Player, Piece) , special_power_on_placement(Piece, Player)).