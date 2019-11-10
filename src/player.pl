:- include('display.pl').
:- include('utils.pl').
:- include('movement.pl').
:- include('placement.pl').
:- include('logic.pl').

player_turn(Player) :-
    repeat,
    (choose_piece(Player, Piece),
    (
        move(Player, Piece);
        (place(Player, Piece) , (Piece \== pawn; special_pawn(Player)))
    )
    ),!,
    readjust_board,
    check_queens_death.
