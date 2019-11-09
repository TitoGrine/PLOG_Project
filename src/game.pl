:- include('player.pl').
:- include('logic.pl').

start_game :-
    display_board,
    play(white).

play(Player) :- 
    player_turn(Player),
    display_board,
    next_player(Player, NextPlayer),
    (end_game ; play(NextPlayer)).

next_player(white, black).
next_player(black, white).

end_game :-
    cell(WKC, WKR, white, king),
    cell(BKC, BKR, black, king),
    !,
    ((check_surrounded(WKR, WKC),
     write('The black player has won!!'));
    (check_surrounded(BKR, BKC),
     write('The white player has won!!'))).


