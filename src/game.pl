:- ensure_loaded('player.pl').
:- ensure_loaded('ai.pl').

start_game :-
    display_board,
    play_player_AI(player),
    reset_board.

play_player_player(Player) :- 
    player_turn(Player),
    display_board,
    next_player(Player, NextPlayer),
    (end_game ; play_player_player(NextPlayer)).

play_player_AI(player) :-
    player_turn(white),
    display_board,
    (end_game ; play_player_AI(ai)).

play_player_AI(ai) :-
    ai_turn(black),
    display_board,
    (end_game ; play_player_AI(player)).

next_player(white, black).
next_player(black, white).

end_game :-
    cell(WKC, WKR, white, king),
    cell(BKC, BKR, black, king),
    !,
    ((check_surrounded(WKR, WKC),
     ansi_format([fg(yellow)], 'The black player has won!!', []));
    (check_surrounded(BKR, BKC),
     ansi_format([fg(yellow)], 'The white player has won!!', []))).


