:- ensure_loaded('player.pl').
:- ensure_loaded('ai.pl').

start_game(player, player) :-
    display_board,
    play_player_player(white),
    reset_board.

start_game(player, ai, Difficulty) :-
    display_board,
    play_player_AI(player, Difficulty),
    reset_board.

start_game(ai, ai, DifficultyWhite, DifficultyBlack) :-
    display_board,
    play_AI_AI(white, DifficultyWhite, DifficultyBlack),
    reset_board.

play_player_player(Player) :- 
    player_turn(Player),
    display_board,
    next_player(Player, NextPlayer),
    (game_over(_) ; play_player_player(NextPlayer)).

play_player_AI(player, Difficulty) :-
    player_turn(white),
    display_board,
    (game_over(_) ; play_player_AI(ai, Difficulty)).

play_player_AI(ai, Difficulty) :-
    ai_turn(black, Difficulty),
    display_board,
    (game_over(_) ; play_player_AI(player, Difficulty)).

play_AI_AI(Player, CurrentDifficulty, NextDifficulty) :-
    ai_turn(Player, CurrentDifficulty),
    display_board,
    next_player(Player, NextPlayer),
    (game_over(_) ; play_AI_AI(NextPlayer, NextDifficulty, CurrentDifficulty)).

next_player(white, black).
next_player(black, white).

game_over(Winner) :-
    cell(WKC, WKR, white, king),
    cell(BKC, BKR, black, king),
    !,
    ((check_surrounded(WKR, WKC), check_surrounded(BKR, BKC),
     ansi_format([fg(yellow)], 'It\'s a draw!!', []), Winner = black_white), nl;
    (check_surrounded(WKR, WKC),
     ansi_format([fg(yellow)], 'The black player has won!!', []), Winner = black), nl;
    (check_surrounded(BKR, BKC),
     ansi_format([fg(yellow)], 'The white player has won!!', []), Winner = white), nl).
