:- ensure_loaded('player.pl').
:- ensure_loaded('ai.pl').

% Used to know the color of the pieces of the next turns' player/AI.
next_player(white, black).
next_player(black, white).

% ====================================================================================

% Starts game mode: Player vs Player.
start_game(player, player) :-
    nl,
    display_board,
    play_player_player(white),
    reset_board.

% Starts game mode: Player vs AI. 
% Difficulty dictates the strategy used by the AI to play. 
start_game(player, ai, Difficulty) :-
    nl,
    display_board,
    play_player_AI(player, Difficulty),
    reset_board.

% Starts game mode: AI vs AI. 
% DifficultyWhite dictates the strategy used by the white AI to play.
% DifficultyBlack dictates the strategy used by the black AI to play. 
start_game(ai, ai, DifficultyWhite, DifficultyBlack) :-
    nl,
    display_board,
    play_AI_AI(white, DifficultyWhite, DifficultyBlack),
    reset_board.

% ====================================================================================

% Alternates between each player until at least one of the kings is surrounded.
play_player_player(Player) :- 
    player_turn(Player),
    display_board,
    next_player(Player, NextPlayer),
    (game_over(_) ; play_player_player(NextPlayer)).

% Let's the Player have its turn, and if there is no king surrounded will call for the AI's turn.
play_player_AI(player, Difficulty) :-
    player_turn(white),
    display_board,
    (game_over(_) ; play_player_AI(ai, Difficulty)).

% Let's the AI have its turn, and if there is no king surrounded will call for the Players's turn.
play_player_AI(ai, Difficulty) :-
    ai_turn(black, Difficulty),
    display_board,
    (game_over(_) ; play_player_AI(player, Difficulty)).

% Alternates between the two AI's until at least one of the kings is surrounded.
play_AI_AI(Player, CurrentDifficulty, NextDifficulty) :-
    ai_turn(Player, CurrentDifficulty),
    display_board,
    next_player(Player, NextPlayer),
    (game_over(_) ; play_AI_AI(NextPlayer, NextDifficulty, CurrentDifficulty)).

% ====================================================================================

% Checks if any of the kings is surrounded. If both are, displays a tie message and returns true. 
% If only one of them is, then the opposite color player has won, a dedicated message is displayed and
% return true. If none are surrounded it returns false.
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
