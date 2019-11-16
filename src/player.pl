:- ensure_loaded('display.pl').
:- ensure_loaded('movement.pl').
:- ensure_loaded('placement.pl').

% Allows the player to choose a piece, redisplays the board and makes the plater take an action.
% If the player cancels the action then the prompt to select the piece is shown once more.
player_turn(Player) :-
    repeat,
    (choose_piece(Player, Piece),
     display_board,
     player_action(Player, Piece)
    ),!,
    readjust_board,             % If the pieces are in the outer ring of the board, it reajusts it by shifting appropriately
    check_queens_death.         % Checks if any of the queens are surrounded (if so they will be removed from the game)

% Depending on the piece selected (if it's on the board or not), the action it takes can either be a move ora placement.
% It always returns true.
player_action(Player, Piece) :-
    move(Player, Piece);
    (place(Player, Piece) , special_ability_on_placement(Piece, Player)). % If its a placement and the piece has a special ability, it will execute it.