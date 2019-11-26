:- ensure_loaded('placement.pl').

% It calculates the valid moves for the AI, and, depending on the Level choosen, decides what
% move to make and takes action.
ai_turn(Player, Level) :-
    valid_moves(Player, Moves),
    choose_move(Level, Player, Moves, Move),
    ai_action(Player, Level, Move),
    readjust_board,             % If the pieces are in the outer ring of the board, it reajusts it by shifting appropriately.
    check_queens_death.         % Checks if any of the queens are surrounded (if so they will be removed from the game).

% ====================================================================================

% From a list of possible moves, chooses a random one. Used by random AI.
choose_move(random, _, Moves, Move) :-
    random_member(Move, Moves).

% From a list of possible moves, calculates the value (integer) of each one, creates a list with only the moves with
% the highest value (best play), and randomly picks one. Used by "smart" AI.
choose_move(smart, Player, Moves, BestMove) :-
    calculate_moves_value(Player, Moves, ValuedMoves),
    max_value_list(ValuedMoves, BestValue),
    make_best_moves_list(ValuedMoves, BestValue, BestMoves),
    random_member(BestMove, BestMoves), !.

% From a list of possible special moves, chooses a random one. Used by random AI.
choose_special_move(random, _, _, Moves, Move) :-
    random_member(Move, Moves).

% From a list of possible special moves, calculates the value (integer) of each one, it then creates a list with only
% the highest value moves (best ones), and randomly chooses one. Used by "smart" AI.
choose_special_move(smart, Player, Piece, Moves, BestMove) :-
    calculate_special_moves_value(Player, Piece, Moves, ValuedMoves),
    max_value_list(ValuedMoves, BestValue),
    make_best_special_moves_list(ValuedMoves, BestValue, BestMoves),
    random_member(BestMove, BestMoves), !.

% ====================================================================================

% Depending on the piece selected (if it's on the board or not), the action it takes can either be a move or a placement.
% It always returns true.
ai_action(Player, Level, [Piece, X, Y]) :-
    move_ai(Player, Piece, X, Y);
    (place_ai(Player, Piece, X, Y), 
    ((valid_special_play(Piece, Player, Moves),                      % If its a placement and the piece has a special ability, it will get a list of possible moves.
      choose_special_move(Level, Player, Piece, Moves, Move),!,      % Chooses a move from the list, criteria depends on the Level of the AI.
      special_ability_on_placement_ai(Piece, Player, Move)); true)). % Executes the special move.

% ====================================================================================

% Finds all the possible moves a player can make (placement and movement), creating a list of type [[Piece0, X0, Y0], [Piece1, X1, Y1], ..., [PieceN, XN, YN]].
valid_moves(Player, PossibleMoves) :-
    findall([Piece, X, Y],(piece(Piece, Player, _), between(0, 5, X),between(0, 5, Y), possible_play(Player, Piece, X, Y)), PossibleMoves).

% Finds all possible moves for the given Piece belonging to Player, creating a list of type [[X0, Y0], [X1, Y1], ..., [XN, YN]].
valid_piece_moves(Player, Piece, PossibleMoves) :-
    setof([X, Y],(between(0, 5, X),between(0, 5, Y), possible_play(Player, Piece, X, Y)), PossibleMoves) ;
    (append([], [], PossibleMoves), true).

% Finds all possible moves for the special ability of the pawn belonging ot the Player, when placed. Creates a list of type [[pawn, X0, X0], ..., [pawn, XN, YN]].
valid_special_play(pawn, Player, PossibleMoves) :-
    cell(PX, PY, Player, pawn), append([[PX, PY]], [], Move1),
    valid_piece_moves(Player, pawn, Moves2),
    append(Move1, Moves2, PossibleMoves).

% Finds all possible pieces that the bishop of Player can remove with its special ability, when placed. Creates a list of type [[X0, X0], ..., [XN, YN]].
valid_special_play(bishop, Player, PossibleMoves) :-
    findall([X, Y],(cell(X, Y, TargetPlayer, Piece), possible_removable(Player, TargetPlayer, Piece, X, Y)), PossibleMoves).

% ====================================================================================

% Checks if the Piece belonging to the Player can either be placed or moved to position (X, Y).
possible_play(Player, Piece, X, Y) :-
    check_placement(Player, Piece, X, Y);
    check_movement(Player, Piece, X, Y).

% Travels the list of moves and adds the corresponding value to each move.
calculate_moves_value(_, [], []).

calculate_moves_value(Player, [Move|Rest], [ValuedMove|ValuedMoves]) :-
    add_value(Player, Move, ValuedMove),
    calculate_moves_value(Player, Rest, ValuedMoves).

% Travels the list of speacial moves and adds the corresponding value to each one of them.
calculate_special_moves_value(_ , _, [], []).

calculate_special_moves_value(Player, Piece, [Move|Rest], [ValuedMove|ValuedMoves]) :-
    add_special_value(Player, Piece, Move, ValuedMove),
    calculate_special_moves_value(Player, Piece, Rest, ValuedMoves).

% ====================================================================================

% Calculates and adds the value (how good a play is) to the given play.
add_value(Player, [Piece, X, Y], [Value, Piece, X, Y]) :-
    value(Player, Piece, X, Y, Value).

% Evaluates the value of a pawn move, that can be slightly different from the other pieces given the fact that it can move immediatly after it's placed.
value(Player, pawn, X, Y, Value) :-
    \+ cell(_, _, Player, pawn),
    value_next_move(Player, V2, pawn, X, Y),
    value_defensive(Player, V3, pawn, X, Y),
    ((V3 =:= -100, Value is V3);
     (V2 >= 1, V3 >= 6, Value is 120); %In this case, it is very likely that the ai can win the game with a special pawn move
      Value is (0 + (0.1 * V2) + V3)).

% Evaluates the value of the given Piece play by the Player, considering 3 factors:
% - Its immediate offensive value;
% - Its usefulness in the next move;
% - It's immediatte defensive value.
value(Player, Piece, X, Y, Value) :-
    value_offensive(Player, V1, Piece, X, Y),
    value_next_move(Player, V2, Piece, X, Y),
    value_defensive(Player, V3, Piece, X, Y),
    ((V3 =:= -100, Value is V3);
      Value is (0 + (1.2 * V1) + (0.05 * V2) + V3)).

% ====================================================================================

% Evaluates the offensive value of the given Piece placement in (X, Y), by considering the number of pieces that will surround
% the enemy king and queen. The higher the number the higher the value. Encourages agressive behaviour.
value_offensive(Player, Value, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece),
    opposite(Player, EnemyPlayer),
    count_attackers(EnemyPlayer, king, NKing),
    count_attackers(EnemyPlayer, queen, NQueen),
    delete_from_database(X, Y, Player, Piece),
    add_offensive_values(NKing, NQueen, Value).

% Evaluates the offensive value of the given Piece move to (X, Y), by considering the number of pieces that will surround
% the enemy king and queen. The higher the number the higher the value. Encourages agressive behaviour.
value_offensive(Player, Value, Piece, X, Y) :-
    cell(X0, Y0, Player, Piece),
    change_database(X, Y, Player, Piece),
    opposite(Player, EnemyPlayer),
    count_attackers(EnemyPlayer, king, NKing),
    count_attackers(EnemyPlayer, queen, NQueen),
    change_database(X0, Y0, Player, Piece),
    add_offensive_values(NKing, NQueen, Value).

% Depending on the number of pieces surrounding the enemy king and queen, calculates the offenses' value.
add_offensive_values(4, _, Value) :-
    Value is 100.               % If the play makes it so the enemy king is surrounded (victory) it's value should be very high.
add_offensive_values(NKing, 4, Value) :-
    Value is 2.0 * NKing + 10.  % If the play makes it so the enemy queen is surrounded (killing it) it's value should be higher
add_offensive_values(NKing, NQueen, Value) :-
    Value is 2.0 * NKing + NQueen.

% ====================================================================================

% Calculates and adds the value of the next move (how good a play is given it's possible next moves) to the given play.
add_next_move_values(_, [], FinalValue, FinalValue).
add_next_move_values(Player, [[X, Y]| Rest], CurrentValue, FinalValue) :-
    opposite(Player, Enemy),
    ((side_piece(X, Y, Enemy, king), side_piece(X, Y, Enemy, queen), NewValue is CurrentValue + 1.5);
     (side_piece(X, Y, Enemy, king), NewValue is CurrentValue + 1);
     (side_piece(X, Y, Enemy, queen), NewValue is CurrentValue + 0.5);
     (NewValue is CurrentValue)),
    add_next_move_values(Player, Rest, NewValue, FinalValue).

% Evaluates the value of the next possible moves by the piece that the player just placed.
value_next_move(Player, Value, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece),
    valid_piece_moves(Player, Piece, Moves),
    add_next_move_values(Player, Moves, 0, Value),
    delete_from_database(X, Y, Player, Piece).

% Evaluates the value of the next possible moves by the piece that the player just moved.
value_next_move(Player, Value, Piece, X, Y) :-
    cell(X0, Y0, Player, Piece),
    change_database(X, Y, Player, Piece),
    valid_piece_moves(Player, Piece, Moves),
    add_next_move_values(Player, Moves, 0, Value),
    change_database(X0, Y0, Player, Piece).

% ====================================================================================

% Evaluates the defensive value of the given Piece placement in (X, Y), by considering the number of pieces that will surround
% the players king and queen. The higher the number the lower the value. Encourages defensive behaviour.
value_defensive(Player, Value, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece),
    count_attackers(Player, king, NKing),
    count_attackers(Player, queen, NQueen),
    delete_from_database(X, Y, Player, Piece),
    add_defensive_values(NKing, NQueen, Value).

% Evaluates the defensive value of the given Piece move to (X, Y), by considering the number of pieces that will surround
% the players king and queen. The higher the number the lower the value. Encourages defensive behaviour.
value_defensive(Player, Value, Piece, X, Y) :-
    cell(X0, Y0, Player, Piece),
    change_database(X, Y, Player, Piece),
    count_attackers(Player, king, NKing),
    count_attackers(Player, queen, NQueen),
    change_database(X0, Y0, Player, Piece),
    add_defensive_values(NKing, NQueen, Value).

% Depending on the number of pieces surrounding the players king and queen, calculates the defensive value.
add_defensive_values(4, _, Value) :-
    Value is -100.                  % If the play makes it so the players king is surrounded (defeat), then its value should be very low.
add_defensive_values(NKing, 4, Value) :-
    Value is -2.0 * NKing - 10.     % If the play makes it so the players queen is surrounded(killing it), then its value should be lower. 
add_defensive_values(NKing, NQueen, Value) :-
    Value is -2.0 * NKing - NQueen.

% ====================================================================================

% Calculates and adds the value (how good a special play is) to the given special play.
add_special_value(Player, Piece, [X, Y], [Value, X, Y]) :-
    value_special(Player, Piece, X, Y, Value).

% Evaluates the value of a pawns' special ability (it can move immediatly after it's placed).
value_special(Player, pawn, X, Y, Value) :-
    value(Player, pawn, X, Y, Value).

% Evaluates the value of a bishops' special ability (it can remove a piece from the board when placed).
value_special(Player, bishop, X, Y, Value) :-
    opposite(Player, Enemy),
    cell(X, Y, PieceColor, Piece),
    value_offensive_remove(Enemy, V1, X, Y),  % If it is the players king/queen then the removal is strongly encouraged.
    value_defensive_remove(Player, V2, X, Y), % Otherwise it is strongly disencouraged.
    value_piece_remove(Player, PieceColor, Piece, V3),
    count_attackers(Player, king, NFriendlyKing),
    count_attackers(Enemy, king, NEnemyKing),
    Value is 0 + NFriendlyKing * V1 + NEnemyKing * V2 + V3. % The higher the number of pieces around a king, the more important is the removal of a piece around it.

% The following predicates atribute a value to each piece removal
% If the piece to be removed is a players piece, then it is not encouraged to remove it
value_piece_remove(Player, Player, pawn, -0.5).
value_piece_remove(Player, Player, knight, -1).
value_piece_remove(Player, Player, rook, -1.5).
value_piece_remove(Player, Player, queen, -2).
% When the piece is an enemy piece, then it is incentivised to remove it
value_piece_remove(_, _, queen, 2).
value_piece_remove(_, _, rook, 1.5).
value_piece_remove(_, _, knight, 1).
value_piece_remove(_, _, pawn, 0.5).
value_piece_remove(_, _, bishop, 0).

% Calculates the value of a piece removal based on the number of pieces that still surround the players king and queen.
% It's encouraged that the player attempts to remove first a piece that surrounds its king or queen, thus its value is higher if so.
value_defensive_remove(Player, Value, X, Y) :-
    side_piece(X, Y, Player, king), side_piece(X, Y, Player, queen), Value is 3.

value_defensive_remove(Player, Value, X, Y) :-
    side_piece(X, Y, Player, king), Value is 2.
value_defensive_remove(Player, Value, X, Y) :-
    side_piece(X, Y, Player, queen), Value is 1.

% If the piece removed doesn't surround any of the players important pieces, then its removal is neither encouraged nor discouraged.
value_defensive_remove(_, 0, _, _).

% Calculates the value of a piece removal based on the number of pieces that still surround the enemy's king and queen.
% It's encouraged that the player attempts to remove a piece that surrounds its king or queen as last resort. Thus it's value is lower if so.
value_offensive_remove(Enemy, Value, X, Y) :-
    side_piece(X, Y, Enemy, king), side_piece(X, Y, Enemy, queen), Value is -3.

value_offensive_remove(Enemy, Value, X, Y) :-
    side_piece(X, Y, Enemy, king), Value is -2.
value_offensive_remove(Enemy, Value, X, Y) :-
    side_piece(X, Y, Enemy, queen), Value is -1.

% If the piece removed doesn't surround any of the enemy's important pieces, then its removal is neither encouraged nor discouraged.
value_offensive_remove(_, 0, _, _).
