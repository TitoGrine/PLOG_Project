:- ensure_loaded('placement.pl').

ai_turn(Player, Level) :-
    valid_moves(Player, Moves),
    choose_move(Level, Player, Moves, Move),
    ai_action(Player, Level, Move),
    readjust_board,
    check_queens_death.

choose_move(random, _, Moves, Move) :-
    random_member(Move, Moves).

choose_move(high, Player, Moves, BestMove) :-
    calculate_moves_value(Player, Moves, ValuedMoves),
    max_value_list(ValuedMoves, BestValue),
    make_best_moves_list(ValuedMoves, BestValue, BestMoves),
    random_member(BestMove, BestMoves).

choose_special_move(random, _, _, Moves, Move) :-
    random_member(Move, Moves).

choose_special_move(high, Player, Piece, Moves, BestMove) :-
    calculate_special_moves_value(Player, Piece, Moves, ValuedMoves),
    max_value_list(ValuedMoves, BestValue),
    make_best_moves_list(ValuedMoves, BestValue, BestMoves),
    random_member(BestMove, BestMoves).

ai_action(Player, Level, [Piece, X, Y]) :-
    move_ai(Player, Piece, X, Y);
    (place_ai(Player, Piece, X, Y), 
    ((valid_special_play(Piece, Player, Moves),
      choose_special_move(Level, Player, Piece, Moves, Move),!,
      special_power_on_placement_ai(Piece, Player, Move)); true)).

valid_moves(Player, PossibleMoves) :-
    findall([Piece, X, Y],(piece(Piece, Player, _), between(0, 5, X),between(0, 5, Y), possible_play(Player, Piece, X, Y)), PossibleMoves).

valid_piece_moves(Player, Piece, PossibleMoves) :-
    findall([X, Y],(between(0, 5, X),between(0, 5, Y), possible_play(Player, Piece, X, Y)), PossibleMoves).

valid_special_play(pawn, Player, PossibleMoves) :-
    findall([X, Y],(between(0, 5, X),between(0, 5, Y), possible_play(Player, pawn, X, Y)), PossibleMoves).

valid_special_play(bishop, Player, PossibleMoves) :-
    findall([X, Y],(cell(X, Y, TargetPlayer, Piece), possible_removable(Player, TargetPlayer, Piece, X, Y)), PossibleMoves).

possible_play(Player, Piece, X, Y) :-
    check_placement(Player, Piece, X, Y);
    check_movement(Player, Piece, X, Y).

calculate_moves_value(_, [], []).
calculate_moves_value(Player, [Move|Rest], [ValuedMove|ValuedMoves]) :-
    add_value(Player, Move, ValuedMove),
    calculate_moves_value(Player, Rest, ValuedMoves).

calculate_special_moves_value(_ , _, [], []).
calculate_special_moves_value(Player, Piece, [Move|Rest], [ValuedMove|ValuedMoves]) :-
    add_special_value(Player, Piece, Move, ValuedMove),
    calculate_special_moves_value(Player, Piece, Rest, ValuedMoves).

%===============================================
% Predicates to evaluate a normal move
add_value(Player, [Piece, X, Y], [Value, Piece, X, Y]) :-
    value(Player, Piece, X, Y, Value).

value(Player, pawn, X, Y, Value) :-
    \+ cell(_, _, Player, pawn),
    value_offensive(Player, V1, pawn, X, Y),
    value_next_move(Player, V2, pawn, X, Y),
    value_defensive(Player, V3, pawn, X, Y),
    ((V3 =:= -100, Value is V3);
     (V2 >= 1, V3 >= 6, Value is 120); %In this case, it is very likely that the ai can win the game with a special pawn move
      Value is (0 + (1.2 * V1) + V3)). % Next_move : (0.1 * V2) (this was making the ai too much predictable)

value(Player, Piece, X, Y, Value) :-
    value_offensive(Player, V1, Piece, X, Y),
    value_next_move(Player, V2, Piece, X, Y),
    value_defensive(Player, V3, Piece, X, Y),
    ((V3 =:= -100, Value is V3);
      Value is (0 + (1.2 * V1) + (0.05 * V2) + V3)).

value_offensive(Player, Value, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece),
    opposite(Player, EnemyPlayer),
    count_attackers(EnemyPlayer, king, NKing),
    count_attackers(EnemyPlayer, queen, NQueen),
    delete_from_database(X, Y, Player, Piece),
    add_offensive_values(NKing, NQueen, Value).

value_offensive(Player, Value, Piece, X, Y) :-
    cell(X0, Y0, Player, Piece),
    change_database(X, Y, Player, Piece),
    opposite(Player, EnemyPlayer),
    count_attackers(EnemyPlayer, king, NKing),
    count_attackers(EnemyPlayer, queen, NQueen),
    change_database(X0, Y0, Player, Piece),
    add_offensive_values(NKing, NQueen, Value).

add_offensive_values(4, _, Value) :-
    Value is 100.
add_offensive_values(NKing, 4, Value) :-
    Value is 2.0 * NKing + 10.
add_offensive_values(NKing, NQueen, Value) :-
    Value is 2.0 * NKing + NQueen.

value_next_move(Player, Value, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece),
    valid_piece_moves(Player, Piece, Moves),
    add_next_move_values(Player, Moves, 0, Value),
    delete_from_database(X, Y, Player, Piece).

value_next_move(Player, Value, Piece, X, Y) :-
    cell(X0, Y0, Player, Piece),
    change_database(X, Y, Player, Piece),
    valid_piece_moves(Player, Piece, Moves),
    add_next_move_values(Player, Moves, 0, Value),
    change_database(X0, Y0, Player, Piece).

add_next_move_values(_, [], FinalValue, FinalValue).
add_next_move_values(Player, [[X, Y]| Rest], CurrentValue, FinalValue) :-
    opposite(Player, Enemy),
    ((side_piece(X, Y, Enemy, king), side_piece(X, Y, Enemy, queen), NewValue is CurrentValue + 1.5);
     (side_piece(X, Y, Enemy, king), NewValue is CurrentValue + 1);
     (side_piece(X, Y, Enemy, queen), NewValue is CurrentValue + 0.5);
     (NewValue is CurrentValue)),
    add_next_move_values(Player, Rest, NewValue, FinalValue).

value_defensive(Player, Value, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece),
    count_attackers(Player, king, NKing),
    count_attackers(Player, queen, NQueen),
    delete_from_database(X, Y, Player, Piece),
    add_defensive_values(NKing, NQueen, Value).

value_defensive(Player, Value, Piece, X, Y) :-
    cell(X0, Y0, Player, Piece),
    change_database(X, Y, Player, Piece),
    count_attackers(Player, king, NKing),
    count_attackers(Player, queen, NQueen),
    change_database(X0, Y0, Player, Piece),
    write(NKing),nl,
    add_defensive_values(NKing, NQueen, Value).

add_defensive_values(4, _, Value) :-
    Value is -100.
add_defensive_values(NKing, 4, Value) :-
    Value is -2.0 * NKing - 10.
add_defensive_values(NKing, NQueen, Value) :-
    Value is -2.0 * NKing - NQueen.

% ===============================================
% Predicates to evaluate a special move
add_special_value(Player, Piece, [X, Y], [Value, X, Y]) :-
    value_special(Player, Piece, X, Y, Value).

value_special(Player, pawn, X, Y, Value) :-
    value(Player, pawn, X, Y, Value).

value_special(Player, bishop, X, Y, Value) :-
    opposite(Player, Enemy),
    cell(X, Y, PieceColor, Piece),
    ((Player == PieceColor, value_defensive_remove(Enemy, V1, X, Y));  % If it is the players king/queen then the removal is strongly encouraged.
                            value_offensive_remove(Player, V1, X, Y)), % Otherwise it is strongly disencouraged.
    value_piece_remove(Player, PieceColor, Piece, V2),
    count_attackers(PieceColor, king, NKing),
    Value is 0 + NKing * V1 + V2. % The higher the number of pieces around a king, the more important is the removal of a piece around it.

% The following predicates atribute a value to each piece removal
% If the piece to be removed is a players piece, then it is not encouraged to remove it
value_piece_remove(Player, Player, queen, -2).
value_piece_remove(Player, Player, rook, -1.5).
value_piece_remove(Player, Player, knight, -1).
value_piece_remove(Player, Player, pawn, -0.5).
% When the piece is an enemy piece, then the ai is encouraged to do remove it
value_piece_remove(_, _, bishop, 0).
value_piece_remove(_, _, pawn, 0.5).
value_piece_remove(_, _, knight, 1).
value_piece_remove(_, _, rook, 1.5).
value_piece_remove(_, _, queen, 2).

value_defensive_remove(Player, Value, X, Y) :-
    side_piece(X, Y, Player, king),!, side_piece(X, Y, Player, queen), Value is 3.

value_defensive_remove(Player, Value, X, Y) :-
    side_piece(X, Y, Player, king), Value is 2.

value_defensive_remove(Player, Value, X, Y) :-
    side_piece(X, Y, Player, queen), Value is 1.

value_defensive_remove(_, 0, _, _).

value_offensive_remove(Enemy, Value, X, Y) :-
    side_piece(X, Y, Enemy, king),!, side_piece(X, Y, Enemy, queen), Value is -3.

value_offensive_remove(Enemy, Value, X, Y) :-
    side_piece(X, Y, Enemy, king), Value is -2.

value_offensive_remove(Enemy, Value, X, Y) :-
    side_piece(X, Y, Enemy, queen), Value is -1.

value_offensive_remove(_, 0, _, _).
