:- ensure_loaded('placement.pl').

ai_turn(Player, Level) :-
    valid_moves(Player, Moves),
    choose_move(Level, Player, Moves, Move),
    ai_action(Player, Level, Move),
    readjust_board,
    check_queens_death.

choose_move(random, _, Moves, Move) :-
    random_member(Move, Moves).

choose_move(high, Player, Moves, Best) :-
    calculate_moves_value(Player, Moves, ValuedMoves),
    max_value_list(ValuedMoves, [_|Best]).

choose_special_move(random, _, _, Moves, Move) :-
    random_member(Move, Moves).

choose_special_move(high, Player, Piece, Moves, Best) :-
    random_member(Best, Moves).
    %calculate_special_moves_value(Player, Piece, Moves, ValuedMoves),
    %max_value_list(ValuedMoves, Best).

ai_action(Player, Level, [Piece, X, Y]) :-
    move_ai(Player, Piece, X, Y);
    (place_ai(Player, Piece, X, Y), 
    ((valid_special_play(Piece, Player, Moves),
      choose_special_move(Level, Player, Piece, Moves, Move),
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

calculate_special_moves_value(Player, Piece, Moves, ValuedMoves).

add_value(Player, [Piece, X, Y], [Value, Piece, X, Y]) :-
    value(Player, Value, Piece, X, Y).

value(Player, Value, Piece, X, Y) :-
    value_offensive(Player, V1, Piece, X, Y),
    value_next_move(Player, V2, Piece, X, Y),
    value_defensive(Player, V3, Piece, X, Y),
    ((V3 =:= -100, Value is V3);
      Value is (0 + (1.2 * V1) + (0.1 * V2) + V3)).

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

add_next_move_values(_, [], Acc, Acc).
add_next_move_values(Player, [[X, Y]| Rest], Current, Acc) :-
    opposite(Player, Enemy),
    ((side_piece(X, Y, Enemy, king), New is Current + 1);
     (side_piece(X, Y, Enemy, queen), New is Current + 0.5);
     (side_piece(X, Y, Enemy, king), side_piece(X, Y, Enemy, king), New is Current + 1.5);
     (New is Current)),
    add_next_move_values(Player, Rest, New, Acc).

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
    add_defensive_values(NKing, NQueen, Value).

add_defensive_values(4, _, Value) :-
    Value is -100.
add_defensive_values(NKing, 4, Value) :-
    Value is -2.0 * NKing - 10.
add_defensive_values(NKing, NQueen, Value) :-
    Value is -2.0 * NKing - NQueen.