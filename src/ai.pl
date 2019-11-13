:- ensure_loaded('placement.pl').

ai_turn(Player, Difficulty) :-
    valid_moves(Player, Moves),
    choose_move(Difficulty, Moves, Move),
    ai_action(Player, Move),
    readjust_board,
    check_queens_death.

choose_move(random, Moves, Move) :-
    random_member(Move, Moves).

ai_action(Player, [Piece, X, Y]) :-
    move_ai(Player, Piece, X, Y);
    (place_ai(Player, Piece, X, Y), 
    ((valid_special_play(Piece, Player, Moves),
      choose_move(random, Moves, Move),
      special_power_on_placement_ai(Piece, Player, Move)); true)).

valid_moves(Player, PossibleMoves) :-
    findall([Piece, X, Y],(piece(Piece, Player, _), between(0, 5, X),between(0, 5, Y), possible_play(Player, Piece, X, Y)), PossibleMoves).

valid_special_play(pawn, Player, PossibleMoves) :-
    findall([X, Y],(between(0, 5, X),between(0, 5, Y), possible_play(Player, pawn, X, Y)), PossibleMoves).

valid_special_play(bishop, Player, PossibleMoves) :-
    findall([X, Y],(cell(X, Y, TargetPlayer, Piece), possible_removable(Player, TargetPlayer, Piece, X, Y)), PossibleMoves).

possible_play(Player, Piece, X, Y) :-
    check_placement(Player, Piece, X, Y);
    check_movement(Player, Piece, X, Y).
