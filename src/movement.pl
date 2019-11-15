:- ensure_loaded('logic.pl').

move(Player, Piece) :-
    cell(Xinit, Yinit, Player, Piece),
    !,
    repeat,
    read_coords(Xdest, Ydest, Player, Piece, move),
    (cancel(Xdest, Ydest);
    (castling_move(Player, Piece, Xdest, Ydest, Xinit, Yinit));
    (valid_cell(Ydest, Xdest),
    valid_move(Piece, Xinit, Yinit, Xdest, Ydest, Player),
    change_database(Xdest, Ydest, Player, Piece),
    ((check_virtual_limits,
      check_connections); (change_database(Xinit, Yinit, Player, Piece), false)))),!,
    \+ cancel(Xdest, Ydest).

move_ai(Player, Piece, Xdest, Ydest) :-
    cell(Xinit, Yinit, Player, Piece),!,
    (castling_move(Player, Piece, Xdest, Ydest, Xinit, Yinit);
     change_database(Xdest, Ydest, Player, Piece)).

check_movement(Player, Piece, Xdest, Ydest):-
    cell(Xinit, Yinit, Player, Piece),!,
    (check_castling(Player, Piece, Xdest, Ydest);
    (valid_cell(Ydest, Xdest),
    valid_move(Piece, Xinit, Yinit, Xdest, Ydest, Player),
    change_database(Xdest, Ydest, Player, Piece),!,
    ((check_virtual_limits, check_connections, change_database(Xinit, Yinit, Player, Piece)); \+ change_database(Xinit, Yinit, Player, Piece)))),!.

check_castling(Player, rook, X, Y) :-
    castling_available(Player),
    cell(X, Y, Player, king).

castling_move(Player, rook, X, Y, RookX, RookY) :-
    castling_available(Player),!,        % Checks if the player has not used castling already
    cell(X, Y, Player, king),
    change_database(RookX, RookY, Player, king),
    change_database(X, Y, Player, rook),
    castling_done(Player).


special_power_on_placement(pawn, Player) :-
    cell(Xinit, Yinit, Player, pawn),
    display_board,
    !,
    repeat,
    change_database(Xinit, Yinit, Player, pawn),
    read_coords_no_cancel(Xdest, Ydest, Player, pawn, move),
    ((Xinit == Xdest, Yinit == Ydest);                      %This allows the user to choose not to move the pawn immediatly
    (valid_cell(Ydest, Xdest),
    valid_move(pawn, Xinit, Yinit, Xdest, Ydest, Player),
    change_database(Xdest, Ydest, Player, pawn),
    ((check_virtual_limits,
      check_connections); (change_database(Xinit, Yinit, Player, pawn), false)))).

special_power_on_placement(bishop, Player) :-
    only_kings_on_board(Player);
    display_board,
    (!,
    repeat,
        read_player_piece(X, Y),
        valid_removable_cell(X, Y, Player, TargetPlayer, Piece),
        delete_from_database(X, Y, TargetPlayer, Piece),
        (check_connections; (change_database(X, Y, TargetPlayer, Piece), false))).
 
special_power_on_placement(_, _).

special_power_on_placement_ai(pawn, Player, [Xdest, Ydest]) :-
    display_board,
    change_database(Xdest, Ydest, Player, pawn).

special_power_on_placement_ai(bishop, Player, [X, Y]) :-
    only_kings_on_board(Player);
    display_board,
    delete_from_database(X, Y, _, _).
 
special_power_on_placement_ai(_, _).

possible_removable(Player, TargetPlayer, Piece, X ,Y) :-
    valid_removable_cell(X, Y, Player, TargetPlayer, Piece),
    delete_from_database(X, Y, TargetPlayer, Piece),!,
    ((check_connections, change_database(X, Y, TargetPlayer, Piece)) ; \+change_database(X, Y, TargetPlayer, Piece)),!.

valid_removable_cell(X, Y, Player, TargetPlayer, Piece) :-
    cell(X, Y, TargetPlayer, Piece),
    Piece \== king,
    \+ (Piece == bishop, TargetPlayer == Player).

only_kings_on_board(Player) :-
    \+((cell(_,_,Color,Piece),
        \+(Piece == king; (Piece == bishop, Player == Color)))).

valid_cell(R, C) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits
    \+cell(C, R, _, _).                 % Checks if there is already a piece in that cell


check_connections :-
    cell(C, R, _, _),
    connected_board([[R, C]]),!,
    ((all_visited, clean_visited) ; (\+ clean_visited)).

connected_board([]).
connected_board([[] | Others]) :-
    connected_board(Others).
connected_board([[R, C] | Others]) :-
    ((check_visited(R, C), Next = Others) ; 
    (add_visited(R, C),
     PR is R - 1, NR is R + 1,
     PC is C - 1, NC is C + 1,
     !,
     (((cell(PC, PR, _, _), Cell1 = [PR, PC]) ; Cell1 = []),
      ((cell( C, PR, _, _), Cell2 = [PR,  C]) ; Cell2 = []),
      ((cell(NC, PR, _, _), Cell3 = [PR, NC]) ; Cell3 = []),
      ((cell(PC,  R, _, _), Cell4 = [ R, PC]) ; Cell4 = []),
      ((cell(NC,  R, _, _), Cell5 = [ R, NC]) ; Cell5 = []),
      ((cell(PC, NR, _, _), Cell6 = [NR, PC]) ; Cell6 = []),
      ((cell( C, NR, _, _), Cell7 = [NR,  C]) ; Cell7 = []),
      ((cell(NC, NR, _, _), Cell8 = [NR, NC]) ; Cell8 = [])),
     append(Others, [Cell1, Cell2, Cell3, Cell4, Cell5, Cell6, Cell7, Cell8], Next))),
    connected_board(Next).
    
all_visited :-
    \+((cell(C, R, _, _),
        \+check_visited(R, C))).

check_virtual_limits :-
    \+((cell(C, R, _, _),
       \+ within_virtual_limits(R, C))).

valid_move(pawn, Xinit, Yinit, Xdest, Ydest, _) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX))).

valid_move(king, Xinit, Yinit, Xdest, Ydest, _) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX));
    (Xdest = NextX, (Ydest = NextY; Ydest = PrevY));
    (Xdest = PrevX, (Ydest = NextY; Ydest = PrevY))).

valid_move(rook, Xinit, Yinit, Xdest, Ydest, Player) :-
    !,
    (Xinit = Xdest; Yinit = Ydest),!,
    straight_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player).

valid_move(bishop, Xinit, Yinit, Xdest, Ydest, Player) :-
    !,
    Xdest \= Xinit,
    Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1),!,
    diagonal_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player).

valid_move(queen, Xinit, Yinit, Xdest, Ydest, Player) :-
    !,
    (Xinit = Xdest; Yinit = Ydest;
    (Xdest \= Xinit, Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1))),!,
    diagonal_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player),!,
    straight_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player).

valid_move(knight, Xinit, Yinit, Xdest, Ydest, _) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    DoubleNextY is Yinit + 2, DoublePrevY is Yinit - 2,
    DoubleNextX is Xinit + 2, DoublePrevX is Xinit - 2,
    (((Xdest = DoubleNextX; Xdest = DoublePrevX), (Ydest = NextY; Ydest = PrevY));
    ((Ydest = DoubleNextY; Ydest = DoublePrevY), (Xdest = NextX; Xdest = PrevX))).
