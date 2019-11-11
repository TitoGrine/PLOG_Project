:- include('logic.pl').

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
    \+ cancel(Xdest, Ydest). % Porque é que isto está aqui?

castling_move(Player, rook, X, Y, RookX, RookY) :-
    castling_available(Player),         %Checking if the player has not used castling already
    cell(KingX, KingY, Player, king),
    KingX == X, KingY == Y, !,
    change_database(RookX, RookY, Player, king),
    change_database(KingX, KingY, Player, rook),
    castling_done(Player).


special_power_on_placement(pawn, Player) :-
    cell(Xinit, Yinit, Player, pawn),
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
    (!,
    repeat,
        read_player_piece(X, Y),
        valid_removable_cell(X, Y, Player, TargetPlayer, Piece),
        delete_from_database(X, Y, TargetPlayer, Piece),
        (check_connections; (change_database(X, Y, TargetPlayer, Piece), false))).
 
special_power_on_placement(_, _).

valid_removable_cell(X, Y, Player, TargetPlayer, Piece) :-
    cell(X, Y, TargetPlayer, Piece),
    Piece \== king,
    \+ (Piece == bishop, TargetPlayer == Player).

only_kings_on_board(Player) :-
    \+((cell(_,_,Color,Piece),
        \+(Piece == king; (Piece == bishop, Player == Color)))).

valid_cell(R, C) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits
    \+ cell(C, R, _, _),!.              % Checks if there is already a piece in that cell


check_connections :-
    \+((cell(C, R, _, _),
       \+ connected(R, C))),
    check_for_islands.

connected(R, C) :-
    PR is R - 1, NR is R + 1,
    PC is C - 1, NC is C + 1,
    !,
    (cell(NC, R, _, _);
     cell(PC, R, _, _);
     cell(C, NR, _, _);
     cell(C, PR, _, _);
     cell(NC, NR, _, _);
     cell(NC, PR, _, _);
     cell(PC, NR, _, _);
     cell(PC, PR, _, _)).

check_for_islands :-
    no_horizontal_islands(1),
    no_vertical_islands(1).

no_horizontal_islands(6) :-
    true.

no_horizontal_islands(Row) :-
    PRow is Row - 1, NRow is Row + 1,
    (\+((\+cell(_, Row, _, _),
        (cell(_, PRow, _, _), cell(_, NRow, _, _)))),
     no_horizontal_islands(NRow)).

no_vertical_islands(6) :-
    true.

no_vertical_islands(Collumn) :-
    PCollumn is Collumn - 1, NCollumn is Collumn + 1,
    (\+((\+cell(_, Collumn, _, _),
        (cell(_, PCollumn, _, _), cell(_, NCollumn, _, _)))),
     no_vertical_islands(NCollumn)).


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

valid_move(_, _, _, _, _, _) :-
    ansi_format([fg(green)],'Invalid move.', []), false.