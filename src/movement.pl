:- include('default_board.pl').
:- include('utils.pl').
:- include('logic.pl').

move(Player, Piece) :-
    cell(Xinit, Yinit, Player, Piece),
    !,
    repeat,
    change_database(Xinit, Yinit, Player, Piece),
    read_coords(Xdest, Ydest, Piece),
    (cancel(Xdest, Ydest); 
    valid_cell(Ydest, Xdest),
    valid_move(Piece, Xinit, Yinit, Xdest, Ydest, Player),
    change_database(Xdest, Ydest, Player, Piece),
    check_virtual_limits,
    check_connections),!,
    \+ cancel(Xdest, Ydest).

special_pawn(Player) :-
    ansi_format([fg(green)],'Pawn can move immediatly after being played.', []), nl,
    cell(Xinit, Yinit, Player, pawn),
    !,
    repeat,
        change_database(Xinit, Yinit, Player, pawn),
        read_coords_no_cancel(Xdest, Ydest, pawn),
        valid_cell(Ydest, Xdest),
        valid_move(pawn, Xinit, Yinit, Xdest, Ydest, Player),
        change_database(Xdest, Ydest, Player, pawn),
        check_virtual_limits,
        check_connections,
    !.


valid_cell(R, C) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits
    \+ cell(C, R, _, _),!.              % Checks if there is already a piece in that cell

% TODO: change to Flood Fill Algorithm
check_connections :-
    \+((cell(C, R, _, _),
       \+ connected(R, C))).

check_virtual_limits :-
    \+((cell(C, R, _, _),
       \+ within_virtual_limits(R, C))).

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