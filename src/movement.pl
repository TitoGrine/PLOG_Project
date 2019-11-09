:- include('default_board.pl').
:- include('utils.pl').

move(Player, Piece) :-
    cell(Xinit, Yinit, Player, Piece),
    !,
    repeat,
    change_database(Xinit, Yinit, Player, Piece),
    read_coords(Xdest, Ydest, Piece),
    valid_cell(Ydest, Xdest),
    valid_move(Piece, Xinit, Yinit, Xdest, Ydest),
    change_database(Xdest, Ydest, Player, Piece),
    check_connections.

valid_cell(R, C) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits
    \+ cell(C, R, _, _),!.              % Checks if there is already a piece in that cell

check_connections :-
    \+((cell(C, R, _, _),
       \+ connected(R, C))).

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

valid_move(pawn, Xinit, Yinit, Xdest, Ydest) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX))).

valid_move(king, Xinit, Yinit, Xdest, Ydest) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX));
    (Xdest = NextX, (Ydest = NextY; Ydest = PrevY));
    (Xdest = PrevX, (Ydest = NextY; Ydest = PrevY))).

valid_move(rook, Xinit, Yinit, Xdest, Ydest) :-
    !,
    (Xinit = Xdest; Yinit = Ydest).

valid_move(bishop, Xinit, Yinit, Xdest, Ydest) :-
    !,
    Xdest \= Xinit,
    Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1).

valid_move(queen, Xinit, Yinit, Xdest, Ydest) :-
    !,
    (Xinit = Xdest; Yinit = Ydest;
    (Xdest \= Xinit, Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1))).

valid_move(knight, Xinit, Yinit, Xdest, Ydest) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    DoubleNextY is Yinit + 2, DoublePrevY is Yinit - 2,
    DoubleNextX is Xinit + 2, DoublePrevX is Xinit - 2,
    (((Xdest = DoubleNextX; Xdest = DoublePrevX), (Ydest = NextY; Ydest = PrevY));
    ((Ydest = DoubleNextY; Ydest = DoublePrevY), (Xdest = NextX; Xdest = PrevX))).

valid_move(_, _, _, _, _) :-
    write('Invalid move'), false.