can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = pawn, !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX))).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = king, !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX));
    (Xdest = NextX, (Ydest = NextY; Ydest = PrevY));
    (Xdest = PrevX, (Ydest = NextY; Ydest = PrevY))).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = rook, !,
    (Xinit = Xdest; Yinit = Ydest).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = bishop, !,
    Xdest \= Xinit,
    Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = queen, !,
    (Xinit = Xdest; Yinit = Ydest;
    (Xdest \= Xinit, Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1))).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = knight, !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    DoubleNextY is Yinit + 2, DoublePrevY is Yinit - 2,
    DoubleNextX is Xinit + 2, DoublePrevX is Xinit - 2,
    (((Xdest = DoubleNextX; Xdest = DoublePrevX), (Ydest = NextY; Ydest = PrevY));
    ((Ydest = DoubleNextY; Ydest = DoublePrevY), (Xdest = NextX; Xdest = PrevX))).

can_move(_, _, _, _, _) :-
    write('Invalid piece'), false.