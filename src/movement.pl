can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = pawn,
    ((Xinit = Xdest, (Yinit = Ydest + 1; Yinit = Ydest - 1));
    (Yinit = Ydest, (Xinit = Xdest + 1; Xinit = Xdest - 1))).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = king,
    (Xinit = Xdest, (Yinit = Ydest + 1; Yinit = Ydest - 1));
    (Yinit = Ydest, (Xinit = Xdest + 1; Xinit = Xdest - 1));
    (Xinit = Xdest + 1, (Yinit = Ydest + 1; Yinit = Ydest - 1));
    (Xinit = Xdest - 1, (Yinit = Ydest + 1; Yinit = Ydest - 1)).

can_move(Piece, Xinit, Yinit, Xdest, Ydest) :-
    Piece = queen,
    (Xinit = Xdest ; Yinit = Ydest).

can_move(_, _, _, _, _) :-
    write('Invalid piece'), false.