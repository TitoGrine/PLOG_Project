% This module handles all restrictions to when you play/place a piece

place(Player, Piece) :-
    repeat,
    format('New Position for ~s: ', [Piece]),
    read_coords(X, Y),
    piece(Piece, Player, _).

side_enemy_king(R, C, Color) :-
    opposite(Color, Opposite),
    cell(KingR, KingC, Opposite, king),
    (R = KingR, (C = KingC + 1; C = KingC - 1));
    (C = KingC, (R = KingR + 1; R = KingR - 1));.

near_same_color(R, C, Color) :-
    cell(R+1, C, Color, _);
    cell(R-1, C, Color, _);
    cell(R, C+1, Color, _);
    cell(R, C-1, Color, _);
    cell(R+1, C+1, Color, _);
    cell(R+1, C-1, Color, _);
    cell(R-1, C+1, Color, _);
    cell(R-1, C-1, Color, _).