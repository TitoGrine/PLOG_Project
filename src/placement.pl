% This module handles all restrictions to when you play/place a piece

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