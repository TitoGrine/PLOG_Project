:- include('utils.pl').
:- include('default_board.pl').

% This module handles all restrictions to when you play/place a piece

place(Player, Piece) :-
    repeat,
    read_coords(X, Y, Piece),
    valid_cell(Y, X, Player),
    add_to_database(X, Y, Player, Piece).

valid_cell(R, C, Player) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits
    \+ cell(C, R, _, _),!,              % Checks if there is already a piece in that cell
    \+ side_enemy_king(R, C, Player),!, % Ensures it is not adjacent to the enemy king
    near_same_color(R, C, Player).      % Ensures that it is connected to a piece of the same color

within_limits(R, C) :-
    (R >= 0, R =< 5),
    (C >= 0, C =< 5).

side_enemy_king(R, C, Color) :-
    opposite(Color, Opposite),
    cell(KingC, KingR, Opposite, king),
    PKR is KingR - 1, NKR is KingR + 1,
    PKC is KingC - 1, NKC is KingC + 1,
    !,
    ((R =:= KingR, (C =:= PKC; C =:= NKC));
     (C =:= KingC, (R =:= PKR; R =:= NKR))).

near_same_color(R, C, Color) :-
    PR is R - 1, NR is R + 1,
    PC is C - 1, NC is C + 1,
    !,
    (cell(NC, R, Color, _);
    cell(PC, R, Color, _);
    cell(C, NR, Color, _);
    cell(C, PR, Color, _);
    cell(NC, NR, Color, _);
    cell(NC, PR, Color, _);
    cell(PC, NR, Color, _);
    cell(PC, PR, Color, _)).