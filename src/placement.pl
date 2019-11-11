:- include('logic.pl').

% This module handles all restrictions to when you play/place a piece
place(Player, queen) :-
    \+ cell(Xinit, Yinit, Player, queen),!,
    dead(Player, queen, false),
    repeat,
    read_coords(X, Y, Player, queen, place),
    (cancel(X, Y);
     (valid_cell(Y, X, Player),
      add_to_database(X, Y, Player, queen))),!,
    \+ cancel(X, Y).

place(Player, Piece) :-
    \+ cell(Xinit, Yinit, Player, Piece),
    repeat,
    read_coords(X, Y, Player, Piece, place),
    (cancel(X, Y);
     (valid_cell(Y, X, Player),
      add_to_database(X, Y, Player, Piece))),!,
    \+ cancel(X, Y).


valid_cell(R, C, Player) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits
    within_virtual_limits(R, C),!,      % Ensures that, if there is a virtual limit, the new position doesn't break it
    \+ cell(C, R, _, _),!,              % Checks if there is already a piece in that cell
    \+ side_enemy_king(R, C, Player),!, % Ensures it is not adjacent to the enemy king
    near_same_color(R, C, Player).      % Ensures that it is connected to a piece of the same color

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