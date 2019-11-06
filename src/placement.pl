:- include('utils.pl').
:- include('default_board.pl').

% This module handles all restrictions to when you play/place a piece

place(Player, Piece) :-
    repeat,
    format('New Position for ~s: ', [Piece]),
    read_coords(X, Y),
    \+side_enemy_king(Y, X, Player), %Not working
    near_same_color(Y, X, Player),  %Not working
    add_to_database(X, Y, Player, Piece). 

side_enemy_king(R, C, Color) :-
    opposite(Color, Opposite),
    cell(KingR, KingC, Opposite, king),
    !,
    (R =:= KingR, (C =:= KingC + 1; C =:= KingC - 1));
    (C =:= KingC, (R =:= KingR + 1; R =:= KingR - 1)).

near_same_color(R, C, Color) :-
    PR is R - 1, NR is R + 1,
    PC is C - 1, NC is C + 1,
    cell(NR, C, Color, _);
    cell(PR, C, Color, _);
    cell(R, NC, Color, _);
    cell(R, PC, Color, _);
    cell(NR, NC, Color, _);
    cell(NR, PC, Color, _);
    cell(PR, NC, Color, _);
    cell(PR, PC, Color, _).