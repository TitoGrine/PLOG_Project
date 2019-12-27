:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').
:- include('puzzleDatabase.pl').
:- include('adjancy.pl').

% Get 1 fetchs a tile from the board
get_1(Board, R, C, Tile, [[R, C]]) :-
    nth0(R, Board, Row),
    nth0(C, Row, Tile).

% Get 2 - 1 fetchs 2 tiles where the first is on top of the other.
% If it is impossible to fetch such tiles it returns an empty list.
get_2_1(Board, Size, R, C, Tiles, [[R, C], [NextR, C]]) :-
    R < Size - 1, !,
    get_1(Board, R, C, Tile1, _),
    NextR is R + 1,
    get_1(Board, NextR, C, Tile2, _),
    append([Tile1], [Tile2], Tiles).

get_2_1(_, Size, R, _, [], []) :-
    R >= Size - 1.

% Get 2 - 2 fetchs 2 tiles where the first is below the other
% If it is impossible to fetch such tiles it returns an empty list.
get_2_2(Board, _, R, C, Tiles, [[R, C], [PrevR, C]]) :-
    R > 0, !,
    get_1(Board, R, C, Tile1, _),
    PrevR is R - 1,
    get_1(Board, PrevR, C, Tile2, _),
    append([Tile1], [Tile2], Tiles).

get_2_2(_, _, R, _, [], []) :-
    R =:= 0.

% Get 2 - 3 fetchs 2 tiles where the first is at the left of the other
% If it is impossible to fetch such tiles it returns an empty list.
get_2_3(Board, Size, R, C, Tiles, [[R, C], [R, NextC]]) :-
    C < Size - 1, !,
    get_1(Board, R, C, Tile1, _),
    NextC is C + 1,
    get_1(Board, R, NextC, Tile2, _),
    append([Tile1], [Tile2], Tiles).

get_2_3(_, Size, _, C, [], []) :-
    C >= Size - 1.

% Get 2 - 4 fetchs 2 tiles where the first is at the right of the other
% If it is impossible to fetch such tiles it returns an empty list.
get_2_4(Board, _, R, C, Tiles, [[R, C], [R, PrevC]]) :-
    C > 0, !,
    get_1(Board, R, C, Tile1, _),
    PrevC is C - 1,
    get_1(Board, R, PrevC, Tile2, _),
    append([Tile1], [Tile2], Tiles).

get_2_4(_, _, _, C, [], []) :-
    C =:= 0.

% Get 3 - 1 - 1 fetchs 3 tiles where the first is at the left top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 1, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_1_1(Board, Size, R, C, Tiles, [[R, C], [R, NextC], [NextR, NextC]]) :-
    C < Size - 1,
    R < Size - 1, !,
    NextR is R + 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, NextC, Tile2, _),
    get_1(Board, NextR, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_1_1(_, Size, R, C, [], []) :-
    (C >= Size - 1; R >= Size - 1), !.

% Get 3 - 1 - 2 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 1, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_1_2(Board, Size, R, C, Tiles, [[R, C], [R, PrevC], [NextR, C]]) :-
    C > 0,
    R < Size - 1, !,
    NextR is R + 1,
    PrevC is C - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, PrevC, Tile2, _),
    get_1(Board, NextR, C, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_1_2(_, Size, R, C, [], []) :-
    (C =:= 0; R >= Size - 1), !.

% Get 3 - 1 - 3 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 1, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_1_3(Board, _, R, C, Tiles, [[R, C], [PrevR, PrevC], [PrevR, C]]) :-
    C > 0,
    R > 0, !,
    PrevR is R - 1,
    PrevC is C - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, PrevR, PrevC, Tile2, _),
    get_1(Board, PrevR, C, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_1_3(_, _, R, C, [], []) :-
    (C =:= 0; R =:= 0), !.

% Shape 1 applies the restriction to a 1 tile and its adjacent tiles
shape_1(Board, Size, Row, Column) :-
    get_1(Board, Row, Column, Tile, Coordinates),
    Tile #= 1,
    get_adjacent_numbers_of_list(Board, Size, Coordinates, Adjacent),
    Adjacent #\= 1.

% Shape 2 applies the restrictions to a 2 shape list of tiles
% and to its adjacent tiles.
shape_2(Board, Size, Tiles, Coordinates) :-
    Tiles #= 2,
    get_adjacent_numbers_of_list(Board, Size, Coordinates, Adjacent),
    filter_adjacents(Tiles, Adjacent, Filtered),
    Filtered #\= 2.

% Shape 2 - 1
% Gets a 2 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_1(Board, Size, Row, Column) :-
    get_2_1(Board, Size, Row, Column, Tiles, Coordinates),
    Tiles \= [], !,
    shape_2(Board, Size, Tiles, Coordinates).

% Shape 2 - 2
% Gets a 2 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_2(Board, Size, Row, Column) :-
    get_2_2(Board, Size, Row, Column, Tiles, Coordinates),
    Tiles \= [], !,
    shape_2(Board, Size, Tiles, Coordinates).

% Shape 2 - 3
% Gets a 2 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_3(Board, Size, Row, Column) :-
    get_2_3(Board, Size, Row, Column, Tiles, Coordinates),
    Tiles \= [], !,
    shape_2(Board, Size, Tiles, Coordinates).

% Shape 2 - 4
% Gets a 2 - 4 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_4(Board, Size, Row, Column) :-
    get_2_4(Board, Size, Row, Column, Tiles, Coordinates),
    Tiles \= [], !,
    shape_2(Board, Size, Tiles, Coordinates).
