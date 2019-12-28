:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').
:- include('puzzleDatabase.pl').
:- include('adjancy.pl').

% Is 1
% Applies the 1 restriction to a tile.
is_1(Tile) :- Tile #= 1.

% Is not 1
% Restricts a tile to be different than 1.
is_not_1(Tile) :- Tile #\= 1.

% Is 2
% Applies the 2 restriction to a tile.
is_2(Tile) :- Tile #= 2.

% Is not 2
% Restricts a tile to be different than 2.
is_not_2(Tile) :- Tile #\= 2.

% Is 3
% Applies the 3 restriction to a tile.
is_3(Tile) :- Tile #= 3.

% Is not 3
% Restricts a tile to be different than 3.
is_not_3(Tile) :- Tile #\= 3.

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

% Get 3 - 1 - 3 fetchs 3 tiles where the first is at the right bottom corner of an L piece
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

% Get 3 - 2 - 1 fetchs 3 tiles where the first is at the left bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 2, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_2_1(Board, Size, R, C, Tiles, [[R, C], [PrevR, C], [PrevR, NextC]]) :-
    C < Size - 1,
    R > 0, !,
    PrevR is R - 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, PrevR, C, Tile2, _),
    get_1(Board, PrevR, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_2_1(_, Size, R, C, [], []) :-
    (C >= Size - 1; R =:= 0), !.

% Get 3 - 2 - 2 fetchs 3 tiles where the first is at the left top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 2, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_2_2(Board, Size, R, C, Tiles, [[R, C], [NextR, C], [R, NextC]]) :-
    C < Size - 1,
    R < Size - 1, !,
    NextR is R + 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, NextR, C, Tile2, _),
    get_1(Board, R, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_2_2(_, Size, R, C, [], []) :-
    (C >= Size - 1; R  >= Size - 1), !.

% Get 3 - 2 - 3 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 2, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_2_3(Board, Size, R, C, Tiles, [[R, C], [R, PrevC], [NextR, PrevC]]) :-
    C > 0,
    R < Size - 1, !,
    NextR is R + 1,
    PrevC is C - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, PrevC, Tile2, _),
    get_1(Board, NextR, PrevC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_2_3(_, Size, R, C, [], []) :-
    (C =:= 0; R  >= Size - 1), !.

% Get 3 - 3 - 1 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 3, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_3_1(Board, Size, R, C, Tiles, [[R, C], [NextR, C], [NextR, PrevC]]) :-
    C > 0,
    R < Size - 1, !,
    NextR is R + 1,
    PrevC is C - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, NextR, C, Tile2, _),
    get_1(Board, NextR, PrevC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_3_1(_, Size, R, C, [], []) :-
    (C =:= 0; R  >= Size - 1), !.

% Get 3 - 3 - 2 fetchs 3 tiles where the first is at the right bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 3, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_3_2(Board, _, R, C, Tiles, [[R, C], [PrevR, C], [R, PrevC]]) :-
    C > 0,
    R > 0, !,
    PrevR is R - 1,
    PrevC is C - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, PrevR, C, Tile2, _),
    get_1(Board, R, PrevC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_3_2(_, _, R, C, [], []) :-
    (C =:= 0; R  =:= 0), !.

% Get 3 - 3 - 3 fetchs 3 tiles where the first is at the left bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 3, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_3_3(Board, Size, R, C, Tiles, [[R, C], [R, NextC], [PrevR, NextC]]) :-
    C < Size - 1,
    R > 0, !,
    PrevR is R - 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, NextC, Tile2, _),
    get_1(Board, PrevR, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_3_3(_, Size, R, C, [], []) :-
    (C >= Size - 1; R  =:= 0), !.

% Get 3 - 4 - 1 fetchs 3 tiles where the first is at the right bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 4, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_4_1(Board, _, R, C, Tiles, [[R, C], [R, PrevC], [PrevR, PrevC]]) :-
    C > 0,
    R > 0, !,
    PrevR is R - 1,
    PrevC is C - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, PrevC, Tile2, _),
    get_1(Board, PrevR, PrevC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_4_1(_, _, R, C, [], []) :-
    (C =:= 0; R  =:= 0), !.

% Get 3 - 4 - 2 fetchs 3 tiles where the first is at the left bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 4, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_4_2(Board, Size, R, C, Tiles, [[R, C], [PrevR, C], [R, NextC]]) :-
    C < Size - 1,
    R > 0, !,
    PrevR is R - 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, PrevR, C, Tile2, _),
    get_1(Board, R, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_4_2(_, Size, R, C, [], []) :-
    (C >= Size - 1; R  =:= 0), !.

% Get 3 - 4 - 3 fetchs 3 tiles where the first is at the left top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 4, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_4_3(Board, Size, R, C, Tiles, [[R, C], [NextR, C], [NextR, NextC]]) :-
    C < Size - 1,
    R < Size - 1, !,
    NextR is R + 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, NextR, C, Tile2, _),
    get_1(Board, NextR, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_4_3(_, Size, R, C, [], []) :-
    (C >= Size - 1; R  >= Size - 1), !.

% Get 3 - 5 - 1 fetchs 3 tiles where the first is at the left corner of a horizontal piece
% Check piece_map.png for 3 tiles pieces, row 5, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_5_1(Board, Size, R, C, Tiles, [[R, C], [R, NextC], [R, NextNextC]]) :-
    C < Size - 2, !,
    NextC is C + 1,
    NextNextC is NextC + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, NextC, Tile2, _),
    get_1(Board, R, NextNextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_5_1(_, Size, _, C, [], []) :-
    C >= Size - 2.

% Get 3 - 5 - 2 fetchs 3 tiles where the first is in the middle tile of a horizontal piece
% Check piece_map.png for 3 tiles pieces, row 5, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_5_2(Board, Size, R, C, Tiles, [[R, C], [R, PrevC], [R, NextC]]) :-
    C < Size - 1,
    C > 0, !,
    PrevC is C - 1,
    NextC is C + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, PrevC, Tile2, _),
    get_1(Board, R, NextC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_5_2(_, Size, _, C, [], []) :-
    (C >= Size - 1; C =:= 0), !.

% Get 3 - 5 - 3 fetchs 3 tiles where the first is in the left corner of a horizontal piece
% Check piece_map.png for 3 tiles pieces, row 5, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_5_3(Board, _, R, C, Tiles, [[R, C], [R, PrevC], [R, PrevPrevC]]) :-
    C > 1, !,
    PrevC is C - 1,
    PrevPrevC is PrevC - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, R, PrevC, Tile2, _),
    get_1(Board, R, PrevPrevC, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_5_3(_, _, _, C, [], []) :-
    C =< 1.

% Get 3 - 6 - 1 fetchs 3 tiles where the first is in the top tile of a vertical piece
% Check piece_map.png for 3 tiles pieces, row 6, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_6_1(Board, Size, R, C, Tiles, [[R, C], [NextR, C], [NextNextR, C]]) :-
    R < Size - 2, !,
    NextR is R + 1,
    NextNextR is NextR + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, NextR, C, Tile2, _),
    get_1(Board, NextNextR, C, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_6_1(_, Size, R, _, [], []) :-
    R >= Size - 2.

% Get 3 - 6 - 2 fetchs 3 tiles where the first is in the middle tile of a vertical piece
% Check piece_map.png for 3 tiles pieces, row 6, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_6_2(Board, Size, R, C, Tiles, [[R, C], [PrevR, C], [NextR, C]]) :-
    R > 0,
    R < Size - 1, !,
    PrevR is R - 1,
    NextR is R + 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, PrevR, C, Tile2, _),
    get_1(Board, NextR, C, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_6_2(_, Size, R, _, [], []) :-
    (R >= Size - 1; R =:= 0), !.

% Get 3 - 6 - 3 fetchs 3 tiles where the first is in the bottom tile of a vertical piece
% Check piece_map.png for 3 tiles pieces, row 6, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_6_3(Board, _, R, C, Tiles, [[R, C], [PrevR, C], [PrevPrevR, C]]) :-
    R > 1, !,
    PrevR is R - 1,
    PrevPrevR is PrevR - 1,
    get_1(Board, R, C, Tile1, _),
    get_1(Board, PrevR, C, Tile2, _),
    get_1(Board, PrevPrevR, C, Tile3, _),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_6_3(_, _, R, _, [], []) :-
    R =< 1.

% Shape 1 applies the restriction to a 1 tile and its adjacent tiles
shape_1(Board, Size, Row, Column) :-
    get_1(Board, Row, Column, Tile, Coordinates),
    get_adjacent_numbers(Board, Size, Row, Column, Adjacent),
    Tile #= 1,
    Adjacent #\= 1.

% Shape 2 applies the restrictions to a 2 shape list of tiles
% and to its adjacent tiles.
shape_2(Board, Size, Tiles, Coordinates) :-
    Tiles #= 2,
    get_adjacent_numbers_of_list(Board, Size, Coordinates, Adjacent),
    filter_adjacents(Tiles, Adjacent, Filtered),
    Filtered #\= 2.

% Shape 3 applies the restrictions to a 3 shape list of tiles
% and to its adjacent tiles
shape_3(Board, Size, Tiles, Coordinates) :-
    Tiles #= 3,
    get_adjacent_numbers_of_list(Board, Size, Coordinates, Adjacent),
    filter_adjacents(Tiles, Adjacent, Filtered),
    Filtered #\= 3.

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
