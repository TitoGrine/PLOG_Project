:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
%:- use_module(library(random)).
%:- use_module(library(system)).
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

% Get 2 - 1 fetchs 2 tiles where the first is on top of the other.
% If it is impossible to fetch such tiles it returns an empty list.
get_2_1(Board, Size, R, C, Tiles) :-
    R < Size - 1, !,
    NextR is R + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, NextR, C, Tile2),
    append([Tile1], [Tile2], Tiles).

get_2_1(_, Size, R, _, []) :-
    R >= Size - 1.

% Get 2 - 2 fetchs 2 tiles where the first is below the other
% If it is impossible to fetch such tiles it returns an empty list.
get_2_2(Board, Size, R, C, Tiles) :-
    R > 0, !,
    PrevR is R - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, C, Tile2),
    append([Tile1], [Tile2], Tiles).

get_2_2(_, _, R, _, []) :-
    R =:= 0.

% Get 2 - 3 fetchs 2 tiles where the first is at the left of the other
% If it is impossible to fetch such tiles it returns an empty list.
get_2_3(Board, Size, R, C, Tiles) :-
    C < Size - 1, !,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, NextC, Tile2),
    append([Tile1], [Tile2], Tiles).

get_2_3(_, Size, _, C, []) :-
    C >= Size - 1.

% Get 2 - 4 fetchs 2 tiles where the first is at the right of the other
% If it is impossible to fetch such tiles it returns an empty list.
get_2_4(Board, Size, R, C, Tiles) :-
    C > 0, !,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, PrevC, Tile2),
    append([Tile1], [Tile2], Tiles).

get_2_4(_, _, _, C, []) :-
    C =:= 0.

% Get 3 - 1 - 1 fetchs 3 tiles where the first is at the left top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 1, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_1_1(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    R < Size - 1, !,
    NextR is R + 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, NextC, Tile2),
    get_number(Board, Size, NextR, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_1_1(_, Size, R, C, []) :-
    (C >= Size - 1; R >= Size - 1), !.

% Get 3 - 1 - 2 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 1, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_1_2(Board, Size, R, C, Tiles) :-
    C > 0,
    R < Size - 1, !,
    NextR is R + 1,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, PrevC, Tile2),
    get_number(Board, Size, NextR, C, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_1_2(_, Size, R, C, []) :-
    (C =:= 0; R >= Size - 1), !.

% Get 3 - 1 - 3 fetchs 3 tiles where the first is at the right bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 1, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_1_3(Board, Size, R, C, Tiles) :-
    C > 0,
    R > 0, !,
    PrevR is R - 1,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, PrevC, Tile2),
    get_number(Board, Size, PrevR, C, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_1_3(_, _, R, C, []) :-
    (C =:= 0; R =:= 0), !.

% Get 3 - 2 - 1 fetchs 3 tiles where the first is at the left bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 2, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_2_1(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    R > 0, !,
    PrevR is R - 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, C, Tile2),
    get_number(Board, Size, PrevR, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_2_1(_, Size, R, C, []) :-
    (C >= Size - 1; R =:= 0), !.

% Get 3 - 2 - 2 fetchs 3 tiles where the first is at the left top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 2, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_2_2(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    R < Size - 1, !,
    NextR is R + 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, NextR, C, Tile2),
    get_number(Board, Size, R, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_2_2(_, Size, R, C, []) :-
    (C >= Size - 1; R  >= Size - 1), !.

% Get 3 - 2 - 3 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 2, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_2_3(Board, Size, R, C, Tiles) :-
    C > 0,
    R < Size - 1, !,
    NextR is R + 1,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, PrevC, Tile2),
    get_number(Board, Size, NextR, PrevC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_2_3(_, Size, R, C, []) :-
    (C =:= 0; R  >= Size - 1), !.

% Get 3 - 3 - 1 fetchs 3 tiles where the first is at the right top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 3, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_3_1(Board, Size, R, C, Tiles) :-
    C > 0,
    R < Size - 1, !,
    NextR is R + 1,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, NextR, C, Tile2),
    get_number(Board, Size, NextR, PrevC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_3_1(_, Size, R, C, []) :-
    (C =:= 0; R  >= Size - 1), !.

% Get 3 - 3 - 2 fetchs 3 tiles where the first is at the right bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 3, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_3_2(Board, Size, R, C, Tiles) :-
    C > 0,
    R > 0, !,
    PrevR is R - 1,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, C, Tile2),
    get_number(Board, Size, R, PrevC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_3_2(_, _, R, C, []) :-
    (C =:= 0; R  =:= 0), !.

% Get 3 - 3 - 3 fetchs 3 tiles where the first is at the left bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 3, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_3_3(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    R > 0, !,
    PrevR is R - 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, NextC, Tile2),
    get_number(Board, Size, PrevR, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_3_3(_, Size, R, C, []) :-
    (C >= Size - 1; R  =:= 0), !.

% Get 3 - 4 - 1 fetchs 3 tiles where the first is at the right bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 4, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_4_1(Board, Size, R, C, Tiles) :-
    C > 0,
    R > 0, !,
    PrevR is R - 1,
    PrevC is C - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, PrevC, Tile2),
    get_number(Board, Size, PrevR, PrevC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_4_1(_, _, R, C, []) :-
    (C =:= 0; R  =:= 0), !.

% Get 3 - 4 - 2 fetchs 3 tiles where the first is at the left bottom corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 4, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_4_2(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    R > 0, !,
    PrevR is R - 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, C, Tile2),
    get_number(Board, Size, R, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_4_2(_, Size, R, C, []) :-
    (C >= Size - 1; R  =:= 0), !.

% Get 3 - 4 - 3 fetchs 3 tiles where the first is at the left top corner of an L piece
% Check piece_map.png for 3 tiles pieces, row 4, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_4_3(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    R < Size - 1, !,
    NextR is R + 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, NextR, C, Tile2),
    get_number(Board, Size, NextR, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_4_3(_, Size, R, C, []) :-
    (C >= Size - 1; R  >= Size - 1), !.

% Get 3 - 5 - 1 fetchs 3 tiles where the first is at the left corner of a horizontal piece
% Check piece_map.png for 3 tiles pieces, row 5, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_5_1(Board, Size, R, C, Tiles) :-
    C < Size - 2, !,
    NextC is C + 1,
    NextNextC is NextC + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, NextC, Tile2),
    get_number(Board, Size, R, NextNextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_5_1(_, Size, _, C, []) :-
    C >= Size - 2.

% Get 3 - 5 - 2 fetchs 3 tiles where the first is in the middle tile of a horizontal piece
% Check piece_map.png for 3 tiles pieces, row 5, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_5_2(Board, Size, R, C, Tiles) :-
    C < Size - 1,
    C > 0, !,
    PrevC is C - 1,
    NextC is C + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, PrevC, Tile2),
    get_number(Board, Size, R, NextC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_5_2(_, Size, _, C, []) :-
    (C >= Size - 1; C =:= 0), !.

% Get 3 - 5 - 3 fetchs 3 tiles where the first is in the left corner of a horizontal piece
% Check piece_map.png for 3 tiles pieces, row 5, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_5_3(Board, Size, R, C, Tiles) :-
    C > 1, !,
    PrevC is C - 1,
    PrevPrevC is PrevC - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, R, PrevC, Tile2),
    get_number(Board, Size, R, PrevPrevC, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_5_3(_, _, _, C, []) :-
    C =< 1.

% Get 3 - 6 - 1 fetchs 3 tiles where the first is in the top tile of a vertical piece
% Check piece_map.png for 3 tiles pieces, row 6, shape 1
% If it is impossible to fetch such tiles it returns an empty list.
get_3_6_1(Board, Size, R, C, Tiles) :-
    R < Size - 2, !,
    NextR is R + 1,
    NextNextR is NextR + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, NextR, C, Tile2),
    get_number(Board, Size, NextNextR, C, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_6_1(_, Size, R, _, []) :-
    R >= Size - 2.

% Get 3 - 6 - 2 fetchs 3 tiles where the first is in the middle tile of a vertical piece
% Check piece_map.png for 3 tiles pieces, row 6, shape 2
% If it is impossible to fetch such tiles it returns an empty list.
get_3_6_2(Board, Size, R, C, Tiles) :-
    R > 0,
    R < Size - 1, !,
    PrevR is R - 1,
    NextR is R + 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, C, Tile2),
    get_number(Board, Size, NextR, C, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_6_2(_, Size, R, _, []) :-
    (R >= Size - 1; R =:= 0), !.

% Get 3 - 6 - 3 fetchs 3 tiles where the first is in the bottom tile of a vertical piece
% Check piece_map.png for 3 tiles pieces, row 6, shape 3
% If it is impossible to fetch such tiles it returns an empty list.
get_3_6_3(Board, Size, R, C, Tiles) :-
    R > 1, !,
    PrevR is R - 1,
    PrevPrevR is PrevR - 1,
    get_number(Board, Size, R, C, Tile1),
    get_number(Board, Size, PrevR, C, Tile2),
    get_number(Board, Size, PrevPrevR, C, Tile3),
    append([Tile1], [Tile2], Temp),
    append(Temp, [Tile3], Tiles).

get_3_6_3(_, _, R, _, []) :-
    R =< 1.

% Shape 1 applies the restriction to a 1 tile and its adjacent tiles
shape_1(Board, Size, Row, Column) :-
    get_number(Board, Size, Row, Column, Tile),
    get_adjacent_numbers(Board, Size, Row, Column, Adjacents),
    Tile #= 1,
    maplist(is_not_1, Adjacents).

% Shape 2 applies the restrictions to a 2 shape list of tiles
% and to its adjacent tiles.
shape_2(Tiles, Adjacents) :-
    maplist(is_2, Tiles),
    maplist(is_not_2, Adjacents).

% Shape 2 - 1
% Gets a 2 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_1(Board, Size, Row, Column) :-
    get_2_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_2_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_2(Tiles, Adjacents).

% Shape 2 - 2
% Gets a 2 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_2(Board, Size, Row, Column) :-
    get_2_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_2_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_2(Tiles, Adjacents).

% Shape 2 - 3
% Gets a 2 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_3(Board, Size, Row, Column) :-
    get_2_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_2_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_2(Tiles, Adjacents).

% Shape 2 - 4
% Gets a 2 - 4 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_2_4(Board, Size, Row, Column) :-
    get_2_4(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_2_4_adjacents(Board, Size, Row, Column, Adjacents),
    shape_2(Tiles, Adjacents).

% Shape 3 applies the restrictions to a 3 shape list of tiles
% and to its adjacent tiles
shape_3(Tiles, Adjacents) :-
    maplist(is_3, Tiles),
    maplist(is_not_3, Adjacents).

% Shape 3 - 1 - 1
% Gets a 3 - 1 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_1_1(Board, Size, Row, Column) :-
    get_3_1_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_1_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 1 - 2
% Gets a 3 - 1 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_1_2(Board, Size, Row, Column) :-
    get_3_1_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_1_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 1 - 3
% Gets a 3 - 1 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_1_3(Board, Size, Row, Column) :-
    get_3_1_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_1_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 2 - 1
% Gets a 3 - 2 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_2_1(Board, Size, Row, Column) :-
    get_3_2_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_2_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 2 - 2
% Gets a 3 - 2 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_2_2(Board, Size, Row, Column) :-
    get_3_2_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_2_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 2 - 3
% Gets a 3 - 2 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_2_3(Board, Size, Row, Column) :-
    get_3_2_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_2_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 3 - 1
% Gets a 3 - 3 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_3_1(Board, Size, Row, Column) :-
    get_3_3_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_3_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 3 - 2
% Gets a 3 - 3 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_3_2(Board, Size, Row, Column) :-
    get_3_3_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_3_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 3 - 3
% Gets a 3 - 3 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_3_3(Board, Size, Row, Column) :-
    get_3_3_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_3_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 4 - 1
% Gets a 3 - 4 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_4_1(Board, Size, Row, Column) :-
    get_3_4_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_4_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 4 - 2
% Gets a 3 - 4 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_4_2(Board, Size, Row, Column) :-
    get_3_4_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_4_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 4 - 3
% Gets a 3 - 4 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_4_3(Board, Size, Row, Column) :-
    get_3_4_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_4_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 5 - 1
% Gets a 3 - 5 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_5_1(Board, Size, Row, Column) :-
    get_3_5_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_5_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 5 - 2
% Gets a 3 - 5 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_5_2(Board, Size, Row, Column) :-
    get_3_5_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_5_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 5 - 3
% Gets a 3 - 5 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_5_3(Board, Size, Row, Column) :-
    get_3_5_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_5_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 6 - 1
% Gets a 3 - 6 - 1 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_6_1(Board, Size, Row, Column) :-
    get_3_6_1(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_6_1_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 6 - 2
% Gets a 3 - 6 - 2 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_6_2(Board, Size, Row, Column) :-
    get_3_6_2(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_6_2_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Shape 3 - 6 - 3
% Gets a 3 - 6 - 3 shape and applies restrictions.
% If it is not possible to use this shape, the predicate fails and restricts nothing
shape_3_6_3(Board, Size, Row, Column) :-
    get_3_6_3(Board, Size, Row, Column, Tiles),
    Tiles \= [], !,
    get_3_6_3_adjacents(Board, Size, Row, Column, Adjacents),
    shape_3(Tiles, Adjacents).

% Apply Constraint Geost
% Applies every piece constraint as a disjunction.
% Any piece that is valid (either in shape or by constraint placement)
% is a possible restriciton for the variables.
% When a piece is not valid, the shape predicates return false,
% which is neutral in a disjunction.
apply_constraint_geost(Board, Size, Row-Column) :-
    (shape_1(Board, Size, Row, Column) ; true), !,
    (shape_2_1(Board, Size, Row, Column) ; true), !,
    (shape_2_2(Board, Size, Row, Column) ; true), !,
    (shape_2_3(Board, Size, Row, Column) ; true), !,
    (shape_2_4(Board, Size, Row, Column) ; true), !,
    (shape_3_1_1(Board, Size, Row, Column) ; true), !,
    (shape_3_1_2(Board, Size, Row, Column) ; true), !,
    (shape_3_1_3(Board, Size, Row, Column) ; true), !,
    (shape_3_2_1(Board, Size, Row, Column) ; true), !,
    (shape_3_2_2(Board, Size, Row, Column) ; true), !,
    (shape_3_2_3(Board, Size, Row, Column) ; true), !,
    (shape_3_3_1(Board, Size, Row, Column) ; true), !,
    (shape_3_3_2(Board, Size, Row, Column) ; true), !,
    (shape_3_3_3(Board, Size, Row, Column) ; true), !,
    (shape_3_4_1(Board, Size, Row, Column) ; true), !,
    (shape_3_4_2(Board, Size, Row, Column) ; true), !,
    (shape_3_4_3(Board, Size, Row, Column) ; true), !,
    (shape_3_5_1(Board, Size, Row, Column) ; true), !,
    (shape_3_5_2(Board, Size, Row, Column) ; true), !,
    (shape_3_5_3(Board, Size, Row, Column) ; true), !,
    (shape_3_6_1(Board, Size, Row, Column) ; true), !,
    (shape_3_6_2(Board, Size, Row, Column) ; true), !,
    (shape_3_6_3(Board, Size, Row, Column) ; true).

% Solves the 123 puzzle using the geost-like approach
solve_puzzle_geost(Board) :-
    length(Board, Size),
    Max is Size - 1,
    append(Board, FlatBoard),

    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint_geost(Board, Size), Positions),

    write('.'),
    statistics(walltime, [Start,_]),
    labeling([], FlatBoard),
    statistics(walltime, [End,_]),
	Time is End - Start,
    
    display_board(Board, Size),
    format(' > Duration: ~3d s~n~n', [Time]),
    format(' > Statistics: ~n~n', []), fd_statistics, nl.