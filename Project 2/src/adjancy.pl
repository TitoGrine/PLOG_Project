:- use_module(library(lists)).

get_number(Board, Size, R, C, Number) :-
    R >= 0, C >= 0, 
    R < Size, C < Size,
    nth0(R, Board, Row),
    nth0(C, Row, Number).

get_horizontal_adjacent_numbers(Board, Size, R, 0, [Adjacent]) :-
    R >= 0, R < Size, 
    nth0(R, Board, Row),
    nth0(1, Row, Adjacent).
get_horizontal_adjacent_numbers(Board, Size, R, C, [Adjacent]) :-
    Size =:= C + 1,
    R >= 0, R < Size,
    nth0(R, Board, Row),
    LeftPos is C - 1,
    nth0(LeftPos, Row, Adjacent).
get_horizontal_adjacent_numbers(Board, Size, R, C, [AdjacentLeft, AdjacentRight]) :-
    R >= 0, C >= 0, 
    R < Size, C < Size, 
    nth0(R, Board, Row),
    LeftPos is C - 1,
    RightPos is C + 1,
    nth0(LeftPos, Row, AdjacentLeft),
    nth0(RightPos, Row, AdjacentRight).
get_horizontal_adjacent_numbers(_, 1, 0, 0, []).

get_vertical_adjacent_numbers(Board, Size, 0, C, [Adjacent]) :-
    C >= 0, C < Size, 
    nth0(1, Board, Row),
    nth0(C, Row, Adjacent).
get_vertical_adjacent_numbers(Board, Size, R, C, [Adjacent]) :-
    Size =:= R + 1,
    C >= 0, C < Size,
    UpPos is R - 1,
    nth0(UpPos, Board, Row),
    nth0(C, Row, Adjacent).
get_vertical_adjacent_numbers(Board, Size, R, C, [AdjacentUp, AdjacentDown]) :-
    R >= 0, C >= 0, 
    R < Size, C < Size, 
    UpPos is R - 1,
    DownPos is R + 1,
    nth0(UpPos, Board, UpperRow),
    nth0(DownPos, Board, LowerRow),
    nth0(C, UpperRow, AdjacentUp),
    nth0(C, LowerRow, AdjacentDown).
get_vertical_adjacent_numbers(_, 1, 0, 0, []).

get_adjacent_numbers(Board, Size, Row, Column, AdjacentNumbers) :-
    get_horizontal_adjacent_numbers(Board, Size, Row, Column, HorizontalAdjacent),
    get_vertical_adjacent_numbers(Board, Size, Row, Column, VerticalAdjacent),
    append(HorizontalAdjacent, VerticalAdjacent, AdjacentNumbers).

% Following predicate colects all adjacent tiles of a list of tiles
% Note that it does not discard the ones already present in the list
get_adjacent_numbers_of_list(Board, Size, List, Adjacent) :-
    get_adjacent_numbers_of_list(Board, Size, List, [], AdjacentWithDuplicates),
    remove_dups(AdjacentWithDuplicates, Adjacent).

get_adjacent_numbers_of_list(_, _, [], Adjacent, Adjacent).

get_adjacent_numbers_of_list(Board, Size, [[Row, Column] | Rest], AdjacentTillNow, Adjacent) :-
    get_horizontal_adjacent_numbers(Board, Size, Row, Column, HorizontalAdjacent),
    get_vertical_adjacent_numbers(Board, Size, Row, Column, VerticalAdjacent),
    append(HorizontalAdjacent, AdjacentTillNow, TempAdj),
    append(VerticalAdjacent, TempAdj, NewAdjacentTillNow),
    get_adjacent_numbers_of_list(Board, Size, Rest, NewAdjacentTillNow, Adjacent).

get_horizontal_adjacent_coords(Size, R, 0, [Adjacent]) :-
    R >= 0, R < Size, 
    Adjacent = R-1.
get_horizontal_adjacent_coords(Size, R, C, [Adjacent]) :-
    Size =:= C + 1,
    R >= 0, R < Size,
    LeftPos is C - 1,
    Adjacent = R-LeftPos.
get_horizontal_adjacent_coords(Size, R, C, [AdjacentLeft, AdjacentRight]) :-
    R >= 0, C >= 1, 
    R < Size, C < Size - 1, 
    LeftPos is C - 1,
    RightPos is C + 1,
    AdjacentLeft = R-LeftPos,
    AdjacentRight = R-RightPos.
get_horizontal_adjacent_coords(1, 0, 0, []).

get_vertical_adjacent_coords(Size, 0, C, [Adjacent]) :-
    C >= 0, C < Size, 
    Adjacent = 1-C.
get_vertical_adjacent_coords(Size, R, C, [Adjacent]) :-
    Size =:= R + 1,
    C >= 0, C < Size,
    UpPos is R - 1,
    Adjacent = UpPos-C.
get_vertical_adjacent_coords(Size, R, C, [AdjacentUp, AdjacentDown]) :-
    R >= 1, C >= 0, 
    R < Size - 1, C < Size, 
    UpPos is R - 1,
    DownPos is R + 1,
    AdjacentUp = UpPos-C,
    AdjacentDown = DownPos-C.
get_vertical_adjacent_coords(1, 0, 0, []).

get_adjacent_coords(Size, Row, Column, Adjacent) :-
    get_horizontal_adjacent_coords(Size, Row, Column, HorizontalAdjacent),
    get_vertical_adjacent_coords(Size, Row, Column, VerticalAdjacent),
    append(HorizontalAdjacent, VerticalAdjacent, Adjacent).

% Get 2 - 1 Adjacents
% Fetchs the adjacent tiles for shape_2_1
get_2_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal1),
    get_horizontal_adjacent_numbers(Board, Size, NextR, C, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, PrevR, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 2 - 2 Adjacents
% Fetchs the adjacent tiles for shape_2_2
get_2_2_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal1),
    get_horizontal_adjacent_numbers(Board, Size, PrevR, C, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, Prev2R, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 2 - 3 Adjacents
% Fetchs the adjacent tiles for shape_2_3
get_2_3_adjacents(Board, Size, R, C, Adjacents) :-
    PrevC is C - 1,
    NextC is C + 1,
    Next2C is C + 2,
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical1),
    get_vertical_adjacent_numbers(Board, Size, R, NextC, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, R, PrevC, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next2C, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 2 - 4 Adjacents
% Fetchs the adjacent tiles for shape_2_4
get_2_4_adjacents(Board, Size, R, C, Adjacents) :-
    PrevC is C - 1,
    Prev2C is C - 2,
    NextC is C + 1,
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical1),
    get_vertical_adjacent_numbers(Board, Size, R, PrevC, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, R, Prev2C, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, NextC, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).
