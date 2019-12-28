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

% Get 3 - 1 - 1 Adjacents
% Fetchs the adjacent tiles for shape_3_1_1
get_3_1_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    PrevC is C - 1, 
    NextC is C + 1,
    Next2C is C + 2,
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical1),
    ((get_number(Board, Size, PrevR, NextC, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, NextC, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, R, PrevC, Tile3), TileLeft = [Tile3]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next2C, Tile4), TileRight = [Tile4]) ; TileRight = []), !,
    ((get_number(Board, Size, NextR, Next2C, Tile5), TileRightBot = [Tile5]) ; TileRightBot = []), !,
    append(TileLeft, TileRight, Horizontal1),
    append(Horizontal1, TileRightBot, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 1 - 2 Adjacents
% Fetchs the adjacent tiles for shape_3_1_2
get_3_1_2_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    PrevC is C - 1, 
    Prev2C is C - 2,
    NextC is C + 1,
    get_vertical_adjacent_numbers(Board, Size, R, PrevC, Vertical1),
    ((get_number(Board, Size, PrevR, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, R, Prev2C, Tile3), TileLeft = [Tile3]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, NextC, Tile4), TileRight = [Tile4]) ; TileRight = []), !,
    ((get_number(Board, Size, NextR, NextC, Tile5), TileRightBot = [Tile5]) ; TileRightBot = []), !,
    append(TileLeft, TileRight, Horizontal1),
    append(Horizontal1, TileRightBot, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 1 - 3 Adjacents
% Fetchs the adjacent tiles for shape_3_1_3
get_3_1_3_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    PrevC is C - 1, 
    Prev2C is C - 2,
    NextC is C + 1,
    get_vertical_adjacent_numbers(Board, Size, PrevR, PrevC, Vertical1),
    ((get_number(Board, Size, Prev2R, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, PrevR, Prev2C, Tile3), TileLeft = [Tile3]) ; TileLeft = []), !,
    ((get_number(Board, Size, PrevR, NextC, Tile4), TileRight = [Tile4]) ; TileRight = []), !,
    ((get_number(Board, Size, R, NextC, Tile5), TileRightBot = [Tile5]) ; TileRightBot = []), !,
    append(TileLeft, TileRight, Horizontal1),
    append(Horizontal1, TileRightBot, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 2 - 1 Adjacents
% Fetchs the adjacent tiles for shape_3_2_1
get_3_2_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    PrevC is C - 1, 
    NextC is C + 1,
    Next2C is C + 2,
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal1),
    ((get_number(Board, Size, PrevR, PrevC, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, PrevR, Next2C, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, Prev2R, C, Tile3), TileTop = [Tile3]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, C, Tile4), TileBot = [Tile4]) ; TileBot = []), !,
    ((get_number(Board, Size, Prev2R, NextC, Tile5), TileRightTop = [Tile5]) ; TileRightTop = []), !,
    append(TileTop, TileBot, Vertical1),
    append(Vertical1, TileRightTop, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 2 - 2 Adjacents
% Fetchs the adjacent tiles for shape_3_2_2
get_3_2_2_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    PrevC is C - 1, 
    NextC is C + 1,
    Next2C is C + 2,
    get_horizontal_adjacent_numbers(Board, Size, NextR, C, Horizontal1),
    ((get_number(Board, Size, R, PrevC, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next2C, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, PrevR, C, Tile3), TileTop = [Tile3]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, C, Tile4), TileBot = [Tile4]) ; TileBot = []), !,
    ((get_number(Board, Size, PrevR, NextC, Tile5), TileRightTop = [Tile5]) ; TileRightTop = []), !,
    append(TileTop, TileBot, Vertical1),
    append(Vertical1, TileRightTop, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 2 - 3 Adjacents
% Fetchs the adjacent tiles for shape_3_2_3
get_3_2_3_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    PrevC is C - 1, 
    Prev2C is C - 2,
    NextC is C + 1,
    get_horizontal_adjacent_numbers(Board, Size, NextR, PrevC, Horizontal1),
    ((get_number(Board, Size, R, Prev2C, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, NextC, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, PrevR, PrevC, Tile3), TileTop = [Tile3]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, PrevC, Tile4), TileBot = [Tile4]) ; TileBot = []), !,
    ((get_number(Board, Size, PrevR, C, Tile5), TileRightTop = [Tile5]) ; TileRightTop = []), !,
    append(TileTop, TileBot, Vertical1),
    append(Vertical1, TileRightTop, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 3 - 1 Adjacents
% Fetchs the adjacent tiles for shape_3_3_1
get_3_3_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    PrevC is C - 1, 
    Prev2C is C - 2,
    NextC is C + 1,
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal1),
    ((get_number(Board, Size, NextR, Prev2C, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, NextR, NextC, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, PrevR, C, Tile3), TileTop = [Tile3]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, C, Tile4), TileBot = [Tile4]) ; TileBot = []), !,
    ((get_number(Board, Size, Next2R, PrevC, Tile5), TileLeftBot = [Tile5]) ; TileLeftBot = []), !,
    append(TileTop, TileBot, Vertical1),
    append(Vertical1, TileLeftBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 3 - 2 Adjacents
% Fetchs the adjacent tiles for shape_3_3_2
get_3_3_2_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    PrevC is C - 1, 
    Prev2C is C - 2,
    NextC is C + 1,
    get_horizontal_adjacent_numbers(Board, Size, PrevR, C, Horizontal1),
    ((get_number(Board, Size, R, Prev2C, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, NextC, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, Prev2R, C, Tile3), TileTop = [Tile3]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, C, Tile4), TileBot = [Tile4]) ; TileBot = []), !,
    ((get_number(Board, Size, NextR, PrevC, Tile5), TileLeftBot = [Tile5]) ; TileLeftBot = []), !,
    append(TileTop, TileBot, Vertical1),
    append(Vertical1, TileLeftBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 3 - 3 Adjacents
% Fetchs the adjacent tiles for shape_3_3_3
get_3_3_3_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    PrevC is C - 1, 
    NextC is C + 1,
    Next2C is C + 2,
    get_horizontal_adjacent_numbers(Board, Size, PrevR, NextC, Horizontal1),
    ((get_number(Board, Size, R, PrevC, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next2C, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, Horizontal2),
    append(Horizontal1, Horizontal2, HorizontalAdjacents), !,
    ((get_number(Board, Size, Prev2R, NextC, Tile3), TileTop = [Tile3]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, NextC, Tile4), TileBot = [Tile4]) ; TileBot = []), !,
    ((get_number(Board, Size, NextR, C, Tile5), TileLeftBot = [Tile5]) ; TileLeftBot = []), !,
    append(TileTop, TileBot, Vertical1),
    append(Vertical1, TileLeftBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 4 - 1 Adjacents
% Fetchs the adjacent tiles for shape_3_4_1
get_3_4_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    PrevC is C - 1, 
    Prev2C is C - 2,
    NextC is C + 1,
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical1),
    ((get_number(Board, Size, Prev2R, PrevC, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, PrevC, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, R, Prev2C, Tile3), TileLeft = [Tile3]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, NextC, Tile4), TileRight = [Tile4]) ; TileRight = []), !,
    ((get_number(Board, Size, PrevR, Prev2C, Tile5), TileLeftTop = [Tile5]) ; TileLeftTop = []), !,
    append(TileLeft, TileRight, Horizontal1),
    append(Horizontal1, TileLeftTop, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 4 - 2 Adjacents
% Fetchs the adjacent tiles for shape_3_4_2
get_3_4_2_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    Prev2R is R - 2,
    NextR is R + 1,
    PrevC is C - 1, 
    NextC is C + 1,
    Next2C is C + 2,
    get_vertical_adjacent_numbers(Board, Size, R, NextC, Vertical1),
    ((get_number(Board, Size, Prev2R, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, R, PrevC, Tile3), TileLeft = [Tile3]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next2C, Tile4), TileRight = [Tile4]) ; TileRight = []), !,
    ((get_number(Board, Size, PrevR, PrevC, Tile5), TileLeftTop = [Tile5]) ; TileLeftTop = []), !,
    append(TileLeft, TileRight, Horizontal1),
    append(Horizontal1, TileLeftTop, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 4 - 3 Adjacents
% Fetchs the adjacent tiles for shape_3_4_3
get_3_4_3_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    PrevC is C - 1, 
    NextC is C + 1,
    Next2C is C + 2,
    get_vertical_adjacent_numbers(Board, Size, NextR, NextC, Vertical1),
    ((get_number(Board, Size, PrevR, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, Vertical2),
    append(Vertical1, Vertical2, VerticalAdjacents), !,
    ((get_number(Board, Size, NextR, PrevC, Tile3), TileLeft = [Tile3]) ; TileLeft = []), !,
    ((get_number(Board, Size, NextR, Next2C, Tile4), TileRight = [Tile4]) ; TileRight = []), !,
    ((get_number(Board, Size, R, PrevC, Tile5), TileLeftTop = [Tile5]) ; TileLeftTop = []), !,
    append(TileLeft, TileRight, Horizontal1),
    append(Horizontal1, TileLeftTop, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 5 - 1 Adjacents
% Fetchs the adjacent tiles for shape_3_5_1
get_3_5_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevC is C - 1,
    NextC is C + 1,
    Next2C is C + 2,
    Next3C is C + 3,
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical1),
    get_vertical_adjacent_numbers(Board, Size, R, NextC, Vertical2),
    get_vertical_adjacent_numbers(Board, Size, R, Next2C, Vertical3),
    append(Vertical1, Vertical2, TempVertical),
    append(TempVertical, Vertical3, VerticalAdjacents), !,
    ((get_number(Board, Size, R, PrevC, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next3C, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 5 - 2 Adjacents
% Fetchs the adjacent tiles for shape_3_5_2
get_3_5_2_adjacents(Board, Size, R, C, Adjacents) :-
    Prev2C is C - 2,
    PrevC is C - 1,
    NextC is C + 1,
    Next2C is C + 2,
    get_vertical_adjacent_numbers(Board, Size, R, PrevC, Vertical1),
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical2),
    get_vertical_adjacent_numbers(Board, Size, R, NextC, Vertical3),
    append(Vertical1, Vertical2, TempVertical),
    append(TempVertical, Vertical3, VerticalAdjacents), !,
    ((get_number(Board, Size, R, Prev2C, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, Next2C, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 5 - 3 Adjacents
% Fetchs the adjacent tiles for shape_3_5_3
get_3_5_3_adjacents(Board, Size, R, C, Adjacents) :-
    Prev3C is C - 3,
    Prev2C is C - 2,
    PrevC is C - 1,
    NextC is C + 1,
    get_vertical_adjacent_numbers(Board, Size, R, Prev2C, Vertical1),
    get_vertical_adjacent_numbers(Board, Size, R, PrevC, Vertical2),
    get_vertical_adjacent_numbers(Board, Size, R, C, Vertical3),
    append(Vertical1, Vertical2, TempVertical),
    append(TempVertical, Vertical3, VerticalAdjacents), !,
    ((get_number(Board, Size, R, Prev3C, Tile1), TileLeft = [Tile1]) ; TileLeft = []), !,
    ((get_number(Board, Size, R, NextC, Tile2), TileRight = [Tile2]) ; TileRight = []), !,
    append(TileLeft, TileRight, HorizontalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 6 - 1 Adjacents
% Fetchs the adjacent tiles for shape_3_6_1
get_3_6_1_adjacents(Board, Size, R, C, Adjacents) :-
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    Next3R is R + 3,
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal1),
    get_horizontal_adjacent_numbers(Board, Size, NextR, C, Horizontal2),
    get_horizontal_adjacent_numbers(Board, Size, Next2R, C, Horizontal3),
    append(Horizontal1, Horizontal2, TempHorizontal),
    append(TempHorizontal, Horizontal3, HorizontalAdjacents), !,
    ((get_number(Board, Size, PrevR, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, Next3R, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 6 - 2 Adjacents
% Fetchs the adjacent tiles for shape_3_6_2
get_3_6_2_adjacents(Board, Size, R, C, Adjacents) :-
    Prev2R is R - 2,
    PrevR is R - 1,
    NextR is R + 1,
    Next2R is R + 2,
    get_horizontal_adjacent_numbers(Board, Size, PrevR, C, Horizontal1),
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal2),
    get_horizontal_adjacent_numbers(Board, Size, NextR, C, Horizontal3),
    append(Horizontal1, Horizontal2, TempHorizontal),
    append(TempHorizontal, Horizontal3, HorizontalAdjacents), !,
    ((get_number(Board, Size, Prev2R, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, Next2R, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).

% Get 3 - 6 - 3 Adjacents
% Fetchs the adjacent tiles for shape_3_6_3
get_3_6_3_adjacents(Board, Size, R, C, Adjacents) :-
    Prev3R is R - 3,
    Prev2R is R - 2,
    PrevR is R - 1,
    NextR is R + 1,
    get_horizontal_adjacent_numbers(Board, Size, Prev2R, C, Horizontal1),
    get_horizontal_adjacent_numbers(Board, Size, PrevR, C, Horizontal2),
    get_horizontal_adjacent_numbers(Board, Size, R, C, Horizontal3),
    append(Horizontal1, Horizontal2, TempHorizontal),
    append(TempHorizontal, Horizontal3, HorizontalAdjacents), !,
    ((get_number(Board, Size, Prev3R, C, Tile1), TileTop = [Tile1]) ; TileTop = []), !,
    ((get_number(Board, Size, NextR, C, Tile2), TileBot = [Tile2]) ; TileBot = []), !,
    append(TileTop, TileBot, VerticalAdjacents),
    append(HorizontalAdjacents, VerticalAdjacents, Adjacents).
    