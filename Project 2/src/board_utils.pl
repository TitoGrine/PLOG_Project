:- use_module(library(lists)).

% Auxilary function that generates a list of length Size with all variables
% Meant to be used with maplist.
row_list(Size, Row) :-
    length(Row, Size).

% Generates an empty square board of Size x Size.
generate_empty_board(Size, Board) :-
    length(Board, Size),
    maplist(row_list(Size), Board).

% Retrieves the number or variale in the cell with the given coordinates.
get_number(Board, Size, R, C, Number) :-
    R >= 0, C >= 0, 
    R < Size, C < Size,
    nth0(R, Board, Row),
    nth0(C, Row, Number).

% The following predicates are used to get the values (numbers) of the 
% cells horizontally adjacent to the given cell.
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

% The following predicates are used to get the values (numbers) of the 
% cells verticcally adjacent to the given cell.
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

% Retrieves the values (numbers or variables) of the cells adjacent to the given cell.
get_adjacent_numbers(Board, Size, Row, Column, AdjacentNumbers) :-
    get_horizontal_adjacent_numbers(Board, Size, Row, Column, HorizontalAdjacent),
    get_vertical_adjacent_numbers(Board, Size, Row, Column, VerticalAdjacent),
    append(HorizontalAdjacent, VerticalAdjacent, AdjacentNumbers).

% The following predicates are used to get the coordinates of the 
% cells horizontally adjacent to the given cell.
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

% The following predicates are used to get the coordinates of the 
% cells vertically adjacent to the given cell.
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

% Retrieves the coordinates of the cells adjacent to the given cell.
get_adjacent_coords(Size, Row, Column, Adjacent) :-
    get_horizontal_adjacent_coords(Size, Row, Column, HorizontalAdjacent),
    get_vertical_adjacent_coords(Size, Row, Column, VerticalAdjacent),
    append(HorizontalAdjacent, VerticalAdjacent, Adjacent).
