:- include('display.pl').
:- include('puzzleDatabase.pl').
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(system)).

exactly(_, [], 0).
exactly(X, [Y|L], N) :-
    X #= Y #<=> B,
    N #= M+B,
    exactly(X, L, M).

row_list(Size, Row) :-
    length(Row, Size).
generate_empty_board(Size, Board) :-
    length(Board, Size),
    maplist(row_list(Size), Board).

flatten_board([], FlatBoard, FlatBoard).
flatten_board([Row | RestBoard], AuxBoard, FlatBoard) :-
    append(AuxBoard, Row, NewAuxBoard),
    flatten_board(RestBoard, NewAuxBoard, FlatBoard).
flatten_board(Board, FlatBoard) :-
    flatten_board(Board, [], FlatBoard).

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

get_adjacent_numbers(Board, Size, Row, Column, Adjacent) :-
    get_horizontal_adjacent_numbers(Board, Size, Row, Column, HorizontalAdjacent),
    get_vertical_adjacent_numbers(Board, Size, Row, Column, VerticalAdjacent),
    append(HorizontalAdjacent, VerticalAdjacent, Adjacent).

get_number(Board, Size, R, C, Number) :-
    R >= 0, C >= 0, 
    R < Size, C < Size,
    nth0(R, Board, Row),
    nth0(C, Row, Number).

get_adjacent_3(Board, Size, R, C, R3, C3) :-
    PR is R - 1, NR is R + 1,
    PC is C - 1, NC is C + 1,
    ((get_number(Board, Size, PR, C, 3), R3 is PR, C3 is C);
     (get_number(Board, Size, NR, C, 3), R3 is NR, C3 is C);
     (get_number(Board, Size, R, PC, 3), R3 is R, C3 is PC);
     (get_number(Board, Size, R, NC, 3), R3 is R, C3 is NC)).

get_adjacent_3(Board, Size, R, C, R3_1, C3_1, R3_2, C3_2) :-
    get_adjacent_3(Board, Size, R, C, R3_1, C3_1),
    (get_adjacent_3(Board, Size, R, C, R3_2, C3_2),
    (R3_2 #\= R3_1; C3_2 #\= C3_1)).

check_3_limit(Board, Size, R, C, 1) :-
    get_adjacent_3(Board, Size, R, C, R3, C3),
    get_adjacent_numbers(Board, Size, R3, C3, Adjacent),
    exactly(3, Adjacent, 2).

check_3_limit(Board, Size, R, C, 2) :-
    get_adjacent_3(Board, Size, R, C, R3_1, C3_1, R3_2, C3_2),
    get_adjacent_numbers(Board, Size, R3_1, C3_1, Adjacent1),
    get_adjacent_numbers(Board, Size, R3_2, C3_2, Adjacent2),
    exactly(3, Adjacent1, 1),
    exactly(3, Adjacent2, 1).

apply_constraint(Board, Size, R, C, 3) :-
    get_adjacent_numbers(Board, Size, R, C, Adjacent),
    exactly(3, Adjacent, 2),
    check_3_limit(Board, Size, R, C, 2).

apply_constraint(Board, Size, R, C, 3) :-
    get_adjacent_numbers(Board, Size, R, C, Adjacent),
    exactly(3, Adjacent, 1),
    check_3_limit(Board, Size, R, C, 1).

apply_constraint(Board, Size, R, C, 2) :-
    get_adjacent_numbers(Board, Size, R, C, Adjacent),
    exactly(2, Adjacent, 1).

apply_constraint(Board, Size, R, C, 1) :-
    get_adjacent_numbers(Board, Size, R, C, Adjacent),
    exactly(1, Adjacent, 0).

verify_column(_, _, Size, Size).
verify_column(Board, R, C, Size) :-
    get_number(Board, Size, R, C, Number),
    apply_constraint(Board, Size, R, C, Number),
    NC is C + 1,
    verify_column(Board, R, NC, Size).

verify_rows(_, Size, Size).
verify_rows(Board, R, Size) :-
    verify_column(Board, R, 0, Size),
    NR is R + 1,
    verify_rows(Board, NR, Size).

valid_board(Board, Size) :-
    verify_rows(Board, 0, Size).

domain_row(Min, Max, Row) :-
    domain(Row, Min, Max).

solve_puzzle(Board) :-
    statistics(walltime, [Start,_]),

    length(Board, Size),
    maplist(domain_row(1, 3), Board),
    valid_board(Board, Size),
    flatten_board(Board, FlatBoard),
    labeling([], FlatBoard),
    display_solution(Board, Size),
    
    statistics(walltime, [End,_]),
	Time is End - Start,
    format(' > Duration: ~3d s~n', [Time]), nl.