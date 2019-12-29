:- include('display.pl').
:- include('puzzleDatabase.pl').
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

selRandom(ListOfVars, Var, Rest) :-
    random_select(Var, ListOfVars, Rest). 

selRandom(Var, _Rest, BB0, BB1) :-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    (first_bound(BB0, BB1), Var #= Value ; later_bound(BB0, BB1), Var #\= Value ).

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

get_number(Board, Size, R, C, Number) :-
    R >= 0, C >= 0, 
    R < Size, C < Size,
    nth0(R, Board, Row),
    nth0(C, Row, Number).
    
apply_different(_, _, [], _, _, _).
apply_different(Board, Size, [R-C | T], Number, N, B) :-
    get_number(Board, Size, R, C, PosNumber),
    PosNumber #\= Number #<=> Diff,
    M #\= N + B - (100 * Diff),
    get_adjacent_numbers(Board, Size, R, C, AdjacentNumbers),
    exactly(Number, AdjacentNumbers, M),
    apply_different(Board, Size, T, Number, N, B).

apply_constraint(Board, Size, R-C) :-
    get_number(Board, Size, R, C, Number),
    Number #< 3 #<=> B,
    get_adjacent_numbers(Board, Size, R, C, AdjacentNumbers),
    ((N #= Number - 1) #\/ (N #= Number - 2 + B)),
    exactly(Number, AdjacentNumbers, N),
    get_adjacent_coords(Size, R, C, Adjacent),
    apply_different(Board, Size, Adjacent, Number, N, B).

random_solution(Board) :-
    length(Board, Size),
    Max is Size - 1,
    append(Board, FlatBoard),
    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint(Board, Size), Positions),
    labeling([value(selRandom)], FlatBoard).

solve_puzzle(Board) :-

    length(Board, Size),
    Max is Size - 1,

    append(Board, FlatBoard),
    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint(Board, Size), Positions),
    write('.'),
    statistics(walltime, [Start,_]),
    labeling([], FlatBoard),
    statistics(walltime, [End,_]),
	Time is End - Start,
    
    display_board(Board, Size),
    format(' > Duration: ~3d s~n~n', [Time]),
    format(' > Statistics: ~n~n', []), fd_statistics, nl.

board_picking([], [], 0).
board_picking([Var | RB], [Number | RFB], Clues) :-
    Var #= Number #<=> N,
    Clues #= N + OtherClues,
    board_picking(RB, RFB, OtherClues).

generate_puzzle(Dimensions, Board) :-
    generate_empty_board(Dimensions, TempBoard),
    generate_empty_board(Dimensions, FullBoard),
    random_solution(FullBoard),
    NumberCells is Dimensions * Dimensions,
    Lower is floor(NumberCells / 7.0), Upper is ceiling(NumberCells / 6.0),
    random(Lower, Upper, Clues), Blanks is NumberCells - Clues, !,
    append(TempBoard, FlatBoard), append(FullBoard, FlatFullBoard),
    domain(FlatBoard, 0, 3),
    board_picking(FlatBoard, FlatFullBoard, Clues),
    global_cardinality(FlatBoard, [0-Blanks, 1-_, 2-_, 3-_]),
    labeling([variable(selRandom), enum], FlatBoard),
    clear_board_zeros(TempBoard, Board).
    % statistics(walltime, [Start,_]),
    % write('.'),
    %labeling([variable(selRandom), enum], FlatBoard),
    % statistics(walltime, [End,_]),
	% Time is End - Start,

    % %display_board(FullBoard, Dimensions),
    % display_board(Board, Dimensions),
    % format(' > Duration: ~3d s~n~n', [Time]),
    % format(' > Statistics: ~n~n', []), fd_statistics, nl.

clear_board_zeros(Board, CleanBoard) :-
    length(Board, Dimensions),
    generate_empty_board(Dimensions, CleanBoard),
    clear_list_zeros(Board, CleanBoard).

clear_list_zeros([], []).

clear_list_zeros([Row1 | Rest1], [Row2 | Rest2]) :-
    clear_row_zeros(Row1, Row2), !,
    clear_list_zeros(Rest1, Rest2).

clear_row_zeros([], []).

clear_row_zeros([H1 | T1], [H1 | T2]) :-
    H1 =\= 0, !,
    clear_row_zeros(T1, T2).

clear_row_zeros([H1 | T1], [_ | T2]) :-
    H1 =:= 0,
    clear_row_zeros(T1, T2).

% The following version of the count_solutions is intended to be used when solve_puzzle displays the solution.
count_solutions(Board, NumSolutions) :-
    length(Board, Size),
    Max is Size - 1,
    append(Board, FlatBoard),

    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint(Board, Size), Positions),

    findall(FlatBoard, labeling([], FlatBoard), ListSolutions),
    length(ListSolutions, NumSolutions).

% The following version of the predicate works fine if solve_puzzle does not display the solution.
% count_solutions(Board, NumSolutions) :-
%     findall(_, solve_puzzle(Board), List),
%     length(List, NumSolutions).

generate_unique_puzzle(Dimensions, Board) :-
    generate_puzzle(Dimensions, Board),
    count_solutions(Board, 1).