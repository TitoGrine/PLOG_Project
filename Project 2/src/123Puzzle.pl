:- include('display.pl').
:- include('puzzleDatabase.pl').
:- include('adjancy.pl').
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
    generate_empty_board(Dimensions, Board),
    generate_empty_board(Dimensions, FullBoard),
    random_solution(FullBoard),
    NumberCells is Dimensions * Dimensions,
    Lower is floor(NumberCells / 7.0), Upper is ceiling(NumberCells / 6.0),
    random(Lower, Upper, Clues), Blanks is NumberCells - Clues, !,
    append(Board, FlatBoard), append(FullBoard, FlatFullBoard),
    domain(FlatBoard, 0, 3),
    board_picking(FlatBoard, FlatFullBoard, Clues),
    global_cardinality(FlatBoard, [0-Blanks, 1-_, 2-_, 3-_]),
    statistics(walltime, [Start,_]),
    write('.'),
    labeling([variable(selRandom), enum], FlatBoard),
    statistics(walltime, [End,_]),
	Time is End - Start,

    %display_board(FullBoard, Dimensions),
    display_board(Board, Dimensions),
    format(' > Duration: ~3d s~n~n', [Time]),
    format(' > Statistics: ~n~n', []), fd_statistics, nl.