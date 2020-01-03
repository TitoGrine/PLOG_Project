:- include('display.pl').
:- include('puzzleDatabase.pl').
:- include('board_utils.pl').
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

% Predicate used in the labeling options, in order for the variable selection
% to be random.
selRandom(ListOfVars, Var, Rest) :-
    random_select(Var, ListOfVars, Rest). 

% Predicate used in the labeling options, in order for the value selection 
% to be random.
selRandom(Var, _Rest, BB0, BB1) :-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    (first_bound(BB0, BB1), Var #= Value ; later_bound(BB0, BB1), Var #\= Value ).

% Applies a restriction to the given List of variables so that there are exactly
% N variables with the value X in the List.
exactly(_, [], 0).
exactly(X, [Y|L], N) :-
    X #= Y #<=> B,
    N #= M+B,
    exactly(X, L, M).

% Applies a restriction to the cells with the coordinates in the given list so that,
% if the value in that cell is equal to Number, then the number of cells adjacent to it
% with that same Number can't be equal to N, otherwise it doesn't apply a meaningful
% restriction.
apply_different(_, _, [], _, _, _).
apply_different(Board, Size, [R-C | T], Number, N, B) :-
    get_number(Board, Size, R, C, PosNumber),
    PosNumber #\= Number #<=> Diff,
    M #\= N + B - (100 * Diff),
    get_adjacent_numbers(Board, Size, R, C, AdjacentNumbers),
    exactly(Number, AdjacentNumbers, M),
    apply_different(Board, Size, T, Number, N, B).

% It applies a restriction to the cell's variable of the given coordinates, so that,
% the Number on that variable must be the number of cells with that same Number, on
% the 'island' of identical cells where cell is inserted, that are adjacent to at least
% one cell on that 'island'.
apply_constraint(Board, Size, R-C) :-
    get_number(Board, Size, R, C, Number),
    Number #< 3 #<=> B,
    get_adjacent_numbers(Board, Size, R, C, AdjacentNumbers),
    ((N #= Number - 1) #\/ (N #= Number - 2 + B)),
    exactly(Number, AdjacentNumbers, N),
    get_adjacent_coords(Size, R, C, Adjacent),
    apply_different(Board, Size, Adjacent, Number, N, B).

% If the given Board is solvable, it finds a solution to it using restriction programming
% and displays the solved board, as well as the time spent on labeling plus other labeling
% statistics. If there are no solutions left then it will return 'no'.
solve_puzzle(Board) :-

    length(Board, Size),
    Max is Size - 1,

    append(Board, FlatBoard),
    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint(Board, Size), Positions),
    %write('.'),
    statistics(walltime, [Start,_]),
    labeling([], FlatBoard),
    statistics(walltime, [End,_]),
	Time is End - Start,
    
    display_board(Board,'Solution'),
    format(' > Duration: ~3d s~n~n', [Time]),
    format(' > Statistics: ~n~n', []), fd_statistics, nl.

% If the given Board is solvable, it finds a solution to it using restriction programming.
% If there are no solutions left then it will return 'no'. Will not display neither the 
% solved board nor any statistics related to the labeling process.
solve_puzzle_no_stats(Board) :-

    length(Board, Size),
    Max is Size - 1,

    append(Board, FlatBoard),
    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint(Board, Size), Positions),
    labeling([], FlatBoard).

% Much like the solve_puzzle predicate, it finds a solution (if one exists) to the given
% board, however the labeling process is done such that the given solution, provided there
% are more than one, is random. Unlike the solve_puzzle it does not display the board or
% labeling statistics, since it is suppose to be used for generating puzzles, as an 
% auxilary predicate.
solve_puzzle_random(Board) :-
    length(Board, Size),
    Max is Size - 1,
    append(Board, FlatBoard),
    domain(FlatBoard, 1, 3),
    findall(R-C, (between(0, Max, R), between(0, Max, C)), Positions),
    maplist(apply_constraint(Board, Size), Positions),
    labeling([value(selRandom)], FlatBoard).

% Works in a similar way to the exactly predicate. It applies a restriction to the list of 
% variables in the first argument, so that the number of variables that have the same number
% in the same position as the variable on the list in the second argument, is exactly Clues.
board_picking([], [], 0).
board_picking([Var | RB], [Number | RFB], Clues) :-
    Var #= Number #<=> N,
    Clues #= N + OtherClues,
    board_picking(RB, RFB, OtherClues).

% Generates a random unsolved puzzle and displays it in the terminal along with some statistics.
generate_puzzle(Dimensions, Board) :-
    generate_empty_board(Dimensions, Board),
    generate_empty_board(Dimensions, FullBoard),
    solve_puzzle_random(FullBoard),
    NumberCells is Dimensions * Dimensions,
    Lower is floor(NumberCells / 7.0), Upper is ceiling(NumberCells / 6.0),
    random(Lower, Upper, Clues), Blanks is NumberCells - Clues, !,
    append(Board, FlatEmptyBoard), append(FullBoard, FlatFullBoard),
    domain(FlatEmptyBoard, 0, 3),
    board_picking(FlatEmptyBoard, FlatFullBoard, Clues),
    global_cardinality(FlatEmptyBoard, [0-Blanks, 1-_, 2-_, 3-_]),
    statistics(walltime, [Start,_]),
    %write('.'),
    labeling([variable(selRandom), enum], FlatEmptyBoard),
    statistics(walltime, [End,_]),
	Time is End - Start,

    display_board(Board, 'Puzzle'),
    format(' > Duration: ~3d s~n~n', [Time]),
    format(' > Statistics: ~n~n', []), fd_statistics, nl.

% Counts the number of possible solutions for the given Board.
count_solutions(Board) :-
    findall(_, solve_puzzle_no_stats(Board), List),
    length(List, NumSolutions),
    format('~n > Number of different solutions: ~d ~n~n', [NumSolutions]).
