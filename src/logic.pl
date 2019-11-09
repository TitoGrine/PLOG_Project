% This module holds everything related to game logic

check_surrounded(R, C) :-
    (check_surrounded_collumn(R, C),
     check_surrounded_row(R, C)).

check_surrounded_row(R, C) :-
    R =< 0,
    NR is R + 1,
    !,
    cell(C, NR, _, _).

check_surrounded_row(R, C) :-
    R >= 5,
    PR is R - 1,
    !,
    cell(C, PR, _, _).

check_surrounded_row(R, C) :-
    PR is R - 1, NR is R + 1,
    !,
    (cell(C, PR, _, _),
     cell(C, NR, _, _)).

check_surrounded_collumn(R, C) :-
    C =< 0,
    NC is C + 1,
    !,
    cell(NC, R, _, _).

check_surrounded_collumn(R, C) :-
    C >= 5,
    PC is C - 1,
    !,
    cell(PC, R, _, _).

check_surrounded_collumn(R, C) :-
    PC is C - 1, NC is C + 1,
    !,
    (cell(PC, R, _, _),
     cell(NC, R, _, _)).

% Set the board back to the starting board
reset_board :-
    retractall(cell(_,_,_,_)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).

readjust_board :-
    ((cell(0, _, _, _), shift_right); true),
    ((cell(5, _, _, _), shift_left); true),
    ((cell(_, 0, _, _), shift_down); true),
    ((cell(_, 5, _, _), shift_up); true).

shift_up :-
    shift_board(-1, 0).

shift_down :-
    shift_board(1, 0).

shift_left :-
    shift_board(0, -1).

shift_right :-
    shift_board(0, 1).

shift_board(Row_delta, Collumn_delta) :-
    \+((cell(C, R, Color, Piece),
        X is C + Collumn_delta,
        Y is R + Row_delta,
       \+ change_database(X, Y, Color, Piece))).

play_piece(R, C, Color,Piece) :-
    assertz(cell(R, C, Color, Piece)).
