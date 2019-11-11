:- ensure_loaded('default_board.pl').
:- ensure_loaded('utils.pl').

opposite(black, white).
opposite(white, black).

% This module holds everything related to game logic
check_queens_death :-
    (check_queen_death(white); true),
    (check_queen_death(black); true).
    
check_queen_death(Player) :-
    cell(QC, QR, Player, queen),!,
    check_surrounded(QR, QC),!,
    kill_from_database(QC, QR, Player, queen).


check_surrounded(R, C) :-
    (check_surrounded_collumn(R, C),
     check_surrounded_row(R, C)).

check_surrounded_row(R, C) :-
    virtual_vertical_limit,
    R =< 1,
    NR is R + 1,
    !,
    cell(C, NR, _, _).

check_surrounded_row(R, C) :-
    virtual_vertical_limit,
    R >= 4,
    PR is R - 1,
    !,
    cell(C, PR, _, _).

check_surrounded_row(R, C) :-
    PR is R - 1, NR is R + 1,
    !,
    (cell(C, PR, _, _),
     cell(C, NR, _, _)).

check_surrounded_collumn(R, C) :-
    virtual_horizontal_limit,
    C =< 1,
    NC is C + 1,
    !,
    cell(NC, R, _, _).

check_surrounded_collumn(R, C) :-
    virtual_horizontal_limit,
    C >= 4,
    PC is C - 1,
    !,
    cell(PC, R, _, _).

check_surrounded_collumn(R, C) :-
    PC is C - 1, NC is C + 1,
    !,
    (cell(PC, R, _, _),
     cell(NC, R, _, _)).


straight_cross_enemy(Rinit, Cinit, Rdest, Cdest, Player) :-
    opposite(Player, Enemy),!,
    \+ ((Rinit =:= Rdest), (Cinit < Cdest), \+ straight_horizontal_cross_enemy(Rinit, Cinit, Cdest, Enemy, 1)),!,
    \+ ((Rinit =:= Rdest), (Cinit > Cdest), \+ straight_horizontal_cross_enemy(Rinit, Cinit, Cdest, Enemy, -1)),!,
    \+ ((Cinit =:= Cdest), (Rinit < Rdest), \+ straight_vertical_cross_enemy(Cinit, Rinit, Rdest, Enemy, 1)),!,
    \+ ((Cinit =:= Cdest), (Rinit > Rdest), \+ straight_vertical_cross_enemy(Cinit, Rinit, Rdest, Enemy, -1)).

straight_horizontal_cross_enemy(Row, Ccurr, Cdest, Enemy, C_delta) :-
    Cnext is Ccurr + C_delta,!,
    \+ cell(Cnext, Row, Enemy, _),!,
    ((Cnext =:= Cdest); straight_horizontal_cross_enemy(Row, Cnext, Cdest, Enemy, C_delta)).

straight_vertical_cross_enemy(Collumn, Rcurr, Rdest, Enemy, R_delta) :-
    Rnext is Rcurr + R_delta,!,
    \+ cell(Collumn, Rnext, Enemy, _),!,
    ((Rnext =:= Rdest); straight_vertical_cross_enemy(Collumn, Rnext, Rdest, Enemy, R_delta)).

diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest, Player) :-
    opposite(Player, Enemy),!,
    \+ ((Rinit < Rdest), (Cinit < Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest,  1,  1, Enemy)),!,
    \+ ((Rinit < Rdest), (Cinit > Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest,  1, -1, Enemy)),!,
    \+ ((Rinit > Rdest), (Cinit < Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest, -1,  1, Enemy)),!,
    \+ ((Rinit > Rdest), (Cinit > Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest, -1, -1, Enemy)).

diagonal_cross_enemy(Rcurr, Ccurr, Rdest, Cdest, R_delta, C_delta, Enemy) :-
    Rnext is Rcurr + R_delta,
    Cnext is Ccurr + C_delta,!,
    \+ cell(Cnext, Rnext, Enemy, _),!,
    ((Rnext =:= Rdest ; Cnext =:= Cdest); diagonal_cross_enemy(Rnext, Cnext, Rdest, Cdest, R_delta, C_delta, Enemy)).

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

