:- ensure_loaded('default_board.pl').
:- ensure_loaded('utils.pl').

% Given a color, this predicates return the enemy's color.
opposite(black, white).
opposite(white, black).

% ====================================================================================

% Counts the number of attackers (surrounded cells) of the given player's piece.
% If the piece is not on the board, then it return 0.
count_attackers(Player, Piece, Number) :-
    (cell(X, Y, Player, Piece), count_surrounded(X, Y, Number));
    Number is 0.

% Counts teh number of occupied cells surrounding the piece at (X, Y) coordinates.
count_surrounded(X, Y, Number) :-
    count_surrounded_collumn(Y, X, N1), % Checks for vertical neighbours.
    count_surrounded_row(Y, X, N2),     % Checks for horizontal neighbours.
    Number is N1 + N2.

% Counts the row neighbours.
% This first predicate checks if the piece has a limit to its left and/or a piece to its right.
count_surrounded_row(R, C, N) :-
    virtual_vertical_limit,
    R =< 1,
    NR is R + 1,
    ((cell(C, NR, _, _), N is 2);(N is 1)).

%This one checks if the piece has a limit to its right and/or a piece to its left.
count_surrounded_row(R, C, N) :-
    virtual_vertical_limit,
    R >= 4,
    PR is R - 1,
    ((cell(C, PR, _, _), N is 2);(N is 1)).

% Finally, this checks if there are any pieces left or right to the given piece's position.
count_surrounded_row(R, C, N) :-
    PR is R - 1, NR is R + 1,
    ((cell(C, PR, _, _), N1 is 1);(N1 is 0)),
    ((cell(C, NR, _, _), N2 is 1);(N2 is 0)),
    N is N1 + N2.

% Counts the column neighbours.
% This first predicate checks if the piece has a limit above it and/or a piece below it.
count_surrounded_collumn(R, C, N) :-
    virtual_horizontal_limit,
    C =< 1,
    NC is C + 1,
    ((cell(NC, R, _, _), N is 2);(N is 1)).

% This one checks if the piece has a limit below it and/or a piece above it.
count_surrounded_collumn(R, C, N) :-
    virtual_horizontal_limit,
    C >= 4,
    PC is C - 1,
    ((cell(PC, R, _, _), N is 2);(N is 1)).

% Finally, this checks if there are any pieces below or above the given piece's position.
count_surrounded_collumn(R, C, N) :-
    PC is C - 1, NC is C + 1,
    ((cell(PC, R, _, _), N1 is 1);(N1 is 0)),
    ((cell(NC, R, _, _), N2 is 1);(N2 is 0)),
    N is N1 + N2.

% ====================================================================================

% Checks if any queen is surrounded. Called at the end of each turn.
check_queens_death :-
    (check_queen_death(white); true),   % Both checks are always made because the queens can both die in the same turn.
    (check_queen_death(black); true).

% Checks if a queen is dead and removes it from game.
% The user is no longer allowed to play the queen.
check_queen_death(Player) :-
    cell(QC, QR, Player, queen),!,
    check_surrounded(QR, QC),!,
    kill_from_database(QC, QR, Player, queen). % This line effectively removes the queen from the game once and for all.

% ====================================================================================
% The following predicates do basically the same verifications as the above 'count_surrounded' predicates.
% However, this ondes do not count and can return false.
% Because of this, they have very distinct use cases (the counters work to value plays for the AI, the checkers are used to verify a PvP game).
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

% ====================================================================================

% Checks if a movement (defined by its starting and final positions) crosses an enemy piece.
% This predicate only checks for vertical and horizontal intersections.
straight_cross_enemy(Rinit, Cinit, Rdest, Cdest, Player) :-
    opposite(Player, Enemy),!,
    \+ ((Rinit =:= Rdest), (Cinit < Cdest), \+ straight_horizontal_cross_enemy(Rinit, Cinit, Cdest, Enemy, 1)),!,
    \+ ((Rinit =:= Rdest), (Cinit > Cdest), \+ straight_horizontal_cross_enemy(Rinit, Cinit, Cdest, Enemy, -1)),!,
    \+ ((Cinit =:= Cdest), (Rinit < Rdest), \+ straight_vertical_cross_enemy(Cinit, Rinit, Rdest, Enemy, 1)),!,
    \+ ((Cinit =:= Cdest), (Rinit > Rdest), \+ straight_vertical_cross_enemy(Cinit, Rinit, Rdest, Enemy, -1)).

% Goes through each column checking if there are no Enemy piece's along the way.
% Calls itself recursively until it reaches the destination column).
straight_horizontal_cross_enemy(Row, Ccurr, Cdest, Enemy, C_delta) :-
    Cnext is Ccurr + C_delta,!,
    \+ cell(Cnext, Row, Enemy, _),!,
    ((Cnext =:= Cdest); straight_horizontal_cross_enemy(Row, Cnext, Cdest, Enemy, C_delta)).

% Goes through each row checking if there are no Enemy piece's along the way.
% Calls itself recursively until it reaches the destination row).
straight_vertical_cross_enemy(Collumn, Rcurr, Rdest, Enemy, R_delta) :-
    Rnext is Rcurr + R_delta,!,
    \+ cell(Collumn, Rnext, Enemy, _),!,
    ((Rnext =:= Rdest); straight_vertical_cross_enemy(Collumn, Rnext, Rdest, Enemy, R_delta)).

% Checks if a movement (defined by its starting and final positions) crosses an enemy piece.
% This predicate only checks for diagonal intersections.
diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest, Player) :-
    opposite(Player, Enemy),!,
    \+ ((Rinit < Rdest), (Cinit < Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest,  1,  1, Enemy)),!,
    \+ ((Rinit < Rdest), (Cinit > Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest,  1, -1, Enemy)),!,
    \+ ((Rinit > Rdest), (Cinit < Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest, -1,  1, Enemy)),!,
    \+ ((Rinit > Rdest), (Cinit > Cdest), \+ diagonal_cross_enemy(Rinit, Cinit, Rdest, Cdest, -1, -1, Enemy)).

% Goes through each diagonal cell checking if there are no Enemy piece's along the way.
% Calls itself recursively until it reaches the destination position).
% Thanks to adaptable R and C deltas, this predicate can check every diagonal direction just from changing these values.
diagonal_cross_enemy(Rcurr, Ccurr, Rdest, Cdest, R_delta, C_delta, Enemy) :-
    Rnext is Rcurr + R_delta,
    Cnext is Ccurr + C_delta,!,
    \+ cell(Cnext, Rnext, Enemy, _),!,
    ((Rnext =:= Rdest ; Cnext =:= Cdest); diagonal_cross_enemy(Rnext, Cnext, Rdest, Cdest, R_delta, C_delta, Enemy)).

% ====================================================================================

% Resets the board back to its initial state.
reset_board :-
    retractall(cell(_,_,_,_)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).

% Detects when a piece is out of the virtual 4x4 board and shifts all pieces accordingly.
readjust_board :-
    ((cell(0, _, _, _), shift_right); true),
    ((cell(5, _, _, _), shift_left); true),
    ((cell(_, 0, _, _), shift_down); true),
    ((cell(_, 5, _, _), shift_up); true).

% Shifts all pieces up by one cell.
shift_up :-
    shift_board(-1, 0).

% Shifts all pieces down by one cell.
shift_down :-
    shift_board(1, 0).

% Shifts all pieces left by one cell.
shift_left :-
    shift_board(0, -1).

% Shifts all pieces right by one cell.
shift_right :-
    shift_board(0, 1).

% Shifts the board Row_delta cells vertically and Column_delta cells horizontally.
shift_board(Row_delta, Collumn_delta) :-
    \+((cell(C, R, Color, Piece),
        X is C + Collumn_delta,
        Y is R + Row_delta,
       \+ change_database(X, Y, Color, Piece))). % This line removes the piece from its previous position and adds it to the new one.
