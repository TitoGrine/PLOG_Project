% This module holds everything related to game logic

% Set the board back to the starting board
reset_board :-
    retractall(cell(_,_,_,_)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).

% Shifts all pieces to the right by 1 column
shift_right(0).
shift_right(C) :-
    C > 0, \+ cell(C, _, _, _),
    Cleft is C-1, shift_right(Cleft).
shift_right(C) :-
    C > 0, Cleft is C-1, Cright is C+1,
    retract(cell(C, _, _, _)),
    assertz(cell(Cright, _, _, _)),
    shift_right(Cleft).

% Shifts all pieces to the left by 1 column
shift_left(6).
shift_left(C) :-
    C < 6, \+ cell(C, _, _, _),
    Cright is C+1, shift_left(Cright).
shift_left(C) :-
    C < 6, Cleft is C-1, Cright is C+1,
    retract(cell(C, _, _, _)),
    assertz(cell(Cleft, _, _, _)),
    shift_left(Cright).

% Shifts all pieces down by 1 row
shift_down(0).
shift_down(R) :-
    R > 0, \+ cell(_, R, _, _),
    Rup is R-1, shift_down(Rup).
shift_down(R) :-
    R > 0, Rup is R-1, Rdown is R+1,
    retract(cell(_, R, _, _)),
    assertz(cell(_, Rdown, _, _)),
    shift_down(Rup).

% Shifts all pieces up by 1 row
shift_up(6).
shift_up(R) :-
    R < 6, \+ cell(_, R, _, _),
    Rdown is R+1, shift_up(Rdown).
shift_up(R) :-
    R < 6, Rup is R-1, Rdown is R+1,
    retract(cell(_, R, _, _)),
    assertz(cell(_, Rup, _, _)),
    shift_up(Rdown).

play_piece(R, C, Color,Piece) :-
    assertz(cell(R, C, Color, Piece)).
