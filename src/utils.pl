:- use_module(library(lists)).

within_limits(R, C) :-
    (R >= 0, R =< 5),
    (C >= 0, C =< 5).

read_coords(X, Y, Piece) :-
    format('New Position for ~s', [Piece]),
    read(Input),
    arg(1, Input, X),
    arg(2, Input, Y),
    integer(X), integer(Y).