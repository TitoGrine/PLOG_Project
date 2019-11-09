:- use_module(library(lists)).

read_coords(X, Y, Piece) :-
    format('New Position for ~s', [Piece]),
    read(Input),
    arg(1, Input, X),
    arg(2, Input, Y),
    integer(X), integer(Y).