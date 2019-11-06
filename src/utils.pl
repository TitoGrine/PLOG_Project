:- use_module(library(lists)).

read_coords(X, Y) :-
    read(Input),
    arg(1, Input, X),
    arg(2, Input, Y).