repeat.

repeat :-
    repeat.

read_coords(X, Y) :-
    read(Input),
    append(First, [32|Last], Input),
    name(X, First), name(Y, Last),
    integer(X), integer(Y).