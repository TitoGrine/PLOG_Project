:- use_module(library(lists)).

within_limits(R, C) :-
    (R >= 0, R =< 5),
    (C >= 0, C =< 5).

within_virtual_limits(R, C) :-
    \+ breaks_virtual_horizontal_limit(C),
    \+ breaks_virtual_vertical_limit(R).

breaks_virtual_horizontal_limit(C) :-
    (C =:= 0 ; C =:= 5),
    virtual_horizontal_limit.

breaks_virtual_vertical_limit(R) :-
    (R =:= 0 ; R =:= 5),
    virtual_vertical_limit.

virtual_horizontal_limit :-
    cell(1, _, _, _),
    cell(4, _, _, _).

virtual_vertical_limit :-
    cell(_, 1, _, _),
    cell(_, 4, _, _).

cancel(-1, -1).

read_coords_no_cancel(X, Y, Piece) :-
    ansi_format([fg(green)],'New Position for ~s', [Piece]),
    read(Input),
    arg(1, Input, X), arg(2, Input, Y),
    integer(X), integer(Y).

read_coords(X, Y, Piece) :-
    ansi_format([fg(green)],'New Position for ~s("-1,-1" to cancel)', [Piece]),
    read(Input),
    arg(1, Input, X), arg(2, Input, Y),
    integer(X), integer(Y).

choose_piece(Player, Piece) :-
    repeat,
    ansi_format([fg(green)],'Player ~s choose a piece: ', [Player]),
    read(Piece),
    (
        piece(Piece, Player, _);
        (ansi_format([fg(green)],'Invalid piece.', []), nl, false)
    ), !.