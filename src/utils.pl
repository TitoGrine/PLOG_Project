:- use_module(library(lists)).

%Checks if a piece is within the 5x5 board limits
within_limits(R, C) :-
    (R >= 0, R =< 5),
    (C >= 0, C =< 5).

%Checks if a piece is within the 4x4 virtual limits (only if they exist)
within_virtual_limits(R, C) :-
    \+ breaks_virtual_horizontal_limit(C),
    \+ breaks_virtual_vertical_limit(R).

%Checks if a piece breaks the horizontal limit
breaks_virtual_horizontal_limit(C) :-
    (C =:= 0 ; C =:= 5),        %A piece can only break the limit if it is left or right of the 4x4 central board
    virtual_horizontal_limit.

%Checks if a piece breaks the vertical limit
breaks_virtual_vertical_limit(R) :-
    (R =:= 0 ; R =:= 5),        %A piece can only break the limit if it is above or below the 4x4 central board
    virtual_vertical_limit.

%Checks if there is a horizontal limit to the dynamic game board
virtual_horizontal_limit :-
    cell(1, _, _, _),
    cell(4, _, _, _).

%Checks if there is a vertical limit to the dynamic game board
virtual_vertical_limit :-
    cell(_, 1, _, _),
    cell(_, 4, _, _).

%Checks if the coordinates are to cancel an action
cancel(-1, -1).

%======================================
%This predicates read coordinates from the standard input
%They only differ in the message displayed
read_coords_no_cancel(X, Y, Player, Piece, move) :-
    ansi_format([fg(green)],'Move ~s ~s to cell', [Player, Piece]),
    read(Input),
    arg(1, Input, X), arg(2, Input, Y),
    integer(X), integer(Y).

read_coords(X, Y, Player, Piece, move) :-
    ansi_format([fg(green)],'Move ~s ~s to cell ("-1,-1" to cancel)', [Player, Piece]),
    read(Input),
    arg(1, Input, X), arg(2, Input, Y),
    integer(X), integer(Y).

read_coords(X, Y, Player, Piece, place) :-
    ansi_format([fg(green)],'Place ~s ~s in cell ("-1,-1" to cancel)', [Player, Piece]),
    read(Input),
    arg(1, Input, X), arg(2, Input, Y),
    integer(X), integer(Y).

read_player_piece(X, Y) :-
    ansi_format([fg(green)],'Choose piece to remove', []),
    read(Input),
    arg(1, Input, X), arg(2, Input, Y),
    integer(X), integer(Y).
%=====================================

%Reads the name of a piece from the standard input
choose_piece(Player, Piece) :-
    repeat,
    ansi_format([fg(green)],'Player ~s choose a piece: ', [Player]),
    read(Piece),
    piece(Piece, Player, _).

%Determines the max value on a list
max_value_list([], Best, Best).

max_value_list([Head| Rest], Current, Best) :-
    greater(Head, Current), max_value_list(Rest, Head, Best).

max_value_list([Head| Rest], Current, Best) :-
    greater(Current, Head), max_value_list(Rest, Current, Best).

max_value_list([Head|Rest], Best) :-
    max_value_list(Rest, Head, Best),!.

%Compares the values between two moves
greater([Value1|_], [Value2|_]) :-
    Value1 >= Value2.
