:- ensure_loaded('logic.pl').
:- ensure_loaded('board.pl').

% Reads the user input coordinates until it finds a valid position to place the queen, returning true,
% if the queen is dead or the user cancels the piece choice, it returns false.
place(Player, queen) :-
    \+ cell(_, _, Player, queen),!,
    \+ dead(Player, queen),         % Check if the players queen has been killed before, in which case the queen can't be replaced on the board.
    repeat,
    read_coords(X, Y, Player, queen, place),
    (cancel(X, Y);
     (valid_cell(Y, X, Player),
      add_to_database(X, Y, Player, queen))),!,
    \+ cancel(X, Y).

% Reads the user input coordinates until it finds a valid position to place the Piece, returnin true,
% or if the user cancels the piece choice, returning false.
place(Player, Piece) :-
    \+ cell(_, _, Player, Piece),
    repeat,
    read_coords(X, Y, Player, Piece, place),
    (cancel(X, Y);
     (valid_cell(Y, X, Player),
      add_to_database(X, Y, Player, Piece))),!,
    \+ cancel(X, Y).

% Checks if the piece is already on the board, in which case it return false, if not it adds it.
% Since the placement is done by the AI that already analysed the move as being valid, no further validation is done.
place_ai(Player, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),
    add_to_database(X, Y, Player, Piece).

% ====================================================================================

% Checks if the Players queen can be place in (X, Y), namely if it hasn't been killed before and the cell is valid.
check_placement(Player, queen, X, Y) :-
    \+ cell(_, _, Player, queen),!,
    \+ dead(Player, queen),!,
    valid_cell(Y, X, Player).

% Checks if the Players Piece can be place in (X, Y), namely if the cell is valid.
check_placement(Player, Piece, X, Y) :-
    \+ cell(_, _, Player, Piece),!,
    valid_cell(Y, X, Player).

% ====================================================================================

% Checks if the cell choosen is valid for a placement.
valid_cell(R, C, Player) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits.
    within_virtual_limits(R, C),!,      % Ensures that, if there is a virtual limit, the new position doesn't break it.
    \+ cell(C, R, _, _),!,              % Checks if there is already a piece in that cell.
    \+ side_enemy_king(R, C, Player),!, % Ensures it is not adjacent to the enemy king.
    near_same_color(R, C, Player).      % Ensures that it is connected to a piece of the same color.

% ====================================================================================

% Cheks if the position (X, Y) is to the side of the given Piece with the given Color.
side_piece(X, Y, Color, Piece) :-
    cell(PieceX, PieceY, Color, Piece),
    PrevY is PieceY - 1, NextY is PieceY + 1,
    PrevX is PieceX - 1, NextX is PieceX + 1,
    !,
    ((Y =:= PieceY, (X =:= PrevX; X =:= NextX));
     (X =:= PieceX, (Y =:= PrevY; Y =:= NextY))).

% Checks if the given coordinates (C, R) are next to the king of the opposite Color.
side_enemy_king(R, C, Color) :-
    opposite(Color, Opposite),
    side_piece(C, R, Opposite, king).

% Returns true if there is at least one piece of the same Color, adjacent to the cell of coordinates (C, R).
near_same_color(R, C, Color) :-
    PR is R - 1, NR is R + 1,
    PC is C - 1, NC is C + 1,
    !,
    (cell(NC, R, Color, _);
     cell(PC, R, Color, _);
     cell(C, NR, Color, _);
     cell(C, PR, Color, _);
     cell(NC, NR, Color, _);
     cell(NC, PR, Color, _);
     cell(PC, NR, Color, _);
     cell(PC, PR, Color, _)).