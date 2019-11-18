:- dynamic(cell/4).
:- dynamic(dead/2).
:- dynamic(castling_available/1).
:- dynamic(visited/2).

% ====================================================================================

% These predicates are solely used for the flood fill algorithm used to check if all pieces on the board are connected.

% Checks if the piece in the given row R and collumn C has been visited.
check_visited(R, C) :-
    visited(R, C).

% Records that the piece in the given row R and collumn C has been visited.
add_visited(R, C) :-
    assertz(visited(R, C)).

% Removes all record of the visited pieces
clean_visited :-
    retractall(visited(_, _)).

% ====================================================================================

% The following predicates are responsable for manipulating the "database", that is the current conditions of the board state

% Stops the player with the given Color pieces, from using the rook's castling move again in that game
castling_done(Color) :-
    retractall(castling_available(Color)).

% Adds to the cell (X, Y) the Piece of the given Color.
add_to_database(X, Y, Color, Piece) :-
    assertz(cell(X, Y, Color, Piece)).

% Removes from the cell (X, Y) the Piece of the given Color. This piece can be added again later in the game.
delete_from_database(X, Y, Color, Piece) :-
    retractall(cell(X, Y, Color, Piece)).

% Removes from the cell (X, Y) the queen of the given Color. It makes it so the queen is no longer playable in that game.
kill_from_database(X, Y, Color, queen) :-
    retractall(cell(X, Y, Color, queen)),!,
    retractall(dead(Color, queen)),!,
    asserta(dead(Color, queen)).   

% The Piece of the given Color, is removed from its current position and placed in the cell (X, Y).
change_database(X, Y, Color, Piece) :-
    retractall(cell(_, _, Color, Piece)),!,
    assertz(cell(X, Y, Color, Piece)).

% Resets the board back to its initial state.
reset_board :-
    retractall(cell(_,_,_,_)),
    retractall(dead(_, _)),
    retractall(castling_available(_)),
    asserta(castling_available(black)),
    assertz(castling_available(white)),
    asserta(cell(2, 2, black, king)),
    assertz(cell(3, 2, white, king)).

% ====================================================================================

% Starting conditions of the board.

% dead(white, queen, false).
% dead(black, queen, false).

castling_available(white).
castling_available(black).

cell(3, 2, white, king).
cell(2, 2, black, king).
