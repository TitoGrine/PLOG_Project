:- include('movement.pl').
:- include('placement.pl').
:- include('player.pl').

start_game :-
    display_board,
    play.

play :- 
    choose_piece(white, Piece),
    (move(white, Piece); place(white, Piece)),
    display_board.

%next_player(white, black).
%next_player(black, white).

