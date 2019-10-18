:- include('board.pl').

% Display
opposite(black, white).
opposite(white, black).

display_board :-
    nl,
    write_top_corner_left,
    \+write_top(5, 0),
    write_top_corner_right, nl,
    display_coordinates,
    \+display_board([6, 6], 0),
    write_bottom_corner_left,
    \+write_bottom(5, 0),
    write_bottom_corner_right, nl.

display_board([R, C], X) :-
    X < R, 
    write_coordinates_line,
    \+write_line(C, 0), write_right, nl,
    display_coordinates(X),
    \+display_row([X, C], 0), nl,
    N is X + 1, display_board([R, C], N).
   
display_row([R, C], X) :-
    X < C, display_piece(R, X),
    N is X + 1, display_row([R, C], N).

display_piece(R, C) :-
    cell(C, R, Color, Piece),
    display(Piece, Color).

display_piece(R, C) :-
    \+cell(C, R, Color, Piece),
    ansi_format([bold, bg(black)],'   ', []), %change bg color here!!
    ansi_format([bold, fg(white)],'│', []).

display_coordinates :-
    ansi_format([bold, fg(white)], '│   │ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │', []),
    nl.

display_coordinates(R) :-
    ansi_format([bold, fg(white)],'│ ', []),
    ansi_format([bold, fg(white)], R, []),
    ansi_format([bold, fg(white)],' │', []).

write_coordinates_line :-
    ansi_format([bold, fg(white)],'├───┼', []).

write_top(Length, X) :-
    X < Length, ansi_format([bold,  fg(white)],'┬───', []),
    N is X + 1, write_top(Length, N).

write_bottom(Length, X) :-
    X < Length, ansi_format([bold, fg(white)],'┴───', []),
    N is X + 1, write_bottom(Length, N).

write_line(Length, X) :-
    X < Length - 1, ansi_format([bold, fg(white)],'───┼', []),
    N is X + 1, write_line(Length, N).

write_top_corner_right :-
    ansi_format([bold, fg(white)],'┬───┐', []).

write_top_corner_left :-
    ansi_format([bold, fg(white)],'┌───', []).

write_right :-
    ansi_format([bold, fg(white)],'───┤', []).

write_left :-
    ansi_format([bold, fg(white)],'├───', []).

write_bottom_corner_right :-
    ansi_format([bold, fg(white)],'┴───┘', []).

write_bottom_corner_left :-
    ansi_format([bold, fg(white)],'└───', []).


display(Piece, Color) :-
    piece(Piece, Color, Char),
    opposite(Color, Oposite),
    ansi_format([bold, hbg(Oposite), fg(Color)],' ', []),
    ansi_format([bold, hbg(Oposite), fg(Color)], Char, []),
    ansi_format([bold, hbg(Oposite), fg(Color)],' ', []),
    ansi_format([bold, fg(white)],'│', []).


