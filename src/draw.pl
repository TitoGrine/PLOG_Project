% This module makes all actual drawing in the console.
% This way drawing details are hidden from the view clauses

% High level rules used by display
draw_top_boundary :-
    draw_top_corner_left,
    \+draw_top(5, 0),
    draw_top_corner_right, nl.

draw_bottom_boundary :-
    draw_bottom_corner_left,
    \+draw_bottom(5, 0),
    draw_bottom_corner_right, nl.

draw_column_coordinates :-
    ansi_format([bold, fg(white)], '│   │ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │', []), nl.

draw_separator(Column) :-
    draw_left,
    \+draw_line(Column, 0),
    draw_right, nl.

draw_line_coordinate(Row) :-
    ansi_format([bold, fg(white)],'│ ', []),
    ansi_format([bold, fg(white)], Row, []),
    ansi_format([bold, fg(white)],' │', []).

draw_blank :-
    ansi_format([bold, bg(black)],'   ', []),
    ansi_format([bold, fg(white)],'│', []).

draw_piece(Char, Color, Opposite) :-
    ansi_format([bold, hbg(Opposite), fg(Color)],' ', []),
    ansi_format([bold, hbg(Opposite), fg(Color)], Char, []),
    ansi_format([bold, hbg(Opposite), fg(Color)],' ', []),
    ansi_format([bold, fg(white)],'│', []).

% Auxiliary rules
draw_line(Length, X) :-
    X < Length - 1, ansi_format([bold, fg(white)],'───┼', []),
    N is X + 1, draw_line(Length, N).

draw_top(Length, X) :-
    X < Length, ansi_format([bold,  fg(white)],'┬───', []),
    N is X + 1, draw_top(Length, N).

draw_bottom(Length, X) :-
    X < Length, ansi_format([bold, fg(white)],'┴───', []),
    N is X + 1, draw_bottom(Length, N).


draw_top_corner_right :-
    ansi_format([bold, fg(white)],'┬───┐', []).

draw_top_corner_left :-
    ansi_format([bold, fg(white)],'┌───', []).

draw_right :-
    ansi_format([bold, fg(white)],'───┤', []).

draw_left :-
    ansi_format([bold, fg(white)],'├───┼', []).

draw_bottom_corner_right :-
    ansi_format([bold, fg(white)],'┴───┘', []).

draw_bottom_corner_left :-
    ansi_format([bold, fg(white)],'└───', []).