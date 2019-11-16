:- ensure_loaded('logic.pl').

% This module makes all actual drawing in the console.
% This way drawing details are hidden from the view clauses.

% Font used for numbers. Useful to draw coordinates on the screen.
number_font(0, '𝟘').
number_font(1, '𝟙').
number_font(2, '𝟚').
number_font(3, '𝟛').
number_font(4, '𝟜').
number_font(5, '𝟝').
number_font(6, '𝟞').
number_font(7, '𝟟').
number_font(8, '𝟠').
number_font(9, '𝟡').

% ====================================================================================
% High level rules used by display.
% Draw predicates use green and magenta colors to make our game more appealing to the eye ;)

% Draws the title that appears over the board.
draw_title :-
    ansi_format([fg(green)], '          ⚜ ECHEK ⚜', []).

% Draws the upper limit of the board.
draw_top_boundary :-
    draw_top_corner_left,
    \+draw_top(5, 0),
    draw_top_corner_right, nl.

% Draws the bottom limit of the board.
draw_bottom_boundary :-
    draw_bottom_corner_left,
    \+draw_bottom(5, 0),
    draw_bottom_corner_right, nl.

% Draws the line with all the column coordinates.
draw_column_coordinates :-
    ansi_format([bold, fg(magenta)],  '│   │ ', []), 
    ansi_format([fg(green)],'𝟘 ', []),
    ansi_format([bold, fg(magenta)],  '│ ', []),
    ansi_format([fg(green)],'𝟙 ', []),
    ansi_format([bold, fg(magenta)],  '│ ', []),
    ansi_format([fg(green)],'𝟚 ', []),
    ansi_format([bold, fg(magenta)],  '│ ', []),
    ansi_format([fg(green)],'𝟛 ', []),
    ansi_format([bold, fg(magenta)],  '│ ', []),
    ansi_format([fg(green)],'𝟜 ', []),
    ansi_format([bold, fg(magenta)],  '│ ', []),
    ansi_format([fg(green)],'𝟝 ', []),
    ansi_format([bold, fg(magenta)],  '│' , []), nl.

% Draws a separator.
% A separator is the line which separates two consecutive rows.
draw_separator(Column) :-
    draw_left,
    \+draw_line(Column, 0),
    draw_right, nl.

% Draws the coordinate at each row's left side.
draw_line_coordinate(Row) :-
    number_font(Row, N),
    ansi_format([bold, fg(magenta)],'│ ', []),
    ansi_format([fg(green)], '~w ', [N]),
    ansi_format([bold, fg(magenta)],'│', []).

% Draws a properly sized blank space.
draw_blank :-
    ansi_format([bold, bg(black)],'   ', []),
    ansi_format([bold, fg(magenta)],'│', []).

% Draws a piece represented by the argument Char.
% The piece is drawn according to its color, but over an opposite color background.
draw_piece(Char, Color, Opposite) :-
    ansi_format([bold, hbg(Opposite), fg(Color)],' ', []),
    ansi_format([bold, hbg(Opposite), fg(Color)], Char, []),
    ansi_format([bold, hbg(Opposite), fg(Color)],' ', []),
    ansi_format([bold, fg(magenta)],'│', []).

% ====================================================================================
% Auxiliary predicates to make the code more readable.

% Draws a line with the below character.
% Calls itself recursively until it reaches the desired length.
draw_line(Length, X) :-
    X < Length - 1, ansi_format([bold, fg(magenta)],'───┼', []),
    N is X + 1, draw_line(Length, N).

% Draws the top line with the below character. This make the board top limit a straight line with no spikes.
% Calls itself recursively until it reaches the desired length.
draw_top(Length, X) :-
    X < Length, ansi_format([bold,  fg(magenta)],'┬───', []),
    N is X + 1, draw_top(Length, N).

% Draws the bottom line with the below character. This make the board bottom limit a straight line with no spikes.
% Calls itself recursively until it reaches the desired length.
draw_bottom(Length, X) :-
    X < Length, ansi_format([bold, fg(magenta)],'┴───', []),
    N is X + 1, draw_bottom(Length, N).

% The following predicates draw special parts of the border specifically.
% Their names indicate which part they refer to.

draw_top_corner_right :-
    ansi_format([bold, fg(magenta)],'┬───┐', []).

draw_top_corner_left :-
    ansi_format([bold, fg(magenta)],'┌───', []).

draw_right :-
    ansi_format([bold, fg(magenta)],'───┤', []).

draw_left :-
    ansi_format([bold, fg(magenta)],'├───┼', []).

draw_bottom_corner_right :-
    ansi_format([bold, fg(magenta)],'┴───┘', []).

draw_bottom_corner_left :-
    ansi_format([bold, hfg(magenta)],'└───', []).

% Draws a constantly sized white space for identation.
draw_space :-
    write('            ').
