
% 3x3 Puzzles with more than one soltuion.

puzzle3x3_1([[3, 3, _],
             [_, 2, _],
             [3, 3, 3]]).

puzzle3x3_2([[_, 3, _],
             [_, _, 3],
             [_, _, _]]).

% 4x4 Puzzles with only one solution.

puzzle4x4_1([[_, _, _, _],
             [_, _, 2, _],
             [_, _, _, 2],
             [_, _, _, _]]).

puzzle4x4_2([[_, _, _, _],
             [1, _, _, _],
             [_, _, _, _],
             [1, _, _, _]]).

puzzle4x4_3([[_, 1, _, _],
             [_, _, _, 2],
             [_, _, _, _],
             [_, _, _, _]]).


% 5x5 Puzzles with only one solution.

puzzle5x5_1([[_, _, 1, _, 1],
             [_, _, _, _, _],
             [_, _, _, _, _],
             [1, _, _, _, 1],
             [_, _, _, _, _]]).

puzzle5x5_2([[_, _, _, _, 2],
             [_, 2, _, _, _],
             [2, _, _, _, _],
             [_, _, _, _, 2],
             [_, _, _, _, _]]).

puzzle5x5_3([[_, _, 3, 3, _],
             [_, _, _, _, _],
             [_, 3, 3, _, _],
             [_, 3, _, _, _],
             [_, _, _, _, _]]).

puzzle5x5_4([[_, _, _, _, 1],
             [_, _, _, 1, _],
             [2, 3, _, _, _],
             [_, _, _, _, _],
             [_, _, _, _, _]]).

puzzle5x5_5([[_, _, 1, _, _],
             [_, _, _, 2, _],
             [3, _, _, _, _],
             [_, 1, _, _, _],
             [_, _, _, _, _]]).

puzzle5x5_6([[2, _, _, _, 1],
             [_, _, _, _, _],
             [_, _, 3, _, _],
             [_, 3, _, _, _],
             [_, _, _, _, _]]).

puzzle5x5_7([[_, _, _, _, _],
             [_, _, _, _, _],
             [_, _, 3, _, 1],
             [2, _, _, _, _],
             [_, _, _, 2, _]]).

puzzle5x5_8([[2, _, _, _, 1],
             [_, _, _, _, _],
             [_, _, 3, _, _],
             [_, _, _, _, _],
             [_, 2, _, _, _]]).

puzzle5x5_9([[_, _, _, _, _],
             [_, _, _, 1, _],
             [_, _, 3, _, 3],
             [_, _, _, _, _],
             [_, _, _, _, 2]]).

puzzle5x5_10([[_, _, _, _, 3],
              [_, 2, _, _, _],
              [_, _, _, _, _],
              [1, _, _, _, _],
              [_, _, _, _, 1]]).


% 6x6 Puzzles with only one solution.

puzzle6x6_1([[_, _, _, 2, _, _],
             [_, _, 2, _, _, _],
             [_, _, _, _, _, _],
             [_, _, _, _, 1, _],
             [_, _, _, _, _, 3],
             [_, _, _, _, 3, _]]).

puzzle6x6_2([[_, _, _, 2, _, _],
             [1, _, _, _, _, _],
             [_, _, _, _, _, 3],
             [_, _, _, _, _, _],
             [_, _, _, 1, _, 1],
             [_, _, _, _, _, _]]).

puzzle6x6_3([[_, _, _, _, _, _],
             [_, _, _, _, _, _],
             [_, _, _, _, 3, _],
             [3, _, _, _, _, _],
             [_, _, 2, _, 2, 1],
             [_, _, _, _, _, _]]).

puzzle6x6_4([[_, _, _, _, _, _],
             [_, _, _, _, _, _],
             [_, _, _, 3, _, 1],
             [_, _, 3, _, _, _],
             [_, _, _, _, 2, _],
             [_, _, _, 1, _, _]]).            

puzzle6x6_5([[_, _, _, _, _, 2],
             [_, 2, _, _, _, _],
             [_, _, _, _, _, _],
             [_, 3, _, _, _, _],
             [1, _, _, _, _, _],
             [_, _, _, _, 1, _]]).

puzzle6x6_6([[_, _, 2, _, 1, 3],
             [_, _, _, _, _, _],
             [_, _, _, _, _, _],
             [_, _, _, _, 2, _],
             [_, _, _, _, _, _],
             [2, _, _, _, _, _]]).

puzzle6x6_7([[_, _, _, 3, _, _],
             [_, _, _, _, _, _],
             [_, _, _, _, _, _],
             [_, _, 1, _, _, _],
             [_, _, _, _, 2, _],
             [_, _, 1, _, 1, _]]).

puzzle6x6_8([[_, _, _, _, 1, _],
             [_, _, _, _, _, _],
             [_, _, _, _, 1, _],
             [_, _, _, _, _, _],
             [1, _, 1, _, _, _],
             [_, _, _, _, 1, _]]).

puzzle6x6_9([[2, _, _, _, _, _],
             [_, _, _, _, _, _],
             [_, _, _, _, _, _],
             [2, _, _, _, 2, _],
             [_, _, 2, _, _, _],
             [2, _, _, _, _, 2]]).

puzzle6x6_10([[_, _, 3, _, _, 3],
              [3, _, _, _, _, _],
              [_, _, _, 3, _, _],
              [_, _, _, _, _, 3],
              [_, _, _, 3, _, _],
              [_, _, _, 3, _, _]]).

% 15x15 puzzle with multiple solutions.

puzzle15x15_1([[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, 1, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, 3, 3, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, 2, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, 1, _],
               [_, _, _, _, 2, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [2, _, _, _, _, _, _, _, _, 3, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]).

puzzle15x15_2([[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, 1, _, _, _, _, _, _, _, 3, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, 2, 3, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, 2, _],
               [_, _, _, _, _, _, _, _, 2, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, 3, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, 3, _, _, _, _, _, _, _, _, 3, 1, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
               [2, _, _, _, _, _, _, _, _, 3, _, _, _, _, _],
               [_, _, _, _, _, _, _, _, _, _, _, _, _, 2, _]]).
 