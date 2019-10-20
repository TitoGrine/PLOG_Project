:- dynamic(cell/4).

% Example board where all pieces have been placed
% This also exemplifies the limits of the board

cell(1, 2, white, king).
cell(3, 3, black, king).
cell(3, 4, white, pawn).
cell(2, 2, black, pawn).
cell(2, 1, white, knight).
cell(4, 2, black, knight).
cell(2, 3, white, queen).
cell(1, 3, black, queen).
cell(3, 2, white, bishop).
cell(3, 1, black, bishop).
cell(1, 4, white, rook).
cell(4, 4, black, rook).