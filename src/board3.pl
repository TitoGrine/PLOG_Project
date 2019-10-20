:- dynamic(cell/4).

% Example board where the black kigng has been surrounded

cell(1, 2, white, king).
cell(3, 3, black, king).
cell(3, 4, white, pawn).
cell(3, 2, black, pawn).
cell(2, 1, white, knight).
cell(4, 2, black, knight).
cell(2, 3, white, queen).
cell(4, 3, white, bishop).
cell(3, 1, black, bishop).
cell(1, 3, white, rook).
cell(4, 4, black, rook).