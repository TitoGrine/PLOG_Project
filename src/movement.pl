:- ensure_loaded('logic.pl').

% This predicate is responsible for moving a piece.
% It askes the user where to move the piece to and then checks if it is a valid move and does not break any rule.
move(Player, Piece) :-
    cell(Xinit, Yinit, Player, Piece),
    !,
    repeat,
    read_coords(Xdest, Ydest, Player, Piece, move),
    (cancel(Xdest, Ydest);
    (castling_move(Player, Piece, Xdest, Ydest, Xinit, Yinit));
    (valid_cell(Ydest, Xdest),
    valid_move(Piece, Xinit, Yinit, Xdest, Ydest, Player),
    change_database(Xdest, Ydest, Player, Piece),
    ((check_virtual_limits,
      check_connections); (change_database(Xinit, Yinit, Player, Piece), false)))),!,
    \+ cancel(Xdest, Ydest).

% This predicate moves a piece for the AI.
% This one does not need to ask for input or to check if it is a valid move because the AI only generates valid moves.
move_ai(Player, Piece, Xdest, Ydest) :-
    cell(Xinit, Yinit, Player, Piece),!,
    (castling_move(Player, Piece, Xdest, Ydest, Xinit, Yinit);
     change_database(Xdest, Ydest, Player, Piece)).

% ====================================================================================

% Checks if a movement is within the rules. Used to generate AI movements.
check_movement(Player, Piece, Xdest, Ydest):-
    cell(Xinit, Yinit, Player, Piece),!,
    (check_castling(Player, Piece, Xdest, Ydest);           % Checks if the movement is a valid castling move.
    (valid_cell(Ydest, Xdest),                              % Ensures the target cell is within limits and has no piece already.
    valid_move(Piece, Xinit, Yinit, Xdest, Ydest, Player),  % Checks if the movement is valid for the respective piece.
    change_database(Xdest, Ydest, Player, Piece),!,         % Temporarily moves the piece to the desired cell to do further verifications.
    ((check_virtual_limits, check_connections,              % Ensures the movement does not disconnect all the pieces and that it does not break the virtual limits.
    change_database(Xinit, Yinit, Player, Piece));          % The piece must be placed on its original cell because this predicate does not move a piece.
    \+ change_database(Xinit, Yinit, Player, Piece)))),!.

% Checks if a move is a valid castling move.
% This forces the castling to be available to the player.
check_castling(Player, rook, X, Y) :-
    castling_available(Player),
    cell(X, Y, Player, king).

% Executes a castling move if it is valid.
castling_move(Player, rook, X, Y, RookX, RookY) :-
    castling_available(Player),!,        % Checks if the player has not used castling already.
    cell(X, Y, Player, king),
    change_database(RookX, RookY, Player, king), % Swapping king and rook's positions.
    change_database(X, Y, Player, rook),
    castling_done(Player).              % From now on the player can no longer do a castling again.

% ====================================================================================

% When the pawn is placed, this predicate handles its special ability.
% This basically allows the player to move the pawn, but does not let them cancel the play.
special_ability_on_placement(pawn, Player) :-
    cell(Xinit, Yinit, Player, pawn),
    display_board,
    !,
    repeat,
    change_database(Xinit, Yinit, Player, pawn),
    read_coords_no_cancel(Xdest, Ydest, Player, pawn, move),
    ((Xinit == Xdest, Yinit == Ydest); % This condition allows the user to choose not to move the pawn immediatly.
    (valid_cell(Ydest, Xdest),
    valid_move(pawn, Xinit, Yinit, Xdest, Ydest, Player),
    change_database(Xdest, Ydest, Player, pawn),
    ((check_virtual_limits,
      check_connections); (change_database(Xinit, Yinit, Player, pawn), false)))).

% When the bishop is placed, it can remove a piece from the board.
% This predicate checks if the coordinates the user inputs are a valid piece to remove and then removes teh piece.
special_ability_on_placement(bishop, Player) :-
    only_kings_on_board(Player);
    display_board,
    (!,
    repeat,
        read_player_piece(X, Y),
        valid_removable_cell(X, Y, Player, TargetPlayer, Piece),
        delete_from_database(X, Y, TargetPlayer, Piece),
        (check_connections; (change_database(X, Y, TargetPlayer, Piece), false))).

% The predicate special_ability_on_placement always return true to avoid unwanted backtracking.
special_ability_on_placement(_, _).

% ====================================================================================

% Applies the pawn special ability (moves the pawn) for AI (it has no check for validity).
special_ability_on_placement_ai(pawn, Player, [Xdest, Ydest]) :-
    display_board,
    change_database(Xdest, Ydest, Player, pawn).

% Removes the piece chosen by the AI from the board.
% The only_kings_on_board verification is just done here to make the creation of valid removables simpler.
special_ability_on_placement_ai(bishop, Player, [X, Y]) :-
    only_kings_on_board(Player);
    display_board,
    delete_from_database(X, Y, _, _).

% This predicate always returns true as does its player counterpart.
special_ability_on_placement_ai(_, _).

% ====================================================================================

% Checks if a removal is possible, never changing the database.
% Used to generate a list of possible removals for AI.
possible_removable(Player, TargetPlayer, Piece, X ,Y) :-
    valid_removable_cell(X, Y, Player, TargetPlayer, Piece),
    delete_from_database(X, Y, TargetPlayer, Piece),!,
    ((check_connections, change_database(X, Y, TargetPlayer, Piece)) ; \+change_database(X, Y, TargetPlayer, Piece)),!.

% Checks if the given coordinates do not correspond to a king or the bishop who has trigerred its special ability.
valid_removable_cell(X, Y, Player, TargetPlayer, Piece) :-
    cell(X, Y, TargetPlayer, Piece),
    Piece \== king,
    \+ (Piece == bishop, TargetPlayer == Player).

% Checks if there are only kings on the board (apart from the newly placed bishop).
% If so, the bishop can not remove any piece and is played anyway.
only_kings_on_board(Player) :-
    \+((cell(_,_,Color,Piece),
        \+(Piece == king; (Piece == bishop, Player == Color)))).

% ====================================================================================

% Checks if a cell is a valid target for movement.
valid_cell(R, C) :-
    within_limits(R, C),!,              % Ensures the position is within the board limits.
    \+cell(C, R, _, _).                 % Checks if there is already a piece in that cell.

% Checks if all the pieces are connected to each other using a flood fill algorithm.
% All the pieces that are visited are marked as such and if there is any left unvisited then there is a disconnection on the board.
% Before exiting, this predicate marks all pieces unvisited again.
check_connections :-
    cell(C, R, _, _), % The flood fill is launched by the first cell registered in the database.
    connected_board([[R, C]]),!,
    ((all_visited, clean_visited) ; (\+ clean_visited)).

% The flood fill algorithm.
connected_board([]).
connected_board([[] | Others]) :-
    connected_board(Others).
connected_board([[R, C] | Others]) :-
    ((check_visited(R, C), Next = Others) ;  % If the piece has already been visited, proceeds.
    (add_visited(R, C),                      % Otherwise marks it as visited and checks for nearby pieces.
     PR is R - 1, NR is R + 1,
     PC is C - 1, NC is C + 1,
     !,
     (((cell(PC, PR, _, _), Cell1 = [PR, PC]) ; Cell1 = []),
      ((cell( C, PR, _, _), Cell2 = [PR,  C]) ; Cell2 = []),
      ((cell(NC, PR, _, _), Cell3 = [PR, NC]) ; Cell3 = []),
      ((cell(PC,  R, _, _), Cell4 = [ R, PC]) ; Cell4 = []),
      ((cell(NC,  R, _, _), Cell5 = [ R, NC]) ; Cell5 = []),
      ((cell(PC, NR, _, _), Cell6 = [NR, PC]) ; Cell6 = []),
      ((cell( C, NR, _, _), Cell7 = [NR,  C]) ; Cell7 = []),
      ((cell(NC, NR, _, _), Cell8 = [NR, NC]) ; Cell8 = [])),
     append(Others, [Cell1, Cell2, Cell3, Cell4, Cell5, Cell6, Cell7, Cell8], Next))), % All pieces nearby are added to the buffer.
    connected_board(Next). % Tail recursion. Only ends when the buffer is empty.
    
% Checks if all pieces have been visited (makes use of backtracking).
all_visited :-
    \+((cell(C, R, _, _),
        \+check_visited(R, C))).

% Checks if a piece is inside the 4x4 virtual board.
% Makes use of backtracking to check if each piece individually breaks the rule.
check_virtual_limits :-
    \+((cell(C, R, _, _),
       \+ within_virtual_limits(R, C))).

% ====================================================================================

% The below predicates represent the piece's own movement rules (just like chess).

% Pawn moves to side adjacent cells.
valid_move(pawn, Xinit, Yinit, Xdest, Ydest, _) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX))).

% King moves in any direction, but just 1 cell at a time.
valid_move(king, Xinit, Yinit, Xdest, Ydest, _) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    ((Xinit = Xdest, (Ydest = NextY; Ydest = PrevY));
    (Yinit = Ydest, (Xdest = NextX; Xdest = PrevX));
    (Xdest = NextX, (Ydest = NextY; Ydest = PrevY));
    (Xdest = PrevX, (Ydest = NextY; Ydest = PrevY))).

% Rook moves any amount of cells in either horizontal or vertical direction.
valid_move(rook, Xinit, Yinit, Xdest, Ydest, Player) :-
    !,
    (Xinit = Xdest; Yinit = Ydest),!,
    straight_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player). % Checks if the rook goes over an enemy piece.

% Bishop moves diagonally, any number of cells.
valid_move(bishop, Xinit, Yinit, Xdest, Ydest, Player) :-
    !,
    Xdest \= Xinit,
    Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1),!,
    diagonal_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player). % Checks if the bishop goes a enemy piece on its way to the destination.

% Queen moves diagonally and horizontally/vertically, any number of cells.
valid_move(queen, Xinit, Yinit, Xdest, Ydest, Player) :-
    !,
    (Xinit = Xdest; Yinit = Ydest;
    (Xdest \= Xinit, Inclination is (Ydest - Yinit) / (Xdest - Xinit),
    (Inclination = -1; Inclination = 1))),!,
    diagonal_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player),!, % The same "over enemy" verifications are made for the queen.
    straight_cross_enemy(Yinit, Xinit, Ydest, Xdest, Player).

% Knight moves in 'L' shape. This piece can go over enemy pieces.
valid_move(knight, Xinit, Yinit, Xdest, Ydest, _) :-
    !,
    NextY is Yinit + 1, PrevY is Yinit - 1,
    NextX is Xinit + 1, PrevX is Xinit - 1,
    DoubleNextY is Yinit + 2, DoublePrevY is Yinit - 2,
    DoubleNextX is Xinit + 2, DoublePrevX is Xinit - 2,
    (((Xdest = DoubleNextX; Xdest = DoublePrevX), (Ydest = NextY; Ydest = PrevY));
    ((Ydest = DoubleNextY; Ydest = DoublePrevY), (Xdest = NextX; Xdest = PrevX))).
