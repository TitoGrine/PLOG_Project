# PLOG_Project

#### To Do:
 - [x] Have the game alternate turns between the players
 - [x] Recenter board when pieces are on the edge
 - [x] Check for virtual limits (horizontal & vertical)
 - [x] Check for valid placement
 - [X] Check for valid movement of all pieces
 - [x] Check if queen is surrounded and remove it from the game (no longer playable)
 - [x] Check if king is surrounded and end the game with victory message
 - [X] When a pawn is placed, allow the player to move it after (in the same turn)
 - [X] When a bishop is placed, make the player to remove a piece from the board (in the same turn) *imp. use delete_from_database*
 - [X] When a player chooses the rook (movement), be able to swap positions with the player's king once per game
 - [X] Make the main menu to choose game mode
 - [X] Implement game mode with random AI
 - [x] Implement game mode with "smart" AI (at least one level of decision)
 - [x] Remove all warnings as possible
 - [ ] Make function for input checking (string and coordinates)
 - [ ] Comment code
 - [ ] Organize functions

#### Testing:
 - [ ] Placement on board
 - [ ] Piece movements
 - [ ] Virtual limits
 - [ ] Game edge cases
 - [ ] Just try to find bugs x)

#### Bugs:
 - [X] As long as there is no piece alone the move will be valid; when it creates a disconnection between 2 groups of pieces it must be invalid.
 - [X] Trying to move rook over king did not work, rook at (3,3), king at (3,2), rook should move to (3,1).
 - [x] Breaking the connection with the queen did not cause an error when the game ended; screenshot was sent to Messenger
 - [x] Should there be a draw message when both kings are surrounded in the same move?
