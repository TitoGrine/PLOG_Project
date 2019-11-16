:- ensure_loaded('game.pl').

% Displays header for the main menu                                                   
main_menu_header :-
    ansi_format([fg(magenta)], '      `7MM"""YMM  ', []), ansi_format([fg(green)],  '  .g8"""bgd  ', []), ansi_format([fg(magenta)],  '`7MMF\'  `7MMF\'', []), ansi_format([fg(green)],  ' `7MM"""YMM ', []), ansi_format([fg(magenta)],  '    .g8"""bgd ', []), ansi_format([fg(green)],  '`7MMF\' `YMM\'', []), nl,   
    ansi_format([fg(magenta)], '        MM    `7  ', []), ansi_format([fg(green)],  '.dP\'     `M ', []), ansi_format([fg(magenta)],  '   MM      MM   ', []), ansi_format([fg(green)],  '  MM    `7  ', []), ansi_format([fg(magenta)],  ' .dP\'     `M ', []), ansi_format([fg(green)],  '  MM   .M\'', []), nl,   
    ansi_format([fg(magenta)], '        MM   d    ', []), ansi_format([fg(green)],  'dM\'       ` ', []), ansi_format([fg(magenta)],  '   MM      MM   ', []), ansi_format([fg(green)],  '  MM   d    ', []), ansi_format([fg(magenta)],  ' dM\'       ` ', []), ansi_format([fg(green)],  '  MM .d"', []), nl,  
    ansi_format([fg(magenta)], '        MMmmMM    ', []), ansi_format([fg(green)],  'MM           ', []), ansi_format([fg(magenta)],  '  MMmmmmmmMM    ', []), ansi_format([fg(green)],  ' MMmmMM     ', []), ansi_format([fg(magenta)],  'MM            ', []), ansi_format([fg(green)],  'MMMMM. ', []), nl,    
    ansi_format([fg(magenta)], '        MM   Y  , ', []), ansi_format([fg(green)],  'MM.          ', []), ansi_format([fg(magenta)],  '  MM      MM    ', []), ansi_format([fg(green)],  ' MM   Y  ,  ', []), ansi_format([fg(magenta)],  'MM.           ', []), ansi_format([fg(green)],  'MM  VMA', []), nl,   
    ansi_format([fg(magenta)], '        MM     ,M ', []), ansi_format([fg(green)],  '`Mb.     ,\' ', []), ansi_format([fg(magenta)],  '   MM      MM   ', []), ansi_format([fg(green)],  '  MM     ,M ', []), ansi_format([fg(magenta)],  ' `Mb.     ,\' ', []), ansi_format([fg(green)],  '  MM   `MM.', []), nl,   
    ansi_format([fg(magenta)], '      .JMMmmmmMMM ', []), ansi_format([fg(green)],  '  `"bmmmd\'  ', []), ansi_format([fg(magenta)],  ' .JMML.  .JMML. ', []), ansi_format([fg(green)],  '.JMMmmmmMMM ', []), ansi_format([fg(magenta)],  '   `"bmmmd\'  ', []), ansi_format([fg(green)],  '.JMML.   MMb.', []), nl,nl,
    !.

% Displays main menu options
main_menu_options :-
    ansi_format([fg(magenta)], '      Please choose one of the following options:           ', []), nl,
    ansi_format([fg(white)], '                                                               (\\=,   ', []), nl, 
    ansi_format([fg(green)],   '           1 - Player vs Player                             ', []), ansi_format([fg(white)], '  //  ',[]), ansi_format([fg(red)], '.', []), ansi_format([fg(white)],'\\  ', []), nl, 
    ansi_format([fg(white)], '                                                             (( \\_  \\ ', []), nl, 
    ansi_format([fg(green)],   '           2 - Player vs Machine                            ', []), ansi_format([fg(white)], '  ))  `\\_)', []), nl,
    ansi_format([fg(white)], '                                                             (/     \\', []), nl, 
    ansi_format([fg(green)],   '           3 - Machine vs Machine                           ', []), ansi_format([fg(white)], '  | _.-\'| ', []), nl,
    ansi_format([fg(white)], '                                                               )___(  ', []), nl, 
    ansi_format([fg(green)],   '           4 - Instructions                                 ', []), ansi_format([fg(white)], '  (=====) ', []), nl,
    ansi_format([fg(white)], '                                                              }====={ ', []), nl, 
    ansi_format([fg(green)],   '           5 - Exit                                         ', []), ansi_format([fg(white)], ' (_______)', []), nl, nl,
    
    !.

% Displays options to select the AI the user wants to play against
player_machine_menu_options :-
    ansi_format([fg(magenta)], '      Which AI do you want to play against:           ', []), nl, nl,
    ansi_format([fg(green)],   '           1 - Random AI                              ', []), nl, nl,
    ansi_format([fg(green)],   '           2 - "Smart" AI                             ', []), nl, nl,
    ansi_format([fg(green)],   '           3 - Go Back                                ', []), nl, nl,
    !.

% Displays options to select the AI's the user wants to see play against each other
machine_machine_menu_options :-
    ansi_format([fg(magenta)], '      Which AI\'s do you wanna see play against each other:       ', []), nl, nl,
    ansi_format([fg(green)],   '           1 - Random AI vs Random AI                             ', []), nl, nl, 
    ansi_format([fg(green)],   '           2 - Random AI vs "Smart" AI                            ', []), nl, nl,
    ansi_format([fg(green)],   '           3 - "Smart" AI vs "Smart" AI                           ', []), nl, nl,
    ansi_format([fg(green)],   '           4 - Go Back                                            ', []), nl, nl,
    !.

% Display some relevant instructions in order to play the game (doesn't have all the game mechanics in detail)
instructions_menu :-
    nl, nl,
    ansi_format([fg(magenta)], '                             =============================================', []), nl,
    ansi_format([fg(magenta)], '                                             INSTRUCTIONS                 ', []), nl,
    ansi_format([fg(magenta)], '                             =============================================', []), nl, nl,
    ansi_format([fg(magenta)], '  You can check the game\'s rules at ', []), ansi_format([fg(cyan)], 'https://en.tipeee.com/leandreproust', []), ansi_format([fg(magenta)], '.', []), nl, nl,
    ansi_format([fg(magenta)], '  Here are some instructions to play the game: ', []), nl, nl,
    ansi_format([fg(green)],   '     1. On your turn you start by choosing any piece of the following {pawn, knight, rook, bishop, queen, king}.', []), nl, nl,
    ansi_format([fg(green)],   '     2. If it\'s already on the board, then choose the cell you want to move the piece to.', []), nl, nl,
    ansi_format([fg(green)],   '     3. If it\'s not yet on the board, then choose the cell where you want to place it.', []), nl, nl,
    ansi_format([fg(green)],   '     4. In any case, you must input the coordinates on the format "X,Y".', []), nl, nl,
    ansi_format([fg(green)],   '     5. When you place your pawn on the board, you can move it immediatly. You just have to choose the', []), nl,
    ansi_format([fg(green)],   '        coordinates once prompted. Input the same coordinates of your pawn if you don\'t want to move it.', []),nl, nl,
    ansi_format([fg(green)],   '     6. To use the rook\'s special move, choose the rook then input your king\'s coordinates.', []), nl, nl,
    ansi_format([fg(green)],   '     7. When you place your bishop on the board, it will prompt you to choose any piece on the board ', []), nl, 
    ansi_format([fg(green)],   '        to remove (except your bishop and the kings).', []), nl, nl,
    ansi_format([fg(magenta)], '  That\'s it! When you are ready to go back just input "back." ', []),
    repeat, read(Input), Input == back, !.

% All the options to start the game when playing Player vs AI
execute_player_machine_menu_input(1) :-
    nl, start_game(player, ai, random), !.

execute_player_machine_menu_input(2) :-
    nl, start_game(player, ai, smart), !.

execute_player_machine_menu_input(3) :- !.

% ====================================================================================

% All the options to start the game when playing AI vs AI
execute_machine_machine_menu_input(1) :-
    nl, start_game(ai, ai, random, random), !.

execute_machine_machine_menu_input(2) :-
    nl, start_game(ai, ai, random, smart), !.

execute_machine_machine_menu_input(3) :-
    nl, start_game(ai, ai, smart, smart), !.

execute_machine_machine_menu_input(4) :- !.

% ====================================================================================

% All the main menu inputs, it can either call other menus when playing with AI's, call Player vs Player, instructions or exit game
execute_menu_input(1) :-
    start_game(player, player), !.

execute_menu_input(2) :-
    nl, nl,
    player_machine_menu_options,
    read(Input),
    execute_player_machine_menu_input(Input),
    Input =:= 3, !.

execute_menu_input(3) :-
    nl, nl,
    machine_machine_menu_options,
    read(Input),
    execute_machine_machine_menu_input(Input),
    Input =:= 4, !.

execute_menu_input(4) :-
    instructions_menu, !.

execute_menu_input(5) :- !.

% ====================================================================================

% Predicate that starts the hole program
play:-
    repeat,
        nl,
        main_menu_header,nl,
        main_menu_options,
        read(Input),
        execute_menu_input(Input),
        Input =:= 5.