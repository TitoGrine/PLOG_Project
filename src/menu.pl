:- ensure_loaded('game.pl').

main_menu_header :-
    write('============================================='), nl,
    write(' _______  _______           _______  _       '), nl,
    write('(  ____ \\(  ____ \\|\\     /|(  ____ \\| \\    /\\'), nl,
    write('| (    \\/| (    \\/| )   ( || (    \\/|  \\  / /'), nl,
    write('| (__    | |      | (___) || (__    |  (_/ / '), nl,
    write('|  __)   | |      |  ___  ||  __)   |   _ (  '), nl,
    write('| (      | |      | (   ) || (      |  ( \\ \\ '), nl,
    write('| (____/\\| (____/\\| )   ( || (____/\\|  /  \\ \\'), nl,
    write('(_______/(_______/|/     \\|(_______/|_/    \\/'),nl,
    write('                                             '), nl,
    write('============================================='), nl,
    !.

main_menu_options :-
    write(' Welcome to Echek! Please choose an option: '), nl,
    write('      1 - Player vs Player'), nl,
    write('      2 - Player vs Machine'), nl,
    write('      3 - Machine vs Machine'), nl,
    write('      4 - Instructions'), nl,
    write('      5 - Exit'), nl,
    !.

instructions_menu :-
    write('============================================='), nl,
    write('                INSTRUCTIONS                 '), nl,
    write('============================================='), nl,
    write(' You can check the game\'s rules at https://en.tipeee.com/leandreproust. '), nl,
    write(' Here are some instructions to play the game: '), nl,
    write(' 1. On your turn you start by choosing a piece; '), nl,
    write(' 2. If it is already on the board, then you will move it; '), nl,
    write(' 3. Otherwise, you will place it; '), nl,
    write(' 4. In any case, you must input the coordinates on the format "X,Y"; '), nl,
    write(' 5. To use the rook\'s special castling move, choose the rook then input your king\'s coordinates; '), nl,
    write(' 6. To use the bishop\'s removing ability, just input the coordinates of the piece you wanna see removed from the board. '), nl, nl,
    write(' That\'s it! When you are ready to go back just input "menu." '),
    repeat, read(Input), Input == menu, !.

execute_menu_input(1) :-
    start_game(player, player), !.
execute_menu_input(2) :-
    start_game(player, ai, random), !.
execute_menu_input(3) :-
    start_game(ai, ai, random, random), !.
execute_menu_input(4) :-
    instructions_menu, !.
execute_menu_input(5) :- !.
    
play:-
    repeat,
        nl,
        main_menu_header,nl,
        main_menu_options,
        read(Input),
        execute_menu_input(Input),
        Input =:= 5.