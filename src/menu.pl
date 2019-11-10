:- include('game.pl').

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
    write('============================================='), nl.

main_menu_options :-
    write(' Welcome to Echek! Please choose an option: '), nl,
    write('      1 - Player vs Player'), nl,
    write('      2 - Player vs Machine'), nl,
    write('      3 - Machine vs Machine'), nl,
    write('      4 - Exit'), nl.

execute_menu_input(1) :-
    !, start_game.
execute_menu_input(2) :- !.
    %!, Input =:= 2, start_game.
execute_menu_input(3) :- !.
    %!, Input =:= 3, start_game.
execute_menu_input(4) :- !.
execute_menu_input(_) :-
    write('Invalid Input!'), false.

echek:-
    repeat,
        main_menu_header,
        main_menu_options,
        read(Input),
        execute_menu_input(Input),
        !, Input =:= 4.