:- use_module(library(clpfd)).

my_geost(N) :-
    Size is N * N,
    Points is Size * 2,
    domain([X1,X2,X3,X4,X5,X6,X7,X8,X9,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9], 1, Size),
    domain([S1,S2,S3,S4,S5,S6,S7,S8,S9], 1, 3),
    geost([object(1,S1,[X1,Y1]), object(2,S2,[X2,Y2]), object(3,S3,[X3,Y3]),
           object(4,S4,[X4,Y4]), object(5,S5,[X5,Y5]), object(6,S6,[X6,Y6]),
           object(7,S7,[X7,Y7]), object(8,S8,[X8,Y8]), object(9,S9,[X9,Y9])],
                   [sbox(1, [0,0], [1,1]),
                    sbox(2, [0,0], [1,1]),
                    sbox(3, [0,0], [1,1])],
            [],
            [(origin(O1,S1,D) ---> O1^x(D)+S1^t(D))),
          
            (end(O1,S1,D) ---> O1^x(D)+S1^t(D)+S1^l(D)),
          
            (overlap(O1,S1,O2,S2,D) --->
                end(O1,S1,D) #> origin(O2,S2,D) #/\
                end(O2,S2,D) #> origin(O1,S1,D)),
          
            (abut(O1,O2) --->
                forall(S1,sboxes([O1^sid]),
                    forall(S2,sboxes([O2^sid]),
                        ((origin(O1,S1,1) #= end(O2,S2,1) #\/
                          origin(O2,S2,1) #= end(O1,S1,1)) #/\
                         overlap(O1,S1,O2,S2,2)) #\/
                        ((origin(O1,S1,2) #= end(O2,S2,2) #\/
                          origin(O2,S2,2) #= end(O1,S1,2)) #/\
                         overlap(O1,S1,O2,S2,1))))),
          
            (forall(O1,objects([3]),
                forall(O2,objects([7]), abut(O1,O2))))]),
    labeling([], [X1,X2,X3,X4,X5,X6,X7,X8,X9,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,S1,S2,S3,S4,S5,S6,S7,S8,S9]),
    write('Object 1, Sid = '), write(S1), write('; coordinates = '), write([X1, Y1]), nl,
    write('Object 2, Sid = '), write(S2), write('; coordinates = '), write([X2, Y2]), nl,
    write('Object 3, Sid = '), write(S3), write('; coordinates = '), write([X3, Y3]), nl,
    write('Object 4, Sid = '), write(S4), write('; coordinates = '), write([X4, Y4]), nl,
    write('Object 5, Sid = '), write(S5), write('; coordinates = '), write([X5, Y5]), nl,
    write('Object 6, Sid = '), write(S6), write('; coordinates = '), write([X6, Y6]), nl,
    write('Object 7, Sid = '), write(S7), write('; coordinates = '), write([X7, Y7]), nl,
    write('Object 8, Sid = '), write(S8), write('; coordinates = '), write([X8, Y8]), nl,
    write('Object 9, Sid = '), write(S9), write('; coordinates = '), write([X9, Y9]), nl.