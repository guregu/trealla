fib(0,0) :- !.
fib(A,B) :-
    fib_(A,0,1,B).

fib_(1,_,A,A).
fib_(A,B,C,D) :-
    A > 1,
    E is A-1,
    F is B+C,
    fib_(E,C,F,D).

test :-
    fib(10,F),
    F =:= 89,
    write('fib(30)='), write(F), write(' PASSED'), nl.

test0 :-
    fib(30,F),
    F =:= 1346269,
    write('fib(30)='), write(F), write(' PASSED'), nl.

test1 :-
    fib(35,F),
    F =:= 14930352,
    write('fib(35)='), write(F), write(' PASSED'), nl.

test2 :-
    fib(40,F),
    F =:= 165580141,
    write('fib(40)='), write(F), write(' PASSED'), nl.
