:- use_module(library(charsio)).
:- op(300,xfx,\\).

main :-
	read_from_chars("[,@].", T).

:- initialization(main).
