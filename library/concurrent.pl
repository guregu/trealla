:- module(concurrent, [
	future/3,
	future_all/2,
	future_any/2,
	future_cancel/1,
	future_done/1,
	await/2
	]).

:- use_module(library(apply)).
:- dynamic('$concurrent_count'/1).
:- dynamic('$future'/1).

'$concurrent_count'(0).

future(Template, Goal, F) :-
	retract('$concurrent_count'(N)),
	N1 is N + 1,
	assertz('$concurrent_count'(N1)),
	F = '$future'(N),
	assertz(F),
	Task0 = ((Goal -> (retract(F), send([F-Template])) ; (retract(F), fail))),
	copy_term(Task0, Task),
	write_term_to_atom(A, Task, [quoted(true)]),
	task(callgoal_, A, F).

:- meta_predicate(future(-,0,?)).
:- help(future(+term,+callable,?list), [iso(false)]).

future_all(Fs, all(Fs)).
future_any(Fs, any(Fs)).

:- help(future_all(+list,-term), [iso(false)]).
:- help(future_any(+list,-term), [iso(false)]).
:- help(future_any(+list,-term), [iso(false)]).

await(all(Fs), Templates) :-
	!,
	findall([F-Template], (
		member(F, Fs),
		wait,
		recv([F-Template])
		), Msgs),
	msort(Msgs, Msgs1),
	strip_prefix_(Msgs1, [], Templates0),
	Templates = Templates0.

await(any(Fs), Template) :-
	!,
	await,
	recv([F-Template0]),
	member(F, Fs),
	!,
	Template = Template0.

await(F, Template) :-
	repeat,
		wait,
		recv([F-Template]),
		!.

future_cancel(all(Fs)) :-
	Fs = [F|Rest],
	future_cancel(F),
	future_cancel(Rest).

future_cancel(any(Fs)) :-
	Fs = [F|Rest],
	future_cancel(F)
	-> true
	; future_cancel(any(Rest)).

future_cancel([]).
future_cancel('$future'(N)) :-
	'$cancel_future'(N).

future_done(all(Fs)) :-
	Fs = [F|Rest],
	future_done(F),
	future_done(Rest).

future_done(any(Fs)) :-
	Fs = [F|Rest],
	future_done(F)
	-> true
	; future_done(any(Rest)).

future_done([]).
future_done(F) :-
	\+ clause(F, _).

:- help(await(+term,?term), [iso(false)]).

strip_prefix_([], L0, L) :- reverse(L0, L).
strip_prefix_([[_-V]|Rest], Init, L) :-
	strip_prefix_(Rest, [V|Init], L).

% NOTE: going via an atom through callgoal/1 is to get around a bug to
% do with passing variables in task/1. Maybe it will get fixed one day.

callgoal_(A, '$future'(N)) :-
	read_term_from_atom(A, T, []),
	'$set_future'(N),
	T.
