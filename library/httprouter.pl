:- module(httprouter, [op(702, xfx, ?), op(701, xfy, &), op(1200, xfx, :->), path_term/2, path_match/2]).

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(pseudojson)).

% http://example.com/foo?a=b&c=d&e#abc
%:- op(703, xfx, ://).
:- op(702, xfx, ?).
:- op(701, xfy, &).
:- op(1200, xfx, :->).
%:- op()

spin:term_expansion( :->(Head0, Body0), :-(Head, Body) ) :-
    head_handler(Head0, H),
    handler_parts(H, Verb, Path0, Headers, ReqBody0, Status),
    once(head_body(ReqBody0, ReqBody)),
    path_term(Path0, p(Path, Q)),
    path_query_body(p(Path, Q), Body0, Query, Body),
    Term =.. [Verb, Path, Query],
    Head = http_handler(Term, Headers, ReqBody, Status),
    format("~w :- ~w.~n", [Head, Body]).

%Body >> get(user/ID?action=Foo, 200) :-> true.

head_handler((Req -> H0), [V, P, _, Req, S]) :-
    !,
    H0 =.. [V, P, S].
head_handler(H0, H) :-
    H0 \= (_ -> _),
    H0 =.. H.

head_body(X, X) :- var(X), !.
head_body(json(Value), json(Value)) :- !.
head_body({JSON}, json(pairs(Ps))) :-
    functor(JSON, Func, 2),
    ( Func = ',' ; Func = ':'),
    json_value({JSON}, pairs(Ps0)),
    keysort(Ps0, Ps),
    !.
head_body({JSON}, json(Value)) :-
    ( functor(JSON, _, 0) ; string(JSON) ),
    json_value(JSON, Value),
    !.
head_body(text(Cs), text(Cs)) :- !.
head_body(Cs, text(Cs)) :- nonvar(Cs), string(Cs), !.
head_body(form(Form), form(Form)) :- !.
head_body(X, X).

% decompose_head((Req -> H), ) :- 

handler_parts([V, P, S],       V, P, _, _, S).
handler_parts([V, P, R, S],    V, P, _, R, S).
handler_parts([V, P, H, R, S], V, P, H, R, S).

path_query_body(p(_, []), Body, [], Body).
path_query_body(p(P, Q), Body0, Query, (
    path_match(p(P, Query), p(P, Q)), Body0
)) :- Q \= [].

path_match(X, Y) :-
    path_term(X, p(P, Q1)),
    path_term(Y, p(P, Q2)),
    intersection(Q1, Q2, Q2).

path_term(p(P, Q), p(P, Q)).
path_term(Path?Query, p(P, Q)) :-
    !,
    path_list(Path, P),
    query_list(Query, Q0),
    keysort(Q0, Q).
path_term(Path, p(P, [])) :-
    Path \= ?(_, _),
    path_list(Path, P).

path(X) :- var(X), !.
path(X) :- atom(X), !.
path(X/Y) :- (atom(Y), ! ; var(Y)), path(X).

path_list(Path, P) :-
    path(Path),
    path_list_(Path, P0),
    reverse(P0, P).
path_list_(/, []) :- !.
path_list_(X, [X]) :- atom(X), ! ; var(X).
path_list_(X/Y, [Y|T]) :- !, path_list_(X, T).

query_list(Query, Q) :-
    query_list_(Query, Q).
query_list_(X0 & Y, [X|T]) :-
    !,
    query_value(X0, X),
    query_list_(Y, T).
query_list_(X0, [X]) :-
    once(query_value(X0, X)).

query_value(X=Y0, X-Y) :-
    ( atom(X) ; var(X) ),
    atom(Y0),
    atom_chars(Y0, Y).
query_value(X=Y, X-Y) :-
    ( atom(X) ; var(X) ),
    ( string(Y) ; var(Y) ).
query_value(X, X-[]) :-
    X \= &(_, _),
    (atom(X) ; var(X)).
