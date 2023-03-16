:- module(wasm_generic, [host_rpc/1]).
:- use_module(library(wasm)).
:- use_module(library(pseudojson)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

host_call(Expr, Cs) :-
	'$host_call'(Expr, Cs), !
	; host_resume_(Cs).

host_resume_(Goal) :-
	yield,
	'$host_resume'(Goal).

% host_resume_(Cs) :-
% 	yield,
% 	(  '$host_resume'(Cs0)
% 	-> (Cs = Cs0 ; host_resume_(Cs))
% 	;  !, fail
% 	).

host_rpc(Goal) :-
	unique_variable_names(Goal, Vars0),
	once(wasm:term_json(Vars0, Goal, ArgValue)),
	json_value(JSON, ArgValue),
	json_chars(JSON, Req),
	host_call(Req, Resp),
	read_term_from_chars(Resp, Reply, [variable_names(Vars1)]),
	host_rpc_eval(Reply, Goal, Vars0, Vars1).

host_rpc_eval(throw(Ball), _, _, _) :- throw(Ball).
host_rpc_eval(true, _, _, _).
host_rpc_eval(call(G), _, Vs0, Vs1) :-
	union(Vs0, Vs1, _),
	call(G).
host_rpc_eval(G, G, Vs0, Vs1) :-
	G \= call(_),
	union(Vs0, Vs1, _).

% from format/charsio:

unique_variable_names(Term, VNs) :-
	term_variables(Term, Vs),
	foldl(var_name, Vs, VNs, 0, _).

var_name(V, Name=V, Num0, Num) :-
	fabricate_var_name(numbervars, Name, Num0),
	Num is Num0 + 1.
