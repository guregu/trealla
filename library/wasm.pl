/*
	This is a JSON toplevel that WASM ports can use to grab answers from Trealla programmatically.
	Very experimental and not in the upstream.

	Current format:
	ASCII START OF TEXT (0x02), stdout text as-is, then ASCII END OF TEXT (0x03), then a JSON response, then a line break.

	{
		"status": "success" | "failure" | "error",
		"answer": { "X": "<substitution for X>", ... },
		"error": "<throw/1 exception term>"
	}
*/
:- module(wasm, [js_toplevel/0, js_ask/1]).

:- use_module(library(lists)).
:- use_module(library(pseudojson)).

% Host (WASM) → Guest (Trealla)

js_toplevel :-
	getline(Line),
	js_ask(Line).

js_ask(Input) :-
	catch(
		read_term_from_chars(Input, Query, [variable_names(Vars)]),
		Error,
		(
			write(stdout, '\u0002\u0003'),
			result_json(error, Vars, Error),
			flush_output(stdout)
		)
	),
	catch(
		query(Query, Status),
		Error,
		Status = error
	),
	write(stdout, '\u0003'),
	% write(Vars),
	setup_call_cleanup(
		'$memory_stream_create'(Stream, [alias(js_ask)]),
		(
			result_json(Status, Vars, Error),
			'$memory_stream_to_chars'(Stream, Got),
			'$put_chars'(Got)
		),
		close(Stream)
	),
	% write_result(JSON),
	flush_output(stdout).

query(Query, Status) :-
	write(stdout, '\u0002'),  % START OF TEXT
	(   call(Query)
	*-> Status = success
	;   Status = failure
	).

write_result(JSON) :-
	% hack to get Go interop to work:
	set_output(stdout),
	% json_value(JS, JSON),
	% json_chars(JS, Cs),
	write(JSON),
	% '$put_chars'(stdout, Cs),
	nl.

result_json(success, Vars, _) :-
	write(js_ask, '{"status":"success", "answer":'),
	once(solution_json(Vars)),
	write(js_ask, '}').
result_json(failure, _, _) :-
	write(js_ask, '{"status":"failure"}').
result_json(error, Vars, Error) :-
	write(js_ask, '{"status":"error", "error":'),
	once(term_json(Vars, Error)),
	write(js_ask, '}').

solution_json([]) :-
	write(js_ask, '{}').
solution_json(Vars) :-
	maplist(sub_json(Vars), Vars).

sub_json(Vars, _=Value0) :-
	% atom_chars(Var0, Var),
	once(term_json_top(Vars, Value0)).

term_json(_, Value) :-
	Value == [],
	write(js_ask, '[]').

term_json(_, Value0) :-
	atom(Value0),
	write(js_ask, '{"functor":"'),
	write(js_ask, Value0),
	write(js_ask, '"}').
	% atom_chars(Value0, Value).

term_json(_, Value) :-
	string(Value),
	write(js_ask, '"'),
	'$put_chars'(Value),
	write(js_ask, '"').

term_json(_, Value) :-
	float(Value),
	write(js_ask, Value).
term_json(_, Value) :-
	number(Value),
	% safe value range for JS integers
	Value =< 9007199254740991,
	Value >= -9007199254740991,
	write(js_ask, Value).
term_json(_, Value) :-
	number(Value),
	write(js_ask, '{"number":"'),
	write(js_ask, Value),
	write(js_ask, '"}').

term_json(Vars, Value) :-
	is_list(Value),
	once(maplist(term_json(Vars), Value)).

term_json(Vars, Value) :-
	compound(Value),
	Value =.. [Functor0|Args0],
	write(js_ask, '{"functor":"'),
	write(js_ask, Functor0),
	write(js_ask, '","args":['),
	once(maplist(term_json(Vars), Args0)),
	write(js_ask, ']}').

term_json(Vars, Value) :-
	var(Value),
	once(var_name(Vars, Value, Name)),
	write(js_ask, '{"var":"'),
	write(js_ask, Name),
	write(js_ask, '"}').

term_json(_, Value) :-
	is_stream(Value),
	% TODO: grab alias/fd from stream
	write(js_ask, '{"stream": -1}').

term_json(_, _) :-
	write(js_ask, '{"blob": "?"}').
	% write_term_to_chars(Value, [], Cs).

term_json_top(Vars, Value) :-
% pairs([string("var")-string(Name), string("attr")-Attr])
	var(Value),
	once(var_name(Vars, Value, Name)),
	write('{"var":"'),
	write(Name),
	write('","attr":'),
	attvar_json(Vars, Value),
	write('}').
term_json_top(Vars, Value) :- term_json(Vars, Value).

var_name([K=V|_], Var, Name) :-
	V == Var,
	atom_chars(K, Name).
var_name([_=V|Vs], Var, Name) :-
	V \== Var,
	var_name(Vs, Var, Name).
var_name([], _, "_").

attvar_json(Vars, Var) :-
	copy_term(Var, Var, Attr),
	once(term_json(Vars, Attr)).
