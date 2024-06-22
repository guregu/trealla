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
:- use_module(library(dcgs)). % TODO: required for clpz?

% Host (WASM) → Guest (Trealla)

js_toplevel :-
	getline(Line),
	js_ask(Line).

js_ask(Input) :-
	% '$memory_stream_create'(Stream, []),
	js_ask(stdout, Input).

js_ask(Stream, Input) :-
	catch(
		read_term_from_chars(Input, Query, [variable_names(Vars)]),
		Error,
		(
			write(Stream, '\x2\\x3\'),
			result_json(error, Stream, Vars, Error)
		)
	),
	% '$memory_stream_create'(Stream, []),
	catch(
		query(Stream, Query, Status),
		Error,
		Status = error
	),
	write(Stream, '\x3\'),
	result_json(Status, Stream, Vars, Error).

query(Stream, Query, Status) :-
	write(Stream, '\x2\'),  % START OF TEXT
	(   call(Query)
	*-> Status = success
	;   Status = failure
	).

result_json(success, Stream, Vars, _) :-
	write(Stream, '{"status":"success","answer":'),
	once(solution_json(Stream, Vars)),
	write(Stream, '}'),
	nl(Stream).
result_json(failure, Stream, _, _) :-
	write(Stream, '{"status":"failure"}'),
	nl(Stream).
result_json(error, Stream, Vars, Error) :-
	write(Stream, '{"status":"error","error":'),
	once(term_json(Stream, Vars, Error)),
	write(Stream, '}'),
	nl(Stream).

solution_json(Stream, []) :-
	write(Stream, '{}').
solution_json(Stream, Vars) :-
	Vars = [V|Vs],
	write(Stream, '{'),
	sub_json(Stream, Vars, V),
	solution_json_(Stream, Vs),
	write(Stream, '}').

solution_json_(_, []).
solution_json_(Stream, [V|Vs]) :-
	write(Stream, ','),
	sub_json(Stream, [V|Vs], V),
	solution_json_(Stream, Vs).

sub_json(Stream, Vars, Var0=Value0) :-
	atom_chars(Var0, Var),
	write_json_term(Stream, Var),
	write(Stream, ':'),
	once(term_json_top(Stream, Vars, Value0)).

term_json(Stream, _, Value) :-
	Value == [],
	write(Stream, '[]').

term_json(Stream, _, Value0) :-
	atom(Value0),
	Value0 == '',
	write(Stream, '{"functor":""}').

term_json(Stream, _, Value0) :-
	atom(Value0),
	Value0 \= '',
	atom_chars(Value0, Value),
	write(Stream, '{"functor":'),
	write_json_term(Stream, Value),
	write(Stream, '}').

term_json(Stream, _, Value) :-
	string(Value),
	(  Value == ''
	-> true
	;  write_json_term(Stream, Value)
	).

term_json(Stream, _, Value) :-
	float(Value),
	write(Stream, Value).
term_json(Stream, _, Value) :-
	number(Value),
	% safe value range for JS integers
	Value =< 9007199254740991,
	Value >= -9007199254740991,
	write(Stream, Value).
term_json(Stream, _, Value) :-
	number(Value),
	write(Stream, '{"number":"'),
	write(Stream, Value),
	write(Stream, '"}').

term_json(Stream, Vars, Value) :-
	is_list(Value),
    term_json_list_(Stream, Vars, Value),
	!.

term_json(Stream, Vars, Value) :-
	compound(Value),
	Value =.. [Functor0|Args0],
	atom_chars(Functor0, Functor),
	(  Functor0 == ''
	-> write(Stream, '{"functor":""')
	;  write(Stream, '{"functor":'), write_json_term(Stream, Functor)
	),
	write(Stream, ',"args":'),
	term_json_list_(Stream, Vars, Args0),
	write(Stream, '}'),
	!.

term_json(Stream, Vars, Value) :-
	var(Value),
	once(var_name(Vars, Value, Name)),
	write(Stream, '{"var":'),
	write_json_term(Stream, Name),
	write(Stream, '}'),
	!.

term_json(Stream, _, Value) :-
	is_stream(Value),
	write(Stream, '{"stream":'),
	term_json_stream_(Stream, Value),
	write(Stream, '}'),
	!.

term_json(Stream, _, _) :-
	write(Stream, '{"blob": "_"}'),
	!.
	% write_term_to_chars(Value, [], Cs).

term_json_top(Stream, Vars, Value) :-
	var(Value),
	once(var_name(Vars, Value, Name)),
	write(Stream, '{"var":'),
	write_json_term(Stream, Name),
	attvar_json(Stream, Vars, Value),
	write(Stream, '}'),
	!.
term_json_top(Stream, Vars, Value) :-
	once(term_json(Stream, Vars, Value)).

var_name([K0=V|_], Var, K) :-
	V == Var,
	atom_chars(K0, K).
var_name([_=V|Vs], Var, Name) :-
	V \== Var,
	var_name(Vs, Var, Name).
var_name([], _, "_").

attvar_json(Stream, Vars, Var) :-
	copy_term(Var, Var, Attr),
	write(Stream, ',"attr":'),
	( 	( Attr \= [], Attr \= [[]] )
	-> 	(
			term_json(Stream, Vars, Attr)
		)
	;	write(Stream, '[]')
	).

term_json_list_(Stream, _, []) :-
	write(Stream, '[]'), !.
term_json_list_(Stream, Vars, [V|Vs]) :-
	write(Stream, '['),
	term_json(Stream, Vars, V), !,
	term_json_list_tail_(Stream, Vars, Vs),
	write(Stream, ']').
term_json_list_tail_(_, _, []) :- !.
term_json_list_tail_(Stream, Vars, [V|Vs]) :-
	write(Stream, ','),
	term_json(Stream, Vars, V), !,
	term_json_list_tail_(Stream, Vars, Vs).

term_json_stream_(Stream, V) :-
	stream_property(V, alias(Alias0)),
	atom_chars(Alias0, Alias),
	write_json_term(Stream, Alias),
	!.
term_json_stream_(Stream, V) :-
	stream_property(V, file_no(Number)),
	write(Stream, Number),
	!.
term_json_stream_(Stream, V) :-
	stream_property(V, file(Number)),
	write(Stream, Number),
	!.
term_json_stream_(Stream, _) :-
	write(Stream, '"_"').

write_json_term(Stream, T) :-
	write_term(Stream, T, [json(true), double_quotes(true), quoted(true), max_depth(0)]).
