:- module(spin, [
		http_handler/4,
		current_http_uri/1, current_http_method/1, current_http_body/1,
		current_http_param/2, current_http_header/2,
		http_header_set/2,
		http_body_output/1,
		html_content/0, html_content/1,
		text_content/0, text_content/1,
		prolog_content/0, prolog_content/1,
		store_open/1, store_open/2,
		store_close/1,
		store_get/3, store_exists/2,
		store_keys/2,
		store_set/3, store_delete/2,
		http_fetch/3,
		postgres_open/3,
		postgres_execute/4,
		postgres_query/5
	]).

:- use_module(library(pseudojson)).
:- use_module(library(error)).
:- use_module(library(httprouter)).

:- dynamic(current_http_uri/1).
:- dynamic(current_http_method/1).
:- dynamic(current_http_body/1).
:- dynamic(current_http_header/2).
:- dynamic(current_http_param/2).

:- dynamic(http_handler/4).
:- multifile([http_handler/4]).

init :-
	map_create(_, [alias(http_headers)]),
	'$memory_stream_create'(_, [alias(http_body)]).

:- initialization(init).

consult_init :- getenv('INIT', File), try_consult(File), !.
consult_init :- try_consult(init), !.
consult_init :- try_consult(lib).

http_handle_request(URI, Method) :-
	assertz(current_http_uri(URI)),
	assertz(current_http_method(Method)),
	consult_init,
	fail.
http_handle_request(RawURI, Method) :-
	findall(K:V, current_http_header(K, V), Headers),
	trace,
	uri_handle(RawURI, Method, Handle),
	% split_string(URI, "/", "", Split),
	% format(http_body, "PATH: ~w", [Path]),
	once(read_body(Body)),
	% get("/index.pl", ["a"-"..."])
	% Handle =.. [Method, URI, Params],
	http_handle_(Handle, Headers, Body, Status),
	map_set(http_headers, "status", Status).

uri_params(RawURI, URI, Params) :-
	once(phrase(uri(URI, Params0), RawURI)),
	keysort(Params0, Params).

uri_term("/", []) :- !.
uri_term(URI, Split) :-
	URI \= "/",
	atom_chars(A, URI),
	split_string(A, '/', [], Split).

uri_handle(RawURI, Method, X) :-
	uri_params(RawURI, URI, Params),
	uri_term(URI, Path),
	X =.. [Method, Path, Params].

read_body([]) :- \+current_http_body(_).
read_body([]) :- current_http_body([]).
read_body(form(Form)) :-
	current_http_header("content-type", "application/x-www-form-urlencoded"),
	current_http_body(Body),
	once(phrase(form(Form), Body)).
read_body(json(JSON)) :-
	current_http_header("content-type", "application/json"),
	current_http_body(Body),
	json_chars(JS, Body),
	json_value(JS, JSON).
read_body(text(Body)) :-
	current_http_body(Body).

http_handle_(Handle, Headers, Body, Status) :-
	catch(
		http_handler(Handle, Headers, Body, Status),
		Error,
		(
			Status = 500,
			format(http_body, "Internal server error. Unhandled exception: ~w", [Error]),
			format(stderr, "Unhandled exception: ~w in ~w", [Error, Handle])
		)
	),
	!.
http_handle_(_, _, _, 404) :-
	write(http_body, 'Not found\n').

http_fetch(URL, Result, Options) :-
	must_be(chars, URL),
	setup_call_cleanup(
		(
			map_create(RequestMap, []),
			map_create(RequestHeaders, []),
			map_create(ResponseMap, []),
			map_create(ResponseHeaders, [])
		),
		http_fetch_(URL, Options, RequestMap, RequestHeaders, ResponseMap, ResponseHeaders, Result),
		(
			map_close(RequestMap),
			map_close(RequestHeaders),
			map_close(ResponseMap),
			map_close(ResponseHeaders)
		)
	).

http_fetch_(URL, Options, RequestMap, RequestHeaders, ResponseMap, ResponseHeaders, Result) :-
	outbound_request_options(Options, RequestMap, RequestHeaders),
	'$wasi_outbound_http'(URL, RequestMap, RequestHeaders, ResponseMap, ResponseHeaders),
	spin_http_result(ResponseMap, ResponseHeaders, Result).

outbound_request_options(Options, Map, HeaderMap) :-
	( memberchk(method(Method), Options) ; Method = get ),
	must_be(atom, Method),
	map_set(Map, method, Method),
	(  memberchk(body(Body), Options), Body \= []
	-> must_be(chars, Body), map_set(Map, body, Body)
	;  true
	),
	( memberchk(headers(Headers), Options) ; Headers = [] ),
	must_be(list, Headers),
	maplist(map_set_kv(HeaderMap), Headers).

map_set_kv(Map, Key:Value) :-
	map_set(Map, Key, Value).

spin_http_result(Map, HeaderMap, response(Status, Headers, Body)) :-
	map_get(Map, status, Status),
	map_get(Map, body, Body0),
	atom_chars(Body0, Body),
	map_list(HeaderMap, Headers).

try_consult(Name) :-
	catch(consult(Name), error(existence_error(source_sink, _), _), fail).

http_header_set(Name, Value) :-
	% Spin uses HTTP2-style lowercase names
	string_lower(Name, Lower),
	map_set(http_headers, Lower, Value).

html_content :-
	http_header_set("content-type", "text/html; charset=utf-8").
html_content(HTML) :-
	html_content,
	'$put_chars'(http_body, HTML).

text_content :-
	http_header_set("content-type", "text/plain; charset=utf-8").
text_content(Text) :-
	text_content,
	'$put_chars'(http_body, Text).

json_content :-
	http_header_set("content-type", "application/json").
json_content(JSON) :-
	json_content,
	json_value(JS, JSON),
	json_chars(JS, Cs),
	!,
	'$put_chars'(http_body, Cs).
json_content(JS) :-
	json_content,
	json_chars(JS, Cs),
	'$put_chars'(http_body, Cs).

prolog_content :-
	http_header_set("content-type", "application/x-prolog").
prolog_content(Term) :-
	prolog_content,
	write_term(http_body, Term, []),
	nl(http_body).

http_body_output(http_body).

uri(URI, Params) --> path(URI), "?", query(Params).
uri(URI, []) --> path(URI).

path([C|Cs]) --> [C], { C \= '?' }, path(Cs).
path([]) --> [].

query([V|Vs]) --> param(V), params(Vs).
query([]) --> [].

params([V|Vs]) --> "&", param(V), params(Vs).
params([]) --> [].

param(K-V) --> param_key(K0), "=", param_value(V0),
	{ atom_chars(K1, K0), chars_urlenc(K2, K1, []), atom_chars(K2, K),
	  atom_chars(V1, V0), chars_urlenc(V2, V1, []), atom_chars(V2, V) }.

param_key([V|Vs]) --> [V], { V \= '=' }, param_key(Vs).
param_key([]) --> [].
param_value([V|Vs]) --> [V], { V \= '&' }, param_value(Vs).
param_value([]) --> [].

form([V|Vs]) --> param(V), params(Vs).

store_open(Handle) :- store_open(default, Handle).
store_open(Name, Handle) :- '$wasi_kv_open'(Name, Handle).

store_close(Handle) :- '$wasi_kv_close'(Handle).

store_get(Handle, Key, Value) :-
	ground(Key),
	term_canon(Key, Ks),
	'$wasi_kv_get'(Handle, Ks, Vs),
	canon_term(Vs, Value).
store_get(Handle, Key, Value) :-
	\+ground(Key),
	store_keys(Handle, Keys),
	member(Key, Keys),
	ground(Key),
	store_get(Handle, Key, Value).

store_keys(Handle, Keys) :-
	'$wasi_kv_get_keys'(Handle, Ks),
	once(maplist(canon_term, Ks, Keys)).

store_set(Handle, Key, Value) :-
	must_be(ground, Key),
	must_be(ground, Value),
	term_canon(Key, Ks),
	term_canon(Value, Vs),
	'$wasi_kv_set'(Handle, Ks, Vs).

store_delete(Handle, Key) :-
	must_be(ground, Key),
	term_canon(Key, Cs),
	'$wasi_kv_delete'(Handle, Cs).

store_exists(Handle, Key) :- '$wasi_kv_exists'(Handle, Key).

term_canon(key(Cs), Cs).
term_canon(Term, Cs) :-
	Term \= key(_),
	write_term_to_chars(Term, [], Cs).
canon_term([], []).
canon_term(Cs, Term) :-
	catch(read_term_from_chars(Cs, Term, []), error(syntax_error(_), _), Term = key(Cs)).

postgres_open(Addr, pg(Cs), Options) :-
	must_be(chars, Addr),
	must_be(list, Options),
	once(phrase(pg_conn(Addr, Options), Cs)).

postgres_execute(PG, Stmt, Params, Result) :-
	(  nonvar(PG), PG = pg(Addr)
	-> true
	;  throw(error(domain_error(postgres_connection, PG), postgres_execute/4))
	),
	'$outbound_pg_execute'(Addr, Stmt, Params, Result),
	(  Result = error(Kind, Msg)
	-> throw(error(postgres_error(Kind, Msg), postgres_execute/4))
	;  true
	).

postgres_query(PG, Stmt, Params, Rows, Cols) :-
	(  nonvar(PG), PG = pg(Addr)
	-> true
	;  throw(error(domain_error(postgres_connection, PG), postgres_query/5))
	),
	'$outbound_pg_query'(Addr, Stmt, Params, Rows, Cols),
	(  Rows = error(Kind, Msg)
	-> throw(error(postgres_error(Kind, Msg), postgres_query/5))
	;  true
	).

pg_conn(Addr, Opts) -->
	"host=",
	Addr,
	" ",
	pg_opts(Opts).

pg_opts([Opt|Os]) -->
	(  pg_opt(Opt)
	-> []
	;  { throw(error(domain_error(postgres_option, Opt), postgres_open/2)) }
	),
	" ",
	pg_opts(Os).
pg_opts([]) --> [].

pg_opt(port(P)) --> "port=", { number_chars(P, Cs) }, Cs.
pg_opt(user(U)) --> "user=", U.
pg_opt(password(Cs)) --> "password=", Cs.
pg_opt(dbname(Cs)) --> "dbname=", Cs.
