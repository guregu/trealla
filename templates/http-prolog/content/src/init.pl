:- use_module(library(spin)).

% See library/spin.pl for all the predicates built-in
% https://github.com/guregu/trealla/blob/main/library/spin.pl

%% http_handler(+Spec, +Headers, +Body, -Status)

http_handler(get("/", _QueryParams), _RequestHeaders, _RequestBody, 200) :-
	html_content,
	setup_call_cleanup(
		store_open(default, Store),
		(
			(  store_get(Store, counter, N0)
			-> true
			;  N0 = 0
			),
			succ(N0, N),
			store_set(Store, counter, N)
		),
		store_close(Store)
	),
	http_header_set("x-powered-by", "memes"),
	current_prolog_flag(dialect, Dialect),
	% stream alias http_body is the response body
	write(http_body, '<!doctype html><html>'),
	format(http_body, "<h1>Hello, ~a prolog!</h1>", [Dialect]),
	format(http_body, "Welcome, visitor #<b>~d!</b>", [N]),
	write(http_body, '</html>').

http_handler(get("/json", _), _, _, 200) :-
	wall_time(Time),
	% json_content({"time": Time}) works too
	json_content(pairs([string("time")-number(Time)])).
