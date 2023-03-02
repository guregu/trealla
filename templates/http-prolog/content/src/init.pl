:- use_module(library(spin)).

http_handler(get("/", _QueryParams), _Headers, _Body, 200) :-
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
    % stream alias http_headers lets you manipulate response headers
    map_set(http_headers, "x-powered-by", "memes"),
    % stream alias http_body is the response body
	format(http_body, "<h3>Welcome, visitor #~d!</h3>", [N]).
