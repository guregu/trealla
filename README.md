# Trealla Prolog

This is a fork of [Trealla Prolog](https://github.com/trealla-prolog/trealla) for experimenting with WebAssembly/WASI.
For more info on Trealla, check out the parent repository.

We endeavor to keep this fork as close as possible to the upstream and contribute all stable changes upstream.
Ideally, when Wasm support is better stablized, this fork won't need to exist.

## Download

You can find binaries on [the Releases page](https://github.com/guregu/trealla/releases).

## Differences from upstream
- `library(wasm)` JSON-based programmatic toplevel.
- `library(wasm_*)` host-guest interop.
- `library(pseudojson)` Very fast JSON parser/generator (but not validator).
- Wasm system predicates: `'$host_call'/2` and `'$host_resume'/1` (see `js_eval/2` in `library/wasm_js.pl`).

## Compile targets

There's a bunch of new compile targets for Wasm.

### wasm

`make wasm` will build a pure WASI version of Trealla: `tpl.wasm`. This binary can be executed by any runtime that supports WASI. (Mostly) upstreamed.

### libtpl

`make libtpl` will build a Wasm binary with host calls enabled: `libtpl.wasm`. This adds host-guest interop exports and imports that break pure WASI compatibility. This is currently used by the Go port.

### libtpl-js

`make libtpl-js` builds the trealla-js version of libtpl. Includes [JS-specific predicates](https://github.com/guregu/trealla-js#predicate-reference).

### libtpl-spin

You can use Trealla Prolog with [Spin](https://developer.fermyon.com/spin/index), a server-side
runtime for WebAssembly.

`make libtpl-spin` builds the [Spin](https://github.com/fermyon/spin)-flavored libtpl.

`make SPINDIR=path/to/spin/source wit` to generate the Wasm component code via wit-bindgen.
These files are included in the repository so you won't need to generate them unless adding support for a new one.
Currently requires wit-bindgen v0.2.0.

#### Spin Components

👉 For a helpful template and README to get started, check out [**trealla-spin**](https://github.com/guregu/trealla-spin). See also `library/spin.pl`.

- [x] Inbound HTTP (via `http_handler/4` multifile predicate)
- [x] Outbound HTTP (via `http_fetch/3`)
- [x] Outbound PostgreSQL
- [x] SQLite
- [ ] Inbound Redis
- [ ] Outbound Redis
- [x] [Key-value store](https://developer.fermyon.com/spin/kv-store)

Place a file called `init.pl` or `lib.pl` in your root directory of the Spin component.
From there you can load other modules, etc.
You can also use set the `INIT` environment variable to a file path to use that file instead.

Example of a visit counter using Spin:

```prolog
:- use_module(library(spin)).

% Roughly:
% http_handler(verb(Path, QueryParams), Headers, RequestBody, ResponseCode) :-
%	http_header_set("cache-control", "no-cache"),
%   write(http_body, 'response body').

http_handler(get("/", _), _, _, 200) :-
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
	format(http_body, "Welcome, visitor #~d!", [N]).
```

<<<<<<< HEAD
- [Trealla Prolog](https://github.com/trealla-prolog/trealla): parent repository
- [trealla-js](https://github.com/guregu/trealla-js): Trealla for Javascript
- [trealla-prolog/go](https://github.com/trealla-prolog/go): Trealla for Go
=======
See the *library/raylib.pl* and *samples/test_raylib.pl* for an example
usage including passing and returning structs by value.

See the *library/curl.pl* and *samples/test_curl.pl* for an example
usage downloading a file.

This is an example using SQLITE. Given the code in *samples/sqlite3.pl*...

```prolog
	:- use_module(library(sqlite3)).

	run :-
		test('samples/sqlite3.db', 'SELECT * FROM company').

	test(Database, Query) :-
		sqlite_flag('SQLITE_OK', SQLITE_OK),
		sqlite3_open(Database, Connection, Ret), Ret =:= SQLITE_OK,
		bagof(Row, sqlite3_query(Connection, Query, Row, _), Results),
		writeq(Results), nl.
```

Run...

```console
	$ tpl -g run,halt samples/sqlite3.pl
	[[1,'Paul',32,'California',20000.0],[2,'Allen',25,'Texas',15000.0],[3,'Teddy',23,'Norway',20000.0],[4,'Mark',25,'Rich-Mond ',65000.0],[5,'David',27,'Texas',85000.0],[6,'Kim',22,'South-Hall',45000.0]]
```

Concurrent Tasks						##EXPERIMENTAL##
================

Co-operative multitasking is available in the form of light-weight
coroutines that run until they yield either explicitly or implicitly
(when waiting on an event of some kind). They are called a `task` here.

	call_task/[1-n]	        # concurrent form of call/1-n
	tasklist/[2-8]          # concurrent form of maplist/1-n

An example:

```prolog
	:-use_module(library(http)).

	geturl(Url) :-
		http_get(Url,_Data,[status_code(Code),final_url(Location)]),
		format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

	% Fetch each URL in list sequentially...

	test54 :-
		L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
		maplist(geturl,L),
		write('Finished\n').

	$ tpl samples/test -g "time(test54),halt"
	Job [www.google.com] 200 ==> www.google.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Finished
	Time elapsed 0.663 secs

	% Fetch each URL in list concurrently...

	test56 :-
		L = ['www.google.com','www.bing.com','www.duckduckgo.com'],
		tasklist(geturl,L),
		write('Finished\n').

	$ tpl samples/test -g "time(test56),halt"
	Job [www.duckduckgo.com] 200 ==> https://duckduckgo.com done
	Job [www.bing.com] 200 ==> www.bing.com done
	Job [www.google.com] 200 ==> www.google.com done
	Finished
	Time elapsed 0.33 secs
```

Linda Co-ordination Language			##EXPERIMENTAL##
============================

Implements a toy (local-only) version of Linda using tasks. See:
[swi-prolog](https://www.swi-prolog.org/pldoc/man?section=tipc-linda-clients).

	linda_eval/1                    # linda_eval(:goal)
	out/1                           # out(+tuple)
	in/1                            # in(?tuple)
	rd/1                            # rd(?tuple)
	in_noblock/1                    # in_noblock(?tuple)
	rd_noblock/1                    # rd_noblock(?tuple)
	bagof_in_noblock/3              # bagof_in_noblock(+term,+tuple,?list)
	bagof_rd_noblock/3              # bagof_rd_noblock(+term,+tuple,?list)
	wait/0
	end_wait/0

For example:

```prolog
	:- use_module(library(linda)).
	:- initialization(main).

	main :-
		linda_eval(consumer('A')),
		linda_eval(consumer('B')),
		linda_eval(producer),
		wait,
		in(producer),               % verify it finished normally
		writeq(done), nl,
		halt.

	producer :-
		between(1, 10, I),
			out({msg:I}),
			sleep(0.25),
			fail.
	producer :-
		forall(rd_noblock({msg:_}), sleep(0.001)),
		end_wait.

	consumer(N) :-
		in({msg:I}),
		write(['consumer',N,'got=',I]), nl,
		random(R),
		sleep(R),
		fail.
```

```console
	$ tpl samples/test_linda.pl
	[consumer,B,got=,1]
	[consumer,B,got=,2]
	[consumer,B,got=,3]
	[consumer,A,got=,4]
	[consumer,B,got=,5]
	[consumer,A,got=,6]
	[consumer,B,got=,7]
	[consumer,A,got=,8]
	[consumer,A,got=,9]
	[consumer,B,got=,10]
	done
```

Concurrent Futures						##EXPERIMENTAL##
==================

Inspired by [Tau-Prolog](http://tau-prolog.org/documentation#concurrent)
concurrent futures. Uses co-operative tasks.

	future/3 – Make a Future from a Prolog goal.
	future_all/2 – Make a Future that resolves to a list of the results of an input list of futures.
	future_any/2 – Make a Future that resolves as soon as any of the futures in a list succeeds.
	future_cancel/1 – Cancel unfinished future.
	future_done/1 – Check if a future finished.
	await/2 – Wait for a Future.

For example:

```prolog
	:- use_module(library(concurrent)).
	:- use_module(library(http)).

	test :-
		future(Status1, geturl("www.google.com", Status1), F1),
		future(Status2, geturl("www.bing.com", Status2), F2),
		future(Status3, geturl("www.duckduckgo.com", Status3), F3),
		future_all([F1,F2,F3], F),
		await(F, StatusCodes),
		C = StatusCodes.
```

See `samples/test_concurrent.pl`.


Engines						##EXPERIMENTAL##
=======

Inspired by [SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=engine-predicates)
engines. Uses co-operative tasks.

	engine_create/[3,4]
	engine_next/2
	engine_yield/1
	engine_post/[2,3]
	engine_fetch/1
	engine_self/1
	is_engine/1
	current_engine/1
	engine_destroy/1


Pre-emptive Multithreading
==========================

Start independent (shared state) Prolog queries as dedicated POSIX
threads and communicate via message queues. Note: the database *is*
shared. These predicates conform to the *ISO Prolog multithreading
support* standards proposal (ISO/IEC DTR 13211–5:2007), now lapsed.

	thread_create/3		# thread_create(:callable,-thread,+options)
	thread_create/2		# thread_create(:callable,-thread)
	thread_signal/2		# thread_signal(+thread,:callable)
	thread_join/2		# thread_join(+thread,-term)
	thread_cancel/1		# thread_cancel(+thread)
	thread_detach/1		# thread_detach(+thread)
	thread_self/1		# thread_self(-thread)
	thread_exit/1		# thread_exit(+term)
	thread_sleep/1		# thread_sleep(+integer)
	thread_yield/0		# thread_yield
	thread_property/2	# thread_property(+thread,+term)
	thread_property/1	# thread_property(+term)

Where 'options' can be *alias(+atom)*, *at_exit(:term)* and/or *detached(+boolean)*
(the default is *NOT* detached, ie. joinable).

Create a stand-alone message queue...

	message_queue_create/2		# message_queue_create(-queue,+options)
	message_queue_create/1		# message_queue_create(-queue)
	message_queue_destroy/1		# message_queue_destroy(+queue)
	message_queue_property/2	# message_queue_property(+queue,+term)
	thread_send_message/2		# thread_send_message(+queue,+term)
	thread_send_message/1		# thread_send_message(+term)
	thread_get_message/2		# thread_get_message(+queue,?term)
	thread_get_message/1		# thread_get_message(?term)
	thread_peek_message/2		# thread_peek_message(+queue,?term)
	thread_peek_message/1		# thread_peek_message(?term)

Where 'options' can be *alias(+atom)*.

Create a stand-alone mutex...

	mutex_create/2				# mutex_create(-mutex,+options)
	mutex_create/1				# mutex_create(-mutex)
	mutex_destroy/1				# mutex_destroy(+mutex)
	mutex_property/2			# mutex_property(+mutex,+term)
	with_mutex/2				# with_mutex(+mutex,:callable)

	mutex_trylock/1				# mutex_trylock(+mutex)
	mutex_lock/1				# mutex_lock(+mutex)
	mutex_unlock/1				# mutex_unlock(+mutex)
	mutex_unlock_all/0			# mutex_unlock_all

Where 'options' can be *alias(+atom)*. Use of mutexes other than
*with_mutex/2* should generally be avoided.

For example...

	```console
	?- thread_create((format("thread_hello~n",[]),sleep(1),format("thread_done~n",[]),thread_exit(99)), Tid, []), format("joining~n",[]), thread_join(Tid,Status), format("join_done~n",[]).
	joining
	thread_hello
	thread_done
	join_done
	   Tid = 1, Status = exited(99).
	?-
	```

Prolog instances			##EXPERIMENTAL##
================

Start independent (no shared state) Prolog instances as dedicated
pre-emptive threads and communicate via message queues. Each thread
has it's own message queue associated with it. Note: the database
is *not* shared. For shared state consider using the blackboard.

	pl_thread/3				# pl_thread(-thread,+filename,+options)
	pl_thread/2				# pl_thread(-thread,+filename)

Where 'options' can be (currently just) *alias(+atom)*.

	pl_msg_send/2			# pl_msg_send(+thread,+term)
	pl_msg_recv/2			# pl_msg_recv(-thread,-term)


For example...

```console
	$ cat samples/thread_calc.pl
	:- initialization(main).

	% At the moment we only do sqrt here...

	main :-
		write('Calculator running...'), nl,
		repeat,
			pl_msg_recv(Tid, Term),
			Term = sqrt(X, Y),
			Y is sqrt(X),
			pl_msg_send(Tid, Term),
			fail.

	$ tpl
	?- pl_thread(_, 'samples/thread_calc.pl', [alias(calc)]).
	Calculator running...
	?- Term = sqrt(2, V),
		pl_msg_send(calc, Term),
		pl_msg_recv(_, Term).
	   Term = sqrt(2,1.4142135623731), V = 1.4142135623731.
	?-
```


Profile
=======

Why did I put this here?

```console
	$ time tpl -q -g 'main,statistics(profile,_),halt' -f ~/trealla/samples/out.pl 2>out.csv
	$ head -1 out.csv >out_sorted.csv && tail -n+2 out.csv | sort -k 3 -t ',' -n -r >> out_sorted.csv
	$ cat out_sorted.csv
	#functor/arity,match_attempts,matched,tcos
	'member_/3',20505037,20036023,19362515
	'can_step/5',1149136,288705,189915
	'can_move/5',164848,98905,32873
	'strength/4',1074794,63382,31691
	'minus_one/2',1621942,1621942,0
	'make_move/6',1086892,1086892,0
	'member_/3',20709531,730369,0
	'member/2',673508,673508,0
	'occupied_by/4',673316,673316,0
	...
```
>>>>>>> 223e9ffe8b15ab5e12f62c87197af5a589f26e05
