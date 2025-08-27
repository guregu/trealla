Trealla Prolog
==============

A compact, efficient Prolog interpreter with ISO Prolog aspirations.

	MIT licensed
	Integers & Rationals are unbounded
	Atoms are UTF-8 of unlimited length
	The default double-quoted representation is *chars* list
	Strings & slices are super-efficient (especially with mmap'd files)
	REPL with history
	Runs on Linux, Android, BSD, macOS, and WebAssembly (WASI) & Go
	API for calling from C (or by using WASM from Go & JS)
	Foreign function interface (FFI) for calling out to user code
	Access SQLITE databases using builtin module (uses FFI)
	Concurrency via tasks / linda / futures / engines (generators)
	Pre-emptive multi-threading
	Blackboarding primitives
	...
	FFIs for GNU Scientific Library (GSL), SQLite, Raylib
	Delimited continuations ##EXPERIMENTAL##
	Rational trees ##EXPERIMENTAL##
	CLP(Z)


Available from: [https://github.com/trealla-prolog/trealla](https://github.com/trealla-prolog/trealla).

Runs with [Jupyter Notebooks](https://github.com/LogtalkDotOrg/logtalk-jupyter-kernel#readme).


Logo
====

![Trealla Logo: Trealla](trealla.png)


Usage
=====

	tpl [options] [files] [-- args]

where options can be:

	-O0, --noopt       - no optimization
	-f file            - load file (*~/.tplrc* not loaded)
	-l file            - load file (*~/.tplrc* loaded)
	file               - load file (*~/.tplrc* loaded)
	-g goal            - query goal (only used once)
	--library path     - alt to TPL_LIBRARY_PATH env var
	-t, --trace        - trace
	-q, --quiet        - quiet mode (no banner)
	-v, --version      - version
	-h, --help         - help
	-d, --daemonize    - daemonize
	-w, --watchdog     - create watchdog
	--autofail         - autofail queries at the toplevel
	--consult          - consult from STDIN

For example:

	tpl -g test2,halt samples/sieve

Invocation without any goal presents the REPL.

The default path to the library is relative to the executable location.

The file *~/.tplrc* is consulted on startup unless the *-f* option is present.

When consulting, reconsulting and deconsulting files the *.pl* version
of the filename is always preferred (if not specified) when looking for a
file.


A note on UTF-8
===============

Trealla uses UTF-8 internally and this works well with modern operating
systems that are already [[1](https://www.utf8everywhere.org/)], or moving to
[[2](https://en.wikipedia.org/wiki/Unicode_in_Microsoft_Windows#UTF-8)],
native UTF-8.

It aligns well with standard C as functions like strcmp/memcmp that
require no special handling to respect codepoint order. This also works
seamlessly with the implementation of double-quoted *strings* (ie.
chars-list), DCGs, and mmap'd files. Any code-point specific
requirements, like *get_char*, *get_code*, *sub_atom*, *atom_length*,
*atom_codes*, *atom_chars* & *_upper/*_lower are handled on the fly.

UTF-8 atoms do not need to be quoted unless they contain breaking
characters...

```console
	?- [user].
	是.            % be: means, approximately, "True".
	不是 :- \+ 是.  % not be: means, approximately, "False".
	<CTRL-D>
	   true.
	?- 是.
	   true.
	?- 不是.
	   false.
	```

	```console
	?- X = 国字.
	   X = 国字.
	?-
```

Trealla accepts as a var any atom beginning with an uppercase
character...

```console
	?- atom_upper(δ,C).
	   C = Δ.
	?- Δ is 123456-123455.
	   Δ = 1.
	?-
```


Building
========

Written in plain-old C99.

	git clone https://github.com/trealla-prolog/trealla.git
	cd trealla

On Debian-like systems, you will need to install (if not alread( the following
packages to set up a build environment:

	sudo apt install build-essential git libreadline-dev libffi-dev libssl-dev xxd

Then...

	make

To build without FFI:

	make NOFFI=1

To build without SSL:

	make NOSSL=1

To build without pre-emptive multi-threading support:

	make NOTHREADS=1

To build (as a last resort) with the included ISOCLINE sources (default is to use GNU Readline,
except Windows):

	make ISOCLINE=1

Older compilers may require:

	make NOPEDANTIC=1

to avoid issues with newer flags.

Finally...

	make install

to install locally.

Optionally...

	make test

and there should be no errors, Further (if valgrind is installed)...

	make check 			# or, more extensively
	make leaks

Should show no memory out-of-bounds, null-pointer, use after free
or memory leaks (there may a few spurious errors).

On *BSD* systems use *gmake* to build and do

	pkg install xxd

or

	pkg install editors/vim   # if necessary

to get the *xxd* utility.

On macOS:

	brew install libffi openssl coreutils readline

and it's important to use the `brew` version of `readline`.


Unbounded integers (Bigints) and Rationals
==========================================

For unbounded arithmetic Trealla uses a modified fork of the
[imath](https://github.com/infradig/imath)
library, which is partially included in the source. Note, unbounded
integers (aka. bigints) are for arithmetic purposes only and will give a
type_error when used in places not expected. The *imath* library has a bug
whereby printing large numbers becomes exponentially slower (100K+ digits).


WebAssembly (WASI)
==================

Trealla has support for WebAssembly System Interface (WASI).

For an easy build envrionment, set up
[wasi-sdk](https://github.com/WebAssembly/wasi-sdk).
[Binaryen](https://github.com/WebAssembly/binaryen) is needed for optimization.

To build the WebAssembinary binary, set CC to wasi-sdk's clang:

	make CC=/opt/wasi-sdk/bin/clang wasm

Setting WASI_CC also works as an alternative to CC.


Cross-compile for Windows x64
=============================

To cross-compile on Linux and produce a Windows/x86-64 executable...

	sudo apt install mingw-w64
	make WIN=1 NOFFI=1 NOSSL=1

```console
	$ file tpl.exe
	tpl.exe: PE32+ executable (console) x86-64, for MS Windows
```

Some have reported success with a native Windows build using msys2.


Cross-compile for Linux x86
===========================

To cross-compile on Linux and produce a Linux/x86-32 executable...

	sudo apt install gcc-multilib
	sudo apt install libssl-dev:i386 libffi-dev:i386 libreadline-dev:i386
	make OPT=-m32

```console
	$ file tpl
	tpl: ELF 32-bit LSB shared object, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux.so.2, BuildID[sha1]=31f643d7a4cfacb0a34e81b7c12c78410493de60, for GNU/Linux 3.2.0, with debug_info, not stripped
```


Contributions
=============

Contributions are welcome.


Acknowledgements
================

This project (in current incarnation) started in March 2020 and it
would not be where it is today without help from these people:

	- [Xin Wang](https://github.com/dram)
	- [Paulo Moura](https://github.com/pmoura)
	- [Markus Triska](https://github.com/triska)
	- [Jos De Roo](https://github.com/josd)
	- [Ulrich Neumerkel](https://github.com/uwn)
	- [Guregu](https://github.com/guregu)

Strings
=======

Double-quoted strings, when *set_prolog_flag(double_quotes,chars)* is set
(which is the default) are stored as packed UTF-8 byte arrays. This is
compact and efficient. Such strings emulate a list representation and
from the programmer point of view are very much indistinguishable from
lists.

A good use of such strings is *open(filename,read,Str,[mmap(Ls))*
which gives a memory-mapped view of a file as a string *Ls*. List
operations on files are now essentially zero-overhead! DCG applications
will gain greatly (*phrase_from_file/[2-3]* uses this).

Both strings and atoms make use of low-overhead reflist-counted byte slices
where appropriate.


Non-standard predicates
=======================

	help/0
	help/1						# help(+functor) or help(+PI)
	help/2						# help(+PI,+atom) where *atom* can be *swi* or *tau*

	module_help/1				# help(+module)
	module_help/2				# help(+module,+functor) or help(+module,+PI)
	module_help/3				# help(+module,+PI,+atom) where *atom* can bw *swi* or *tau*

	source_info/2				# source_info(+PI, -list)
	module_info/2				# module_info(+atom, -list)

	module/1					# module(?atom)
	modules/1					# modules(-list)

	load_text/2					# load_text(+atom,+opts)

	listing/0
	listing/1					# listing(+PI)

	abolish/2					# abolish(+pi,+list)
	pretty/1					# pretty-print version of listing/1
	between/3
	msort/2						# version of sort/3 with duplicates
	samsort/2                   # same as msort/2
	merge/3
	format/[1-3]
	portray_clause/[1-2]
	predicate_property/2
	evaluable_property/2
	numbervars/[1,3-4]
	e/0
	name/2
	tab/[1,2]

	get_unbuffered_code/1		# read a single unbuffered code
	get_unbuffered_char/1		# read a single unbuffered character

	read_from_atom/2            # read_from_atom(+atom,?term)
	read_from_chars/2	        # read_from_chars(+chars,?term)
	read_term_from_atom/3       # read_term_from_atom(+atom,?term,+optlist)
	read_term_from_chars/3	    # read_term_from_chars(+chars,?term,+optlist)

	read_from_chars_/3	        # read_from_chars+(?term,+chars,-rest)
	read_term_from_chars_/4	    # read_term_from_chars+(?term,+optlist,+chars,-rest)

	write_term_to_atom/3        # write_term_to_atom(?atom,?term,+oplist)
	write_canonical_to_atom/3   # write_canonical_to_atom(?atom,?term,+oplist)
	term_to_atom/2              # term_to_atom(?atom,?term)

	setrand/1                   # set_seed(+integer) set random number seed
	srandom/1                   # set_seed(+integer) set random number seed
	set_seed/1                  # set_seed(+integer) set random number seed
	get_seed/1                  # get_seed(-integer) get random number seed
	rand/1                      # rand(-integer) integer [0,RAND_MAX]
	random/1                    # random(-float) float [0.0,<1.0]
	random_between/3            # random_between(+int,+int,-int) integer [arg1,<arg2]

	random_float/0              # function returning float [0.0,<1.0]
	random_integer/0            # function returning integer [0,RAND_MAX]
	rand/0                      # function returning integer [0,RAND_MAX]

	gensym/2					# gensym(+atom,-atom)
	reset_gensym/1				# reset_gensym(+atom)

	call_residue_vars/2
	expand_term/2               # expand_term(+rule,-Term)
	sub_string/5				# sub_string(+string,?before,?len,?after,?substring)
	atomic_concat/3             # atomic_concat(+atom,+list,-list)
	atomic_list_concat/2	    # atomic_list_concat(L,Atom)
	atomic_list_concat/3	    # atomic_list_concat(L,Sep,Atom)
	write_term_to_chars/3	    # write_term_to_chars(?chars,?term,+list)
	write_canonical_to_chars/3  # write_canonical_to_chars(?chars,?term,+list)
	chars_base64/3              # currently options are ignored
	chars_urlenc/3              # currently options are ignored
	hex_chars/2                 # as number_chars, but in hex
	octal_chars/2               # as number_chars, but in octal
	partial_string/2            # partial_string(+string,-String)
	partial_string/3            # partial_string(+string,-String,-Var)
	if/3, (*->)/2               # soft-cut
	call_det/2					# call_det(+call,?boolean)
	term_attributed_variables/2 # term_attributed_variables(+term,-Vs)
	copy_term_nat/2             # doesn't copy attrs
	copy_term/3                 # copy_term(+term1,-term2,-Goals)
	unifiable/3                 # unifiable(+term1,+term2,-Goals)
	?=/2                        # ?=(+term1,+term2)
	term_expansion/2
	goal_expansion/2
	cyclic_term/1
	term_singletons/2
	findall/4
	sort/4
	ignore/1
	is_list/1
	is_partial_list/1
	is_list_or_partial_list/1
	is_stream/1
	term_hash/2
	term_hash/3					# ignores arg2 (options)
	time/1
	inf/0
	nan/0
	\uXXXX and \UXXXXXXXX 		# Unicode escapes for JSON)
	gcd/2
	uuid/1                      # uuid(-string)
	load_files/[1,2]
	split_string/4				# SWI-compatible
	module/1
	line_count/2
	atom_number/2
	cfor/3						# cfor(+evaluable,+evaluable,-var)
	repeat/1					# repeat(+integer)
	make/0
	argv/1						# argv(-list)
	raw_argv/1					# raw_argv(-list)

	rdiv/2						# evaluable
	numerator/1					# evaluable
	denominator/1				# evaluable
	rational/1
	rationalize/2				# rationalize(+number,-rational)

	with_output_to(chars(Cs), Goal)		# SWI-compatible
	with_output_to(string(Cs), Goal)	# SWI-compatible
	with_output_to(atom(Atom), Goal)	# SWI-compatible

	call_with_time_limit/2		# SWI-compatible
	time_out/3					# SICStus-compatible

	bb_b_put/2					# bb_b_put(:atom, +term)
	bb_put/2					# bb_put(:atom, +term)
	bb_get/2					# bb_get(:atom, ?term)
	bb_update/3					# bb_update(:atom, ?term, ?term)
	bb_delete/2					# bb_delete(:atom, ?term)

	posix_strftime/3			# posix_strftime(+format,-string,+tm(NNN,...))
	posix_strptime/3			# posix_strptime(+format,+string,-tm(NNN,...))
	posix_mktime/2				# posix_mktime(+tm(NNN,...),-seconds)
	posix_gmtime/2				# posix_gmtime(+seconds,-tm(NNN,...))
	posix_localtime/2			# posix_localtime(+seconds,-tm(NNN,...))
	posix_ctime/2				# posix_time(+seconds,-atom)
	posix_time/1				# posix_time(-seconds)
	posix_getpid/1				# posix_pid(-pid)
	posix_getppid/1				# posix_ppid(-pid)
	posix_fork/1				# posix_fork(-pid)

	nb_setval(K,V)
	nb_getval(K,V)
	nb_delete(K)
	nb_current(K,V)

	b_setval(K,V)
	b_getval(K,V)
	b_delete(K)

	call_nth/2
	offset/2
	limit/2

	getenv/2
	setenv/2
	unsetenv/1

	directory_files/2
	delete_file/1
	exists_file/1
	rename_file/2
	copy_file/2
	time_file/2
	size_file/2
	exists_directory/1
	make_directory/1
	make_directory_path/1
	working_directory/2
	chdir/1
	absolute_file_name/[2,3]	# expand(Bool) & relative_to(file) options
	is_absolute_file_name/1
	access_file/2
	set_stream/2				# only supports alias/1 property
	alias/2						# alias(?integer,+atom)

	string_upper/2
	string_lower/2
	atom_upper/2
	atom_lower/2

	divmod/4                    # SWI-compatible
	popcount/1                  # function returning number of 1 bits
	lsb/1                       # function returning the least significant bit of a positive integer (count from zero)
	msb/1                       # function returning the most significant bit of a positive integer (count from zero)
	log10/1                     # function returning log10 of arg
	now/0                       # function returning C-time in secs as integer
	now/1                       # now (-integer) C-time in secs as integer
	get_time/1                  # get_time(-Var) elapsed wall time in secs as float
	cpu_time/1                  # cpu_time(-Var) elapsed CPU time in secs as float

	current_key/1
	string_concat/3				# string_concat(+string,+string,?string)
	string_length/2
	sleep/1                     # sleep time in secs
	split/4                     # split(+string,+sep,?left,?right)
	shell/1
	shell/2
	wall_time/1
	date_time/6
	date_time/7
	loadfile/2                  # loadfile(+filename,-string)
	savefile/2                  # savefile(+filename,+string)
	getfile/2                   # getfile(+filename,-strings)
	getfile/3                   # getfile(+filename,-strings,+opts)
	getline/1                   # getline(-string)
	getline/2                   # getline(+stream,-string)
	getline/3                   # getline(+stream,-string,+opts)
	getlines/1                  # getlines(-strings)
	getlines/2                  # getlines(+stream,-strings)
	getlines/3                  # getlines(+stream,-strings,+opts)
	read_line_to_codes/2	   	# removes terminator
	read_line_to_string/2		# removes terminator
	read_file_to_string/3
	bread/3                     # bread(+stream,?len,-string)
	bflush/1                    # bflush(+stream)
	bwrite/2                    # bwrite(+stream,+string)
	replace/4                   # replace(+string,+old,+new,-string)

	open(stream(Str),...)       # with open/4 reopen a stream
	open(F,M,S,[mmap(Ls)])      # with open/4 mmap() the file to Ls

	reset/3						# parser_reset(:goal,?ball,-cont)
	shift/1						# shift(+ball)

	term_variables/3

Note: consult/1 and load_files/2 support lists of files as args. Also
support loading into modules eg. *consult(MOD:FILE-SPEC)*.

Use these *POSIX* system calls for interprocess creation and
communication...

	popen/3                     # popen(+cmd,+mode,-stream)
	popen/4                     # popen(+cmd,+mode,-stream,+opts)

For example...

```console
tpl -g "use_module(library(apply)),popen('ps -a',read,S,[]),getlines(S,Ls),close(S),maplist(print,Ls),halt"
	PID   TTY      TIME     CMD
	2806  tty2     00:00:00 gnome-session-b
	31645 pts/0    00:00:00 tpl
	31646 pts/0    00:00:00 sh
	31647 pts/0    00:00:00 ps
```

For general *POSIX* process creation use these *SWI-compatible* calls...

	process_create/3			# process_create(+cmd,+args,+opts)
	process_wait/2				# process_wait(+pid,+opts)
	process_wait/1				# process_wait(+pid)
	process_kill/2				# process_kill(+pid,+sigint)
	process_kill/1				# process_kill(+pid)

For example...

```console
	?- process_create('ls',['-l'],[process(Pid)]),process_wait(Pid).
	total 2552
	   4 -rw-rw-r-- 1 andrew andrew    1813 Aug 25 10:18 ATTRIBUTION
	   4 -rw-rw-r-- 1 andrew andrew    1093 Aug 25 10:18 LICENSE
	   8 -rw-rw-r-- 1 andrew andrew    7259 Sep 18 18:27 Makefile
	  24 -rw-rw-r-- 1 andrew andrew   23709 Sep 19 08:56 README.md
	   4 -rw-rw-r-- 1 andrew andrew      28 Aug 25 10:18 _config.yml
	   4 drwxrwxr-x 2 andrew andrew    4096 Sep 17 10:41 docs
	   4 drwxrwxr-x 2 andrew andrew    4096 Sep 18 21:29 library
	   4 drwxrwxr-x 2 andrew andrew    4096 Sep  3 13:02 samples
	   4 drwxrwxr-x 6 andrew andrew    4096 Sep 19 09:38 src
	   4 drwxrwxr-x 5 andrew andrew    4096 Sep 14 20:49 tests
	1448 -rwxrwxr-x 1 andrew andrew 1478712 Sep 19 09:38 tpl
	   8 -rw-rw-r-- 1 andrew andrew    7671 Aug 25 10:18 tpl.c
	  16 -rw-rw-r-- 1 andrew andrew   13928 Sep 18 18:28 tpl.o
	  36 -rw-rw-r-- 1 andrew andrew   33862 Aug 25 10:18 trealla.png
	   Pid = 735602.
	?-
```

Note: read_term/[2,3] supports the positions(Start,End) and the
line_counts(Start,End) property options to report file information.
This is analogous to stream_property/2 use of position(Pos) and
line_count(Line) options.

Note: read_term, write_term & friends support the *json(Boolean)*
option to make more sympathetic support for JSON using the builtin
parsing and printing mechanisms.


Definite Clause Grammars
========================

Uses Ulrich Neumerkel's standard reference library.

	:- use_module(library(dcgs)).


Crypto functions
================

Hash a plain-text data string to a hexadecimal byte string
representing the cryptographic strength hashed value. The options
are *algorithm(Name)* where *Name* can be *sha256*, *sha384* or
*sha512*, and optionally *hmac(Key)* where *Key* is a list of byte
values. This predicate is only available when compiled with OpenSSL...

	crypto_data_hash/3          # crypto_data_hash(+data,-hash,+options)

Generate 'N' random bytes.

	crypto_n_random_bytes(N, Bs) # crypto_n_random_bytes(+integer, -codes)

Convert a hexadecimal string to a byte-list. At least one arg must be
instantiated...

	hex_bytes/2                 # hex_bytes(?hash,?bytes)


Parsing CSV with builtins
=========================

Fast, efficient parsing of CSV files.

Reading:

	parse_csv_line/2			# parse_csv_line(+atom,-list)
	parse_csv_line/3			# parse_csv_line(+atom,-compound,+options)
	parse_csv_file/2			# parse_csv_file(+filename,+options)

Where options can be:

	trim(Boolean)				# default false, trims leading and trailing whitespace
	numbers(Boolean)			# default false, converts integers and floats
	header(Boolean)				# default false, skip first (header) line in file
	comments(Boolean)			# default false, skip lines beginning with comment character in file
	comment(Char)				# default '#', set the comment character
	strings(Boolean)			# default depends on type of input (atom or string)
	arity(Integer)				# default to not checking arity, otherwise throw domain_error
	assert(Boolean)				# default false, assertz to database instead (assumed for files, needs a functor)
	functor(Atom)				# default output is a list, create a structure (mandatory for files and with assert)
	quote(Char)					# default to double-quote
	sep(Char)					# default to comma for .csv or unknown files & TAB for .tsv files

Writing:

	write_csv_file/3			# write_csv_file(+filename,+list,+options)

Where options can be:

	append(Boolean)				# default is to truncate file, or append to file
	strings(Boolean)			# default depends on type of input (atom or string)
	sep(Char)					# default to comma for .csv or unknown files & TAB for .tsv files

Examples...

```console
	? L=[["1 1",12,'1 3'],[],['21','','23']], write_csv_file('x.csv',L,[]).

	$ cat x.csv
	"1 1",12,1 3

	21,,23

	?- Row=["1 1",12,'1 3'], L=[Row], write_csv_file('x.csv',L,[]).

	$ cat x.csv
	"1 1",12,1 3

	?- parse_csv_line('123,2.345,3456789',T).
	   T = ['123','2.345','3456789'].
	?- parse_csv_line("123,2.345,3456789",T).
	   T = ["123","2.345","3456789"].
	?- parse_csv_line('123,2.345,3456789',T,[functor(f)]).
	   T = f('123','2.345','3456789').
	?- parse_csv_line('123,2.345,3456789',T,[functor(f),numbers(true)]).
	   T = f(123,2.345,3456789).
	?- parse_csv_line('abc, abc, a b c ',T).
	   T = [abc,' abc',' a b c '].
	?- parse_csv_line('abc, abc, a b c ',T,[trim(true)]).
	   T = [abc,abc,'a b c'].
	?- parse_csv_line('123,2.345,3456789',T,[functor(f),numbers(true),assert(true)]).
	   true.
	?- f(A,B,C).
	   A = 123, B = 2.345, C = 3456789.
	?- time(parse_csv_file('../logtalk3/library/csv/test_files/tickers.csv',[functor(f),quote('\'')])).
	% Parsed 35193 lines
	% Time elapsed 0.096s, 3 Inferences, 0.000 MLips)
		  true.
	?- f(A,B,C,D,E,F).
	   A = '1125:HK', B = 'OTCGREY', C = 'Stock', D = 'USD', E = '1999-06-22', F = '2019-10-22'
	;  A = '6317:TK', B = 'PINK', C = 'Stock', D = 'USD', E = '2018-06-27', F = '2020-03-02'
	;  A = 'A', B = 'NYSE', C = 'Stock', D = 'USD', E = '1999-11-18', F = '2021-06-25'
	;  A = 'AA', B = 'NYSE', C = 'Stock', D = 'USD', E = '2016-11-01', F = '2021-06-25'
	;  A = 'AA-W', B = 'NYSE', C = 'Stock', D = 'USD', E = '2016-10-18', F = '2016-11-08'
	;  A = 'AAA', B = 'NYSEARCA', C = 'ETF', D = 'USD', E = '2020-09-09', F = '2021-06-25'
	;
```


Application maps (dictionaries)
===============================

Maps use atomic key/value pairs only and are represented as
pseudo-streams:

	map_create/2					# map_create(-skiplist,+opts)
	map_create/1					# map_create(-skiplist)
	map_set/3						# map_set(+skiplist,+key,+value)
	map_get/3						# map_get(+skiplist,+key,?value)
	map_del/2						# map_del(+skiplist,+key)
	map_count/2						# map_count(+skiplist,-count)
	map_list/2						# map_list(+skiplist,?list)
	map_close/1						# map_close(+skiplist)

```console
	$ tpl
	?- map_create(S,[alias(foo)]).
	   S = <$stream>(4).
	?- map_set(foo,1,111), map_set(foo,two,222), map_set(foo,3,333).
	   true.
	?- map_get(foo,3,V).
	   V = 333.
	?- map_del(foo,3).
	   true.
	?- map_list(foo,L).
	   L = [1=111,two=222].
	?- map_close(foo).
	   true.
```

Maps can store virtually unlimited amounts of volatile data in
an efficient indexed manner.

Maps don't require syntactic extensions to Prolog as found in
other non-standard systems.

A possible future extension would be to load a CSV file directly
in a very efficient manner.


HTTP 1.1
========

	:- use_module(library(http)).

	http_get/3				# http_get(Url, Data, Opts)
	http_post/4				# http_post(Url, Data, Opts)
	http_patch/4			# http_patch(Url, Data, Opts)
	http_put/4				# http_put(Url, Data, Opts)
	http_delete/3			# http_delete(Url, Data, Opts)
	http_server/2			# http_server(Goal,Opts),
	http_request/5			# http_request(S, Method, Path, Ver, Hdrs)

```console
	?- http_get("https://github.com/trealla-prolog/trealla", Data, [status_code(Code)]).
	   Data = "\n\n\n\n\n\n<!DOCTYPE html>\n<html\n"||... , Code = 200.
```

A server *Goal* takes a single arg, the connection stream.


Networking					##EXPERIMENTAL##
==========

	http_location/2         # http_location(?list,?url)
	parse_url/2             # parse_url(?url,?list)

```console
	$ tpl
	?- parse_url('http://www.xyz.org:81/hello?msg=Hello+World%21&foo=bar#xyz',P).
	   P = [search([msg='Hello World!',foo=bar]),protocol(http),host('www.xyz.org'),port(81),path('/hello'),fragment(xyz)].
	?- parse_url(U,[search([msg='Hello World!',foo=bar]),protocol(http),host('www.xyz.org'),port(81),path('/hello'),fragment(xyz)]).
	   U = 'http://www.xyz.org:81/hello?msg=Hello+World%21&foo=bar#xyz'.
	?-
```

	server/2                # server(+host,-stream)
	server/3                # server(+host,-stream,+list)
	accept/2                # accept(+stream,-stream)
	client/2                # client(+url,-stream)
	client/4                # client(+url,-host,-path,-stream)
	client/5                # client(+url,-host,-path,-stream,+list)

The options list can include *udp(bool)* (default is false),
*nodelay(bool)* (default is true), *ssl(bool)* (default is false)
and *certfile(filespec)*.

Additional server options can include *keyfile(filespec)*. If just
one concatenated file (keyfile+certfiles) is supplied, use
*keyfile(filespec)* only.

Optional schemes 'unix://', 'http://' (the default) and 'https://'
can be provided in the client URL.

With *bread/3* the 'len' arg can be an integer > 0 meaning return that
many bytes, = 0 meaning return whatever is there (if non-blocking) or
a var meaning return all bytes until end end of file,


Simple regular expressions
==========================

This is meant as a place-holder until a proper regex package is included.

	sre_compile/2				# sre_compile(+pattern,-reg)
	sre_matchp/4				# sre_matchp(+reg,+text,-match,-rest)
	sre_substp/4				# sre_substp(+reg,+text,-prefix,-rest)

	sre_match/4					# sre_match(+pattern,+text,-match,-rest)
	sre_match_all/3				# sre_matchall(+pattern,+text,-list)
	sre_match_all_pos/3			# sre_matchall_pos(+pattern,+text,-pairs)

	sre_match_all_in_file/3		# sre_matchall_in_file(+pattern,+filename,-list)
	sre_match_all_pos_in_file/3 # sre_matchall_pos_in_file(+pattern,+filename,-pairs)

	sre_subst/4					# sre_subst(+pattern,+text,-prefix,-rest)
	sre_subst_all/4				# sre_subst(+pattern,+text,+subst,-text)

	sre_subst_all_in_file/4		# sre_subst_in_file(+pattern,+filename,+subst,-text)

```
	 * Supports:
	 * ---------
	 *   '.'        Dot, matches any character
	 *   '^'        Start anchor, matches beginning of string
	 *   '$'        End anchor, matches end of string
	 *   '*'        Asterisk, match zero or more (greedy)
	 *   '+'        Plus, match one or more (greedy)
	 *   '?'        Question, match zero or one (non-greedy)
	 *   '[abc]'    Character class, match if one of {'a', 'b', 'c'}
	 *   '[^abc]'   Inverted class, match if NOT one of {'a', 'b', 'c'}
	 *   '[a-zA-Z]' Character ranges, the character set of the ranges { a-z | A-Z }
	 *   '\s'       Whitespace, \t \f \r \n \v and spaces
	 *   '\S'       Non-whitespace
	 *   '\w'       Alphanumeric, [a-zA-Z0-9_]
	 *   '\W'       Non-alphanumeric
	 *   '\d'       Digits, [0-9]
	 *   '\D'       Non-digits
```

For example...

```console
	?- sre_compile("d.f", Reg), sre_matchp(Reg, "abcdefghi", M, Rest).
	   Reg = <$blob>(0x6AC5AAF0), M = "def", Rest = "ghi".

	?- sre_match("d.f", "abcdefghi", M, Rest).
	   M = "def", Rest = "ghi".

	?- sre_match_all("d.f", "xdafydbfzdcf-", L).
	   L = ["daf","dbf","dcf"].

	?- sre_match_all_pos("d.f", "xdafydbfzdcf-", L).
	   L = [1-3,2-3,3-3].

	?- sre_match_all("d[^c]f", "xdafydbfzdcfxddf-", L).
	   L = ["daf","dbf","ddf"].

	?- sre_subst("d.f", "xdafydbfzdcf-", P, L).
	   P = "x", L = "ydbfzdcf-".

	?- sre_subst_all("d.f", "xdafydbfzdcf-", "$", L).
	   L = "x$y$z$-".

	?- sre_match_all("\\S", "Needle In A Haystack", L).
	   L = ["N","e","e","d","l","e","I","n","A",...].

	?- sre_match_all_pos("\\s", "Needle In A Haystack", L).
	   L = [6-1,9-1,11-1].

	?- time(sre_match_all_in_file("t\\We",'thesaurus.txt',L)),
		length(L,Len),
		format("Occurrs: ~w times~n",[Len]),
		halt.
	Time elapsed 0.0463s
	Occurrs: 749 times
```

Note: if no match is found the returned *match*, *text* (and *list*) is *[]*
indicating an empty string.

Note: if the input *text* arg is a string then the output *text* arg
is a no-copy slice of the string. So if the input is a memory-mapped
file then regex searches can be performed quickly and efficiently over
huge files.


Foreign Function Interface (libffi)
===================================

Allows the loading of dynamic libraries and calling of foreign functions
written in C from within Prolog...

	'$dlopen'/3 			# '$dlopen(+name, +flag, -handle)

These predicates register a foreign function as a builtin and use a
wrapper to validate arg types at call/runtime...

	'$register_function'/4		# '$ffi_reg'(+handle,+symbol,+types,+ret_type)
	'$register_predicate'/4		# '$ffi_reg'(+handle,+symbol,+types,+ret_type)

The allowed types are
*sint8*, *sint16*, *sint32*, *sint64*, *sint* (native *signed int*),
*uint8*, *uint16*, *uint32*, *uint64*, *uint* (native *unsigned int*),
*ushort*, *sshort*, *float*, *double*,
*bool*, (use integer 0/1 to align with C *bool* pseudo-type)
*void* (a return type only),
*cstr* (a char pointer),
and *ptr* (for arbitrary pointers/handles).

Assuming the following C-code in *samples/foo.c*:

```c
	double foo(double x, int64_t y)
	{
		return pow(x, (double)y);
	}

	int bar(double x, int64_t y, double *result)
	{
		*result = pow(x, (double)y);
		return 0;
	}

	char *baz(const char *x, const char *y)
	{
		char *s = malloc(strlen(x) + strlen(y) + 1);
		strcpy(s, x);
		strcat(s, y);
		return s;
	}
```

```console
	$ gcc -fPIC -c foo.c
	$ gcc -shared -o libfoo.so foo.o
```

Register a builtin function...

```console
	?- '$dlopen'('samples/libfoo.so', 0, H),
		'$register_function'(H, foo, [double, sint64], double).
	   H = 94051868794416.
	?- R is foo(2.0, 3).
	   R = 8.0.
	?- R is foo(abc,3).
	   error(type_error(float,abc),foo/2).
```

Register a builtin predicate...

```console
	?- '$dlopen'('samples/libfoo.so', 0, H),
		'$register_predicate'(H, bar, [double, sint64, -double], sint64),
		'$register_predicate'(H, baz, [cstr, cstr], cstr),
	   H = 94051868794416.
	?- bar(2.0, 3, X, Return).
	   X = 8.0, Return = 0.
	?- baz('abc', '123', Return).
	   Return = abc123.
```

Note: the foreign function return value is passed as an extra argument
to the predicate call, unless it was specified to be of type *void*.


Foreign Module Interface (libffi)
=================================

This is a simplified interface to FFIs inspired by Adrián Arroyo Calle
and largely supercedes the implementation given above.

	foreign_struct(+atom, +list)
	use_foreign_module(+atom, +list)

For example...

```prolog
	:- use_foreign_module('samples/libfoo.so', [
		bar([double, sint64, -double], sint64),
		baz([cstr, cstr], cstr)
	]).
```

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


ISO Prolog Multithreading
=========================

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
	inp/1                           # inp(?tuple)
	rdp/1                           # rdp(?tuple)
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
		forall(rdp({msg:_}), sleep(0.001)),
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

	future/3          # Make a Future from a Prolog goal.
	future_all/2      # Make a Future that resolves to a list of the results of an input list of futures.
	future_any/2      # Make a Future that resolves as soon as any of the futures in a list succeeds.
	future_cancel/1   # Cancel unfinished future.
	future_done/1     # Check if a future finished.
	await/2           # Wait for a Future.

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
