GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"
COMPILER_IS_GCC := $(shell $(CC) --version | grep -E -o 'g?cc')

CFLAGS = -Isrc -I/usr/local/include -DVERSION='$(GIT_VERSION)' \
	-funsigned-char \
	-O3 $(OPT) -D_GNU_SOURCE \
	-Wall -Wextra \
	-Wno-unused-parameter \
	-Wno-unused-variable
LDFLAGS = -L/usr/local/lib -lm

ifdef HOMEBREW_PREFIX
LDFLAGS += -L$(HOMEBREW_PREFIX)/opt/libffi/lib -L$(HOMEBREW_PREFIX)/opt/openssl@3/lib
CFLAGS += -I$(HOMEBREW_PREFIX)/opt/libffi/include -I$(HOMEBREW_PREFIX)/opt/openssl@3/include
endif

ifndef TPL
TPL = tpl
endif

ifndef NOPEDANTIC
CFLAGS += -Wno-unused-but-set-variable
endif

ifdef WASI
CFLAGS += -std=c11 -Isrc/wasm \
	-D_WASI_EMULATED_MMAN -D_WASI_EMULATED_SIGNAL \
	-D_WASI_EMULATED_PROCESS_CLOCKS
LDFLAGS += -lwasi-emulated-mman -lwasi-emulated-signal \
	-lwasi-emulated-process-clocks -Wl,--stack-first \
	-Wl,-zstack-size=4194304
NOFFI = 1
NOSSL = 1
NOTHREADS = 1
ifdef WASI_CC
CC = $(WASI_CC)
endif
endif

ifndef WIZER
WIZER = wizer
endif
ifndef WASMOPT
WASMOPT = wasm-opt
endif
ifndef SPINDIR
SPINDIR = ../spin
endif
ifdef WIN
ISOCLINE = 1
# CC = x86_64-w64-mingw32-gcc
endif

ifdef ISOCLINE
CFLAGS += -DUSE_ISOCLINE=1
else
ifndef WASI
LDFLAGS += -lreadline
endif
endif

ifndef NOFFI
CFLAGS += -DUSE_FFI=1 -I/usr/local/opt/libffi/include
LDFLAGS += -lffi -ldl
endif

ifndef NOSSL
CFLAGS += -DUSE_OPENSSL=1 -I/usr/local/opt/openssl/include
LDFLAGS += -L/usr/local/opt/openssl/lib -lssl -lcrypto
endif

ifdef NORATIONAL_TREES
CFLAGS += -DUSE_RATIONAL_TREES=0
endif

ifndef NOTHREADS
CFLAGS += -DUSE_THREADS=1 -pthread
LDFLAGS += -pthread
# -latomic only works for gcc
ifeq ($(COMPILER_IS_GCC),gcc)
LDFLAGS += -latomic
else
LDFLAGS +=
endif
endif

ifdef LTO
CFLAGS += -flto=$(LTO)
LDFLAGS += -flto=$(LTO)
endif

ifndef WASMOPT
WASMOPT = wasm-opt
endif

SRCOBJECTS = tpl.o \
	src/base64.o \
	src/bif_atts.o \
	src/bif_bboard.o \
	src/bif_contrib.o \
	src/bif_control.o \
	src/bif_csv.o \
	src/bif_database.o \
	src/bif_ffi.o \
	src/bif_format.o \
	src/bif_functions.o \
	src/bif_maps.o \
	src/bif_os.o \
	src/bif_posix.o \
	src/bif_predicates.o \
	src/bif_sort.o \
	src/bif_sregex.o \
	src/bif_streams.o \
	src/bif_tasks.o \
	src/bif_threads.o \
	src/compile.o \
	src/heap.o \
	src/history.o \
	src/library.o \
	src/list.o \
	src/module.o \
	src/network.o \
	src/parser.o \
	src/print.o \
	src/prolog.o \
	src/query.o \
	src/skiplist.o \
	src/terms.o \
	src/toplevel.o \
	src/unify.o \
	src/wasi.o \
	src/utf8.o \
	src/version.o

LIBOBJECTS +=  \
	library/abnf.o \
	library/aggregate.o \
	library/arithmetic.o \
	library/assoc.o \
	library/atts.o \
	library/builtins.o \
	library/charsio.o \
	library/concurrent.o \
	library/clpz.o \
	library/curl.o \
	library/dcgs.o \
	library/debug.o \
	library/dict.o \
	library/dif.o \
	library/error.o \
	library/format.o \
	library/freeze.o \
	library/gensym.o \
	library/gsl.o \
	library/heaps.o \
	library/http.o \
	library/iso_ext.o \
	library/json.o \
	library/lambda.o \
	library/linda.o \
	library/lists.o \
	library/ordsets.o \
	library/pairs.o \
	library/pio.o \
	library/pseudojson.o \
	library/random.o \
	library/raylib.o \
	library/rbtrees.o \
	library/reif.o \
	library/si.o \
	library/sqlite3.o \
	library/time.o \
	library/ugraphs.o \
	library/uuid.o \
	library/wasm.o \
	library/wasm_generic.o \
	library/wasm_js.o \
	library/when.o

SRCOBJECTS += src/imath/imath.o
SRCOBJECTS += src/imath/imrat.o
SRCOBJECTS += src/sre/re.o

ifdef WASI_TARGET_SPIN
SRCOBJECTS += src/wasm/spin.o \
	src/wasm/spin-http.o \
	src/wasm/wasi-outbound-http.o \
	src/wasm/key-value.o \
	src/wasm/outbound-pg.o \
	src/wasm/sqlite.o
LIBOBJECTS += library/spin.o
endif

ifdef ISOCLINE
SRCOBJECTS += src/isocline/src/isocline.o
endif

OBJECTS = $(SRCOBJECTS) $(LIBOBJECTS)

library/%.c: library/%.pl
	echo '#include <stddef.h>' > $@
	xxd -i $^ >> $@

.PHONY: test

all: tpl

tpl: $(OBJECTS) Makefile README.md LICENSE
	rm src/version.o
	$(CC) $(CFLAGS) -o src/version.o -c src/version.c
	$(CC) $(CFLAGS) -o $(TPL) $(OBJECTS) $(OPT) $(LDFLAGS)

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g3 -DDEBUG'

release:
	$(MAKE) 'OPT=$(OPT) -DNDEBUG'

install:
	ln -s ./tpl ~/bin/tpl

tpl.wasm:
	$(MAKE) WASI=1 TPL=tpl.wasm 'OPT=$(OPT) -DNDEBUG'

tpl-debug.wasm:
	$(MAKE) WASI=1 TPL=tpl-debug.wasm 'OPT=-O0 -g -DDEBUG'

wasm: tpl.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o tpl-wizened.wasm tpl.wasm
	$(WASMOPT) --enable-bulk-memory tpl-wizened.wasm -o tpl.wasm -O4
	rm tpl-wizened.wasm

libtpl.wasm:
	$(MAKE) WASI=1 TPL=libtpl.wasm 'OPT=$(OPT) -DNDEBUG -DWASI_IMPORTS -DWASI_TARGET_GENERIC'

libtpl-debug.wasm:
	$(MAKE) WASI=1 TPL=libtpl-debug.wasm 'OPT=-O0 -g -DWASI_IMPORTS -DWASI_TARGET_GENERIC -DDEBUG'

libtpl-js.wasm:
	$(MAKE) WASI=1 TPL=libtpl-js.wasm 'OPT=$(OPT) -DNDEBUG -DWASI_IMPORTS -DWASI_TARGET_GENERIC -DWASI_TARGET_JS'
	# $(MAKE) WASI=1 TPL=libtpl-js.wasm 'OPT=-O0 -g -DDEBUG -DWASI_IMPORTS -DWASI_TARGET_GENERIC -DWASI_TARGET_JS'

libtpl-spin.wasm:
	$(MAKE) WASI=1 WASI_TARGET_SPIN=1 TPL=libtpl-spin.wasm 'OPT=$(OPT) -DNDEBUG -DWASI_TARGET_SPIN'

libtpl: libtpl.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o libtpl-wizened.wasm libtpl.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl.wasm -O4
	rm libtpl-wizened.wasm

libtpl-js: libtpl-js.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o libtpl-wizened.wasm libtpl-js.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl-js.wasm -O4
	rm libtpl-wizened.wasm

libtpl-js-slim: libtpl-js.wasm
	cp libtpl-js.wasm libtpl-wizened.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl-js.wasm -O4
	rm libtpl-wizened.wasm

libtpl-spin: libtpl-spin.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o libtpl-wizened.wasm libtpl-spin.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl-spin.wasm -O4
	rm libtpl-wizened.wasm

wit:
	wit-bindgen c --export $(SPINDIR)/wit/ephemeral/spin-http.wit --out-dir ./src/wasm/
	wit-bindgen c --import $(SPINDIR)/wit/ephemeral/wasi-outbound-http.wit --out-dir ./src/wasm/
	wit-bindgen c --import $(SPINDIR)/wit/ephemeral/key-value.wit --out-dir ./src/wasm/
	wit-bindgen c --import $(SPINDIR)/wit/ephemeral/outbound-pg.wit --out-dir ./src/wasm/
	wit-bindgen c --import $(SPINDIR)/wit/ephemeral/sqlite.wit --out-dir ./src/wasm/
	sed -i '' -e 's/<spin-http.h>/"spin-http.h"/' ./src/wasm/spin-http.c
	sed -i '' -e 's/<wasi-outbound-http.h>/"wasi-outbound-http.h"/' ./src/wasm/wasi-outbound-http.c
	sed -i '' -e 's/<key-value.h>/"key-value.h"/' ./src/wasm/key-value.c
	sed -i '' -e 's/<outbound-pg.h>/"outbound-pg.h"/' ./src/wasm/outbound-pg.c
	sed -i '' -e 's/<sqlite.h>/"sqlite.h"/' ./src/wasm/sqlite.c

test:
	./tests/run.sh

check:
	./tests/run_valgrind.sh

leaks:
	./tests/run_valgrind_leaks.sh

clean:
	rm -f tpl tpl.wasm tpl*.wasm libtpl*.wasm \
		src/*.o src/imath/*.o src/isocline/src/*.o src/sre/*.o src/wasm/*.o \
		src/utf8/*.o \
		library/*.o library/*.c *.o samples/*.o samples/*.so \
		vgcore.* *.core core core.* *.exe gmon.* \
		samples/*.xwam
	rm -f *.itf *.po *.xwam samples/*.itf samples/*.po

# from [gcc|clang] -MM src/*.c src/imath/*.c src/isocline/src/*.c src/sre/*.c

src/base64.o: src/base64.c src/base64.h
src/bif_atts.o: src/bif_atts.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/module.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/bif_bboard.o: src/bif_bboard.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/module.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/bif_contrib.o: src/bif_contrib.c src/threads.h src/trealla.h src/internal.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/query.h src/parser.h src/builtins.h
src/bif_control.o: src/bif_control.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/module.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/bif_csv.o: src/bif_csv.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/module.h src/query.h src/parser.h src/builtins.h
src/bif_database.o: src/bif_database.c src/base64.h src/threads.h src/heap.h src/internal.h \
 src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/skiplist.h src/list.h \
 src/utf8.h src/history.h src/library.h src/module.h src/parser.h \
 src/prolog.h src/query.h src/builtins.h
src/bif_ffi.o: src/bif_ffi.c src/prolog.h src/threads.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/module.h src/query.h src/parser.h src/builtins.h src/heap.h
src/bif_format.o: src/bif_format.c src/network.h src/threads.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/query.h src/parser.h src/builtins.h
src/bif_functions.o: src/bif_functions.c src/threads.h src/heap.h src/internal.h \
 src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/skiplist.h src/list.h \
 src/utf8.h src/module.h src/prolog.h src/query.h src/parser.h \
 src/builtins.h
src/bif_maps.o: src/bif_maps.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/prolog.h src/query.h src/parser.h src/builtins.h
src/bif_os.o: src/bif_os.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/prolog.h src/query.h src/parser.h src/builtins.h
src/bif_posix.o: src/bif_posix.c src/threads.h src/trealla.h src/internal.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/heap.h \
 src/prolog.h src/query.h src/parser.h src/builtins.h
src/bif_predicates.o: src/bif_predicates.c src/threads.h src/base64.h src/heap.h \
 src/internal.h src/trealla.h src/cdebug.h src/stringbuf.h \
 src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/history.h src/library.h \
 src/module.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/bif_sort.o: src/bif_sort.c src/threads.h src/base64.h src/heap.h \
 src/internal.h src/trealla.h src/cdebug.h src/stringbuf.h \
 src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/history.h src/library.h \
 src/module.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/bif_sregex.o: src/bif_sregex.c src/threads.h src/history.h src/trealla.h src/prolog.h \
 src/internal.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/sre/re.h src/skiplist.h src/list.h \
 src/utf8.h src/query.h src/parser.h src/builtins.h
src/bif_streams.o: src/bif_streams.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/module.h src/network.h src/parser.h src/prolog.h src/query.h \
 src/builtins.h
src/bif_tasks.o: src/bif_tasks.c src/base64.h src/threads.h src/heap.h src/internal.h \
 src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/skiplist.h src/list.h \
 src/utf8.h src/history.h src/library.h src/module.h src/parser.h \
 src/prolog.h src/query.h src/builtins.h
src/bif_threads.o: src/bif_threads.c src/threads.h src/heap.h src/internal.h \
 src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/skiplist.h src/list.h \
 src/utf8.h src/history.h src/library.h src/module.h src/parser.h \
 src/prolog.h src/query.h src/builtins.h
src/compile.o: src/compile.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/history.h src/library.h src/module.h src/parser.h src/prolog.h \
 src/query.h src/builtins.h
src/heap.o: src/heap.c src/heap.h src/threads.h src/internal.h src/trealla.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/prolog.h \
 src/query.h src/parser.h src/builtins.h
src/history.o: src/history.c src/internal.h src/trealla.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/history.h \
 src/prolog.h
src/library.o: src/library.c src/library.h
src/list.o: src/list.c src/list.h
src/module.o: src/module.c src/threads.h src/module.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/parser.h src/history.h src/library.h src/prolog.h src/query.h \
 src/builtins.h
src/network.o: src/network.c src/threads.h src/history.h src/trealla.h src/network.h \
 src/internal.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/skiplist.h src/list.h \
 src/utf8.h src/query.h src/parser.h src/builtins.h
src/parser.o: src/parser.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/history.h src/library.h src/module.h src/parser.h src/prolog.h \
 src/query.h src/builtins.h
src/print.o: src/print.c src/threads.h src/heap.h src/internal.h src/trealla.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/module.h \
 src/network.h src/parser.h src/query.h src/builtins.h
src/prolog.o: src/prolog.c src/threads.h src/library.h src/module.h src/internal.h \
 src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
 src/imath/imrat.h src/imath/imath.h src/skiplist.h src/list.h \
 src/utf8.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/query.o: src/query.c src/threads.h src/heap.h src/internal.h src/trealla.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/module.h \
 src/network.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/skiplist.o: src/skiplist.c src/threads.h src/skiplist.h src/list.h
src/terms.o: src/terms.c src/threads.h src/heap.h src/internal.h src/trealla.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/query.h \
 src/parser.h src/builtins.h
src/toplevel.o: src/toplevel.c src/threads.h src/heap.h src/internal.h src/trealla.h \
 src/cdebug.h src/stringbuf.h src/imath/imath.h src/imath/imrat.h \
 src/imath/imath.h src/skiplist.h src/list.h src/utf8.h \
 src/history.h src/module.h src/prolog.h src/query.h src/parser.h \
 src/builtins.h
src/unify.o: src/unify.c src/threads.h src/heap.h src/internal.h src/trealla.h src/cdebug.h \
 src/stringbuf.h src/imath/imath.h src/imath/imrat.h src/imath/imath.h \
 src/skiplist.h src/list.h src/utf8.h src/query.h \
 src/parser.h src/builtins.h
src/version.o: src/version.c
src/wasi.o: src/wasi.c
src/utf8.o: src/utf8.c src/utf8.h
