GIT_VERSION := "$(shell git describe --abbrev=4 --dirty --always --tags)"

CFLAGS =  -std=gnu99 -Isrc -I/usr/local/include -DVERSION='$(GIT_VERSION)' -O3 \
	-funsigned-char $(OPT) -D_GNU_SOURCE \
	-Wall -Wextra \
	-Wno-deprecated-declarations \
	-Wno-unused-function -Wno-unused-parameter \
	-Wno-unused-variable

LDFLAGS = -L/usr/local/lib -lm

ifdef HOMEBREW_PREFIX
LDFLAGS += -L$(HOMEBREW_PREFIX)/opt/libffi/lib -L$(HOMEBREW_PREFIX)/opt/openssl@3/lib
CFLAGS += -I$(HOMEBREW_PREFIX)/opt/libffi/include -I$(HOMEBREW_PREFIX)/opt/openssl@3/include
endif

ifndef TPL
TPL = tpl
endif

ifdef WASI
CFLAGS += -std=c11 -Isrc/wasm \
	-D_WASI_EMULATED_MMAN -D_WASI_EMULATED_SIGNAL \
	-D_WASI_EMULATED_PROCESS_CLOCKS
LDFLAGS += -lwasi-emulated-mman -lwasi-emulated-signal \
	-lwasi-emulated-process-clocks -Wl,--stack-first \
	-Wl,-zstack-size=8388608 -Wl,--initial-memory=100663296
NOFFI = 1
NOSSL = 1
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

ifdef THREADS
CFLAGS += -DUSE_THREADS=1 -pthread
LDFLAGS += -pthread
endif

ifdef LTO
CFLAGS += -flto=$(LTO)
LDFLAGS += -flto=$(LTO)
endif

ifndef WASMOPT
WASMOPT = wasm-opt
endif

SRCOBJECTS = tpl.o \
	src/bags.o \
	src/base64.o \
	src/contrib.o \
	src/control.o \
	src/ffi.o \
	src/format.o \
	src/functions.o \
	src/heap.c \
	src/history.o \
	src/library.o \
	src/module.o \
	src/network.o \
	src/parser.o \
	src/posix.o \
	src/predicates.o \
	src/print.o \
	src/prolog.o \
	src/query.o \
	src/skiplist.o \
	src/streams.o \
	src/toplevel.o \
	src/unify.o \
	src/utf8.o \
	src/wasi.o

LIBOBJECTS +=  \
	library/abnf.o \
	library/apply.o \
	library/assoc.o \
	library/atts.o \
	library/builtins.o \
	library/charsio.o \
	library/dcgs.o \
	library/dict.o \
	library/dif.o \
	library/error.o \
	library/format.o \
	library/freeze.o \
	library/http.o \
	library/json.o \
	library/lambda.o \
	library/lists.o \
	library/ordsets.o \
	library/pairs.o \
	library/pio.o \
	library/pseudojson.o \
	library/random.o \
	library/raylib.o \
	library/si.o \
	library/spin.o \
	library/sqlite3.o \
	library/ugraphs.o \
	library/wasm.o \
	library/wasm_generic.o \
	library/wasm_js.o \
	library/when.o

SRCOBJECTS += src/imath/imath.o
SRCOBJECTS += src/sre/re.o

ifdef WASI_TARGET_SPIN
SRCOBJECTS += src/wasm/spin.o
SRCOBJECTS += src/wasm/spin-http.o
SRCOBJECTS += src/wasm/wasi-outbound-http.o
endif

ifdef ISOCLINE
SRCOBJECTS += src/isocline/src/isocline.o
endif

OBJECTS = $(SRCOBJECTS) $(LIBOBJECTS) src/version.o

library/%.c: library/%.pl
	xxd -i $^ $@

all: tpl

tpl: $(OBJECTS) Makefile README.md LICENSE
	rm src/version.o
	$(CC) $(CFLAGS) -o src/version.o -c src/version.c
	$(CC) $(CFLAGS) -o $(TPL) $(OBJECTS) $(OPT) $(LDFLAGS)

profile:
	$(MAKE) 'OPT=$(OPT) -O0 -pg -DDEBUG'

debug:
	$(MAKE) 'OPT=$(OPT) -O0 -g -DDEBUG'

release:
	$(MAKE) 'OPT=$(OPT) -DNDEBUG'

tpl.wasm:
	$(MAKE) WASI=1 TPL=tpl.wasm 'OPT=$(OPT) -DNDEBUG'

wasm: tpl.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o tpl-wizened.wasm tpl.wasm
	$(WASMOPT) --enable-bulk-memory tpl-wizened.wasm -o tpl.wasm -O4
	rm tpl-wizened.wasm

libtpl.wasm:
	$(MAKE) WASI=1 TPL=libtpl.wasm 'OPT=$(OPT) -DNDEBUG -DWASI_IMPORTS -DWASI_TARGET_GENERIC'

libtpl-js.wasm:
	$(MAKE) WASI=1 TPL=libtpl-js.wasm 'OPT=$(OPT) -DNDEBUG -DWASI_IMPORTS -DWASI_TARGET_JS'

libtpl-spin.wasm:
	$(MAKE) WASI=1 WASI_TARGET_SPIN=1 TPL=libtpl-spin.wasm 'OPT=$(OPT) -DNDEBUG -DWASI_TARGET_SPIN'

libtpl: libtpl.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o libtpl-wizened.wasm libtpl.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl.wasm -O4
	rm libtpl-wizened.wasm

libtpl-js: libtpl-js.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o libtpl-wizened.wasm libtpl-js.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl-js.wasm -Oz
	rm libtpl-wizened.wasm

libtpl-spin: libtpl-spin.wasm
	$(WIZER) --wasm-bulk-memory true --allow-wasi --dir . -o libtpl-wizened.wasm libtpl-spin.wasm
	$(WASMOPT) --enable-bulk-memory libtpl-wizened.wasm -o libtpl-spin.wasm -O4
	rm libtpl-wizened.wasm

wit:
	wit-bindgen guest c --export $(SPINDIR)/wit/ephemeral/spin-http.wit --out-dir ./src/wasm/
	wit-bindgen guest c --import $(SPINDIR)/wit/ephemeral/wasi-outbound-http.wit --out-dir ./src/wasm/
	wit-bindgen guest c --import $(SPINDIR)/wit/ephemeral/outbound-pg.wit --out-dir ./src/wasm/
	sed -i 's/<spin-http.h>/"spin-http.h"/' ./src/wasm/spin-http.c
	sed -i 's/<wasi-outbound-http.h>/"wasi-outbound-http.h"/' ./src/wasm/wasi-outbound-http.c
	sed -i 's/<outbound-pg.h>/"outbound-pg.h"/' ./src/wasm/outbound-pg.c

test:
	./tests/run.sh

check:
	./tests/run_valgrind.sh

check-leaks:
	./tests/run_valgrind_leaks.sh

clean:
	rm -f tpl tpl.wasm tpl*.wasm libtpl*.wasm \
		src/*.o src/imath/*.o src/isocline/src/*.o src/sre/*.o src/wasm/*.o \
		library/*.o library/*.c *.o samples/*.o samples/*.so \
		vgcore.* *.core core core.* *.exe gmon.*
	rm -f *.itf *.po samples/*.itf samples/*.po

# from [gcc|clang] -MM src/*.c src/imath/*.c src/isocline/src/*.c src/sre/*.c

src/bags.o: src/bags.c src/heap.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/module.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/base64.o: src/base64.c src/base64.h
src/contrib.o: src/contrib.c src/trealla.h src/internal.h src/map.h \
  src/skiplist.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/query.h src/builtins.h
src/control.o: src/control.c src/heap.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/module.h src/parser.h src/prolog.h src/query.h \
  src/builtins.h
src/ffi.o: src/ffi.c src/prolog.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/query.h src/builtins.h
src/format.o: src/format.c src/network.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/query.h src/builtins.h src/utf8.h
src/functions.o: src/functions.c src/heap.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/module.h src/prolog.h src/query.h src/builtins.h
src/heap.o: src/heap.c src/heap.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/query.h src/builtins.h
src/history.o: src/history.c src/history.h src/utf8.h src/cdebug.h
src/library.o: src/library.c src/library.h
src/module.o: src/module.c src/module.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/parser.h src/prolog.h src/query.h src/builtins.h \
  src/utf8.h
src/network.o: src/network.c src/network.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/query.h src/builtins.h
src/parser.o: src/parser.c src/heap.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/library.h src/module.h src/parser.h src/prolog.h src/query.h \
  src/builtins.h src/utf8.h
src/posix.o: src/posix.c src/trealla.h src/internal.h src/map.h \
  src/skiplist.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/heap.h src/prolog.h src/query.h src/builtins.h
src/predicates.o: src/predicates.c src/base64.h src/heap.h src/internal.h \
  src/map.h src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/history.h src/library.h src/module.h src/sre/re.h \
  src/parser.h src/prolog.h src/query.h src/builtins.h src/utf8.h
src/print.o: src/print.c src/heap.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/module.h src/network.h src/parser.h src/query.h src/builtins.h \
  src/utf8.h
src/prolog.o: src/prolog.c src/library.h src/module.h src/internal.h \
  src/map.h src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/parser.h src/prolog.h src/query.h src/builtins.h
src/query.o: src/query.c src/heap.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/module.h src/network.h src/parser.h src/prolog.h src/query.h \
  src/builtins.h src/utf8.h
src/skiplist.o: src/skiplist.c src/skiplist.h
src/streams.o: src/streams.c src/heap.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/module.h src/network.h src/parser.h src/prolog.h \
  src/query.h src/builtins.h src/utf8.h
src/toplevel.o: src/toplevel.c src/heap.h src/internal.h src/map.h \
  src/skiplist.h src/trealla.h src/cdebug.h src/stringbuf.h \
  src/imath/imath.h src/history.h src/module.h src/parser.h src/prolog.h \
  src/query.h src/builtins.h src/utf8.h
src/unify.o: src/unify.c src/heap.h src/internal.h src/map.h src/skiplist.h \
  src/trealla.h src/cdebug.h src/stringbuf.h src/imath/imath.h \
  src/module.h src/query.h src/builtins.h src/utf8.h
src/utf8.o: src/utf8.c src/utf8.h
src/wasi.o: src/wasi.c
src/version.o: src/version.c
src/imath.o: src/imath/imath.c src/imath/imath.h
src/re.o: src/sre/re.c src/sre/re.h
src/isocline.o: src/isocline/src/isocline.c src/isocline/src/attr.c \
  src/isocline/src/common.h src/isocline/src/../include/isocline.h \
  src/isocline/src/stringbuf.h src/isocline/src/attr.h \
  src/isocline/src/term.h src/isocline/src/tty.h \
  src/isocline/src/bbcode.c src/isocline/src/bbcode.h \
  src/isocline/src/bbcode_colors.c src/isocline/src/editline.c \
  src/isocline/src/env.h src/isocline/src/history.h \
  src/isocline/src/completions.h src/isocline/src/undo.h \
  src/isocline/src/highlight.h src/isocline/src/editline_help.c \
  src/isocline/src/editline_history.c \
  src/isocline/src/editline_completion.c src/isocline/src/highlight.c \
  src/isocline/src/undo.c src/isocline/src/history.c \
  src/isocline/src/completers.c src/isocline/src/completions.c \
  src/isocline/src/term.c src/isocline/src/term_color.c \
  src/isocline/src/tty_esc.c src/isocline/src/tty.c \
  src/isocline/src/stringbuf.c src/isocline/src/wcwidth.c \
  src/isocline/src/common.c
