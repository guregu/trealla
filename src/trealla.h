#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __wasi__
#define EXPORT(name) __attribute__((export_name(#name)))
#else
#define EXPORT(name)
#endif

typedef struct prolog_ prolog;
typedef struct {} pl_sub_query;

EXPORT(pl_create)
extern prolog *pl_create();
EXPORT(pl_destroy)
extern void pl_destroy(prolog*);

EXPORT(pl_eval)
extern bool pl_eval(prolog*, const char *expr, bool interactive);
EXPORT(pl_consult)
extern bool pl_consult(prolog*, const char *filename);
EXPORT(pl_consult_fp)
extern bool pl_consult_fp(prolog*, FILE *fp, const char *filename);
EXPORT(pl_isatty)
extern bool pl_isatty(prolog*);
EXPORT(pl_stdin)
extern FILE *pl_stdin(prolog*);
extern bool pl_restore(prolog*, const char *filename);
extern bool pl_logging(prolog*, const char *filename);

EXPORT(pl_query)
extern bool pl_query(prolog*, const char *expr, pl_sub_query **q, unsigned int yield_time_in_ms);
EXPORT(pl_yield_at)
extern bool pl_yield_at(pl_sub_query *q, unsigned int time_in_ms);
EXPORT(pl_did_yield)
extern bool pl_did_yield(pl_sub_query *q);
EXPORT(pl_redo)
extern bool pl_redo(pl_sub_query *q);
EXPORT(pl_done)
extern bool pl_done(pl_sub_query *q);	// only call if redo still active


// These are for Wasm runtimes without output capturing support
EXPORT(pl_capture)
extern void pl_capture(prolog *pl);
EXPORT(pl_capture_read)
extern void pl_capture_read(prolog *pl, char **stdout_str, int32_t *stdout_len, char **stderr_str, int32_t *stderr_len);
EXPORT(pl_capture_free)
extern void pl_capture_free(prolog *pl);
EXPORT(pl_capture_reset)
extern void pl_capture_reset(prolog *pl);

// TODO: delete
EXPORT(query_did_yield)
extern bool query_did_yield(pl_sub_query *q);

EXPORT(get_halt_code)
extern int get_halt_code(prolog*);
EXPORT(get_halt)
extern bool get_halt(prolog*);
EXPORT(get_error)
extern bool get_error(prolog*);
EXPORT(get_status)
extern bool get_status(prolog*);
EXPORT(get_redo)
extern bool get_redo(prolog*);
EXPORT(did_dump_vars)
extern bool did_dump_vars(prolog*);

EXPORT(set_trace)
extern void set_trace(prolog*);
EXPORT(set_quiet)
extern void set_quiet(prolog*);
EXPORT(set_noindex)
extern void set_noindex(prolog*);
EXPORT(set_opt)
extern void set_opt(prolog*, int onoff);

void convert_path(char *filename);

#ifdef __wasi__
EXPORT(pl_global_init)
extern void pl_global_init();
EXPORT(pl_global)
extern void *pl_global();
#endif

extern int g_tpl_interrupt;
extern int g_ac, g_avc;
extern char **g_av, *g_argv0;
extern char *g_tpl_lib;
extern const char *g_version;

#undef EXPORT
