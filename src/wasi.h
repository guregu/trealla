#pragma once
#ifdef WASI_IMPORTS
#include <stddef.h>
#include <stdint.h>

// Return statuses for host calls
#define WASM_HOST_CALL_ERROR 0
#define WASM_HOST_CALL_OK    1
#define WASM_HOST_CALL_YIELD 2

__attribute__((import_module("trealla"), import_name("host-call")))
extern int32_t host_call(int32_t subquery, const char *msg, size_t msg_size, char **reply, size_t *reply_size);
__attribute__((import_module("trealla"), import_name("host-resume")))
extern bool host_resume(int32_t subquery, char **reply, size_t *reply_size);

#endif

#ifdef __wasi__
#define COMPONENT(type) __attribute__((cleanup(type##_free))) type##_t
#define COMPONENT_CSTR(s) ( s.ptr == NULL || s.len == 0 ? s.ptr : \
    s.ptr[s.len] != 0 ? s.ptr = realloc(s.ptr, s.len+1), s.ptr[s.len] = 0, s.ptr : s.ptr )
#endif
