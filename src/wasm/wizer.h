/*
	Minimal implementation of Wizer exports for C.
	Public domain. No rights reserved.

	For C++ support, backwards compatibility, etc., check out the
	official version: https://github.com/bytecodealliance/wizer
*/
#pragma once
#ifdef __wasi__

extern void __wasm_call_ctors();
extern void __wasm_call_dtors();
extern int  __main_void();

#define WIZER_INIT(init)							\
__attribute__((export_name("wizer.initialize")))	\
void wizer_init() {									\
	__wasm_call_ctors();							\
	init();											\
}													\
__attribute__((export_name("wizer.resume")))		\
void wizer_resume() {								\
	__main_void();									\
	__wasm_call_dtors();							\
}

#endif
