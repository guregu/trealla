#ifdef WASI_TARGET_SPIN

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "cdebug.h"
#include "trealla.h"
#include "prolog.h"
#include "spin-http.h"
#include "stringbuf.h"
#include "map.h"

const char* spin_methods[] = {
	[SPIN_HTTP_METHOD_GET] 		= "get",
	[SPIN_HTTP_METHOD_POST] 	= "post",
	[SPIN_HTTP_METHOD_PUT] 		= "put",
	[SPIN_HTTP_METHOD_DELETE]	= "delete",
	[SPIN_HTTP_METHOD_PATCH]	= "patch",
	[SPIN_HTTP_METHOD_HEAD]		= "head",
	[SPIN_HTTP_METHOD_OPTIONS]	= "options"
};
#define SPIN_HTTP_METHOD_MAX (sizeof(spin_methods) / sizeof(const char*))

extern void spin_http_handle_http_request(spin_http_request_t *request, spin_http_response_t *response) {
	pl_global_init();
	prolog* pl = pl_global();

	// s will be the query sent to Prolog
	SB(s);

	SB_strcat(s, "assertz(http_uri(\"");
	SB_strcatn(s, request->uri.ptr, request->uri.len);
	SB_strcat(s, "\")), ");

	if (request->method > SPIN_HTTP_METHOD_MAX) {
BADMETHOD:
		fprintf(stderr, "Unhandled method: %d\n", request->method);
		response->status = 405;
		return;
	}
	const char *method = spin_methods[request->method];
	if (!method) goto BADMETHOD;

	char *tmpbuf = malloc(1024);
	size_t tmpbuf_len = 1024;

	for (size_t i = 0; i < request->headers.len; i++) {
		spin_http_tuple2_string_string_t header = request->headers.ptr[i];
		size_t fmt_len = header.f1.len*3;
		if (fmt_len > tmpbuf_len) {
			tmpbuf = realloc(tmpbuf, fmt_len);
			fmt_len = tmpbuf_len;
		}
		size_t len = pl_format_string(tmpbuf, tmpbuf_len, header.f1.ptr, header.f1.len, true);
		SB_strcat(s, "assertz(http_header(\"");
		SB_strcatn(s, header.f0.ptr, header.f0.len);
		SB_strcat(s, "\",\"");
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\")), ");
	}

	if (request->body.is_some && request->body.val.len > 0) {
		size_t body_size = request->body.val.len*3;
		if (body_size > tmpbuf_len) {
			tmpbuf = realloc(tmpbuf, body_size);
			tmpbuf_len = body_size;
		}
		size_t len = pl_format_string(tmpbuf, tmpbuf_len, (const char*)request->body.val.ptr, request->body.val.len, true);
		SB_strcat(s, "assertz(http_body(\"");
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\")), ");
	}
	free(tmpbuf);

	SB_strcat(s, "spin:http_handle_request(\"");
	SB_strcatn(s, request->uri.ptr, request->uri.len);
	SB_strcat(s, "\",");
	SB_strcat(s, method);
	SB_strcat(s, ").");

	bool ok = pl_eval(pl, SB_cstr(s));
	SB_free(s);

	if (!ok || !get_status(pl)) {
		int n = pl->current_error;
		stream *str = &pl->streams[n];
		const char *src = SB_cstr(str->sb);
		size_t len = SB_strlen(str->sb);
		char* body = (len > 0) ? strdup(src) : strdup("query failed :(\n");
		int32_t body_length = strlen(body);
		response->status = 500;
		response->body.is_some = true;
		response->body.val.ptr = (uint8_t*)body;
		response->body.val.len = body_length;
		return;
	}

	int status = 0;
	const char *body_string;
	size_t body_len = 0;
	
	int n = pl_get_stream(pl, "http_body", strlen("http_body"));
	stream *str = &pl->streams[n];
	if (str->is_memory) {
		body_string = SB_cstr(str->sb);
		body_len = SB_strlen(str->sb);
	}
	

	n = pl_get_stream(pl, "http_headers", strlen("http_headers"));
	str = &pl->streams[n];
	spin_http_headers_t hdrs = {0};
	if (str->is_map) {
		map *m = str->keyval;
		const char *sch;
		if (map_get(m, "status", (const void**)&sch)) {
			status = atoi(sch);
			map_del(m, "status");
		}

		size_t count = map_count(m);
		hdrs.len = count;
		spin_http_tuple2_string_string_t *headers = calloc(count, sizeof(spin_http_tuple2_string_string_t));
		hdrs.ptr = headers;
		if (count > 0)
			response->headers.is_some = true;

		miter *iter = map_first(m);
		char *value;
		char *key;
		size_t i = 0;
		while (map_next(iter, (void**)&value)) {
			const char *key = map_key(iter);
			spin_http_string_t header_name, header_value;
			spin_http_string_dup(&header_name, key);
			spin_http_string_dup(&header_value, value);
			spin_http_tuple2_string_string_t *header = &headers[i++];
			header->f0 = header_name;
			header->f1 = header_value;
		}
		map_done(iter);
	}
	response->headers.val = hdrs;

	if (!status)
		status = 500;
	response->status = status;

	if (body_len > 0) {
		uint8_t *body = malloc(body_len);
		memcpy(body, body_string, body_len);
		response->body.is_some = true;
		response->body.val.ptr = body;
		response->body.val.len = body_len;
	}
}

#endif