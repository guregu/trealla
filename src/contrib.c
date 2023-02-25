#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "query.h"
#include "heap.h"

#ifdef WASI_TARGET_SPIN
#include "wasi.h"
#include "spin.h"
#include "wasi-outbound-http.h"
#include "key-value.h"

#define check_kv_error(p, ret) {															\
		if (ret.is_err) {																	\
			switch (ret.val.err.tag) {														\
			case KEY_VALUE_ERROR_STORE_TABLE_FULL:											\
				return throw_error(q, p, p##_ctx, "key_value_error", "store_table_full");	\
			case KEY_VALUE_ERROR_NO_SUCH_STORE:												\
				return throw_error(q, p, p##_ctx, "key_value_error", "no_such_store");		\
			case KEY_VALUE_ERROR_ACCESS_DENIED:												\
				return throw_error(q, p, p##_ctx, "key_value_error", "access_denied");		\
			case KEY_VALUE_ERROR_INVALID_STORE:												\
				return throw_error(q, p, p##_ctx, "key_value_error", "invalid_store");		\
			case KEY_VALUE_ERROR_NO_SUCH_KEY:												\
				return false;																\
			case KEY_VALUE_ERROR_IO:														\
				return throw_error(q, p, p##_ctx, "key_value_io_error",						\
					COMPONENT_CSTR(ret.val.err.val.io));									\
			default:																		\
				return throw_error(q, p, p##_ctx, "key_value_error", "unknown_error");		\
			}																				\
		}																					\
	}

static bool fn_sys_wasi_kv_open_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);

	COMPONENT(key_value_string) kv_name;
	COMPONENT(key_value_expected_store_error) ret;
	
	key_value_string_dup(&kv_name, C_STR(q, p1));

	key_value_open(&kv_name, &ret);
	check_kv_error(p1, ret);

	cell tmp;
	make_int(&tmp, ret.val.ok);
	tmp.flags |= FLAG_INT_HANDLE | FLAG_INT_OCTAL;
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_wasi_kv_close_1(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	const key_value_store_t store = p1->val_int;
	key_value_close(store);
	return true;
}

static bool fn_sys_wasi_kv_get_3(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);
	GET_NEXT_ARG(p3,var);

	dup_string(key, p2);

	const key_value_store_t store = p1->val_int;
	COMPONENT(key_value_string) kv_key = {
		.ptr = key,
		.len = key_len
	};
	COMPONENT(key_value_expected_list_u8_error) ret;

	key_value_get(store, &kv_key, &ret);
	check_kv_error(p1, ret);

	cell tmp;
	check_heap_error(make_stringn(&tmp, (const char*)ret.val.ok.ptr, ret.val.ok.len));
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_wasi_kv_get_keys_2(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,var);

	const key_value_store_t store = p1->val_int;
	COMPONENT(key_value_expected_list_string_error) ret;

	key_value_get_keys(store, &ret);
	check_kv_error(p1, ret);

	for (size_t i = 0; i < ret.val.ok.len; i++) {
		key_value_string_t key = ret.val.ok.ptr[i];
		
		cell tmp;
		check_heap_error(make_stringn(&tmp, key.ptr, key.len));

		if (i == 0)
			allocate_list(q, &tmp);
		else
			append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_sys_wasi_kv_exists_2(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);

	dup_string(key, p2);

	const key_value_store_t store = p1->val_int;
	COMPONENT(key_value_string) kv_key = {
		.ptr = key,
		.len = key_len
	};
	COMPONENT(key_value_expected_bool_error) ret;

	key_value_exists(store, &kv_key, &ret);
	check_kv_error(p1, ret);

	return ret.val.ok;
}

static bool fn_sys_wasi_kv_set_3(query *q) {
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);
	GET_NEXT_ARG(p3,atom_or_list);

	dup_string(key, p2);
	dup_string(val, p3);

	const key_value_store_t store = p1->val_int;
	COMPONENT(key_value_string) kv_key = {
		.ptr = key,
		.len = key_len
	};
	COMPONENT(key_value_list_u8) kv_val = {
		.ptr = (uint8_t*)val,
		.len = val_len
	};
	COMPONENT(key_value_expected_unit_error) ret;

	key_value_set(store, &kv_key, &kv_val, &ret);
	check_kv_error(p1, ret);

	return true;
}

static bool fn_sys_wasi_kv_delete_2(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);

	dup_string(key, p2);

	const key_value_store_t store = p1->val_int;
	COMPONENT(key_value_string) kv_key = {
		.ptr = key,
		.len = key_len
	};
	COMPONENT(key_value_expected_unit_error) ret;

	key_value_delete(store, &kv_key, &ret);
	check_kv_error(p1, ret);

	return true;
}

static bool fn_sys_wasi_outbound_http_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,stream);
	stream *req_str = &q->pl->streams[get_stream(q, p2)];
	GET_NEXT_ARG(p3,stream);
	stream *req_hdr_str = &q->pl->streams[get_stream(q, p3)];
	GET_NEXT_ARG(p4,stream);
	stream *resp_str = &q->pl->streams[get_stream(q, p4)];
	GET_NEXT_ARG(p5,stream);
	stream *resp_hdr_str = &q->pl->streams[get_stream(q, p5)];

	if (!is_map_stream(req_str))
		return throw_error(q, p2, q->st.curr_frame, "type_error", "not_a_map");
	if (!is_map_stream(req_hdr_str))
		return throw_error(q, p3, q->st.curr_frame, "type_error", "not_a_map");
	if (!is_map_stream(resp_str))
		return throw_error(q, p4, q->st.curr_frame, "type_error", "not_a_map");
	if (!is_map_stream(resp_hdr_str))
		return throw_error(q, p5, q->st.curr_frame, "type_error", "not_a_map");

	COMPONENT(wasi_outbound_http_request) request = {0};
	COMPONENT(wasi_outbound_http_response) response;

	// Request URL
	const dup_string(url, p1);
	wasi_outbound_http_string_set(&request.uri, url);

	// Request method
	const char *tmpstr;
	if (map_get(req_str->keyval, "method", (const void **)&tmpstr)) {
		if (!spin_http_method_lookup(tmpstr, &request.method))
			return throw_error(q, p2, q->st.curr_frame, "domain_error", "http_method");
	}

	// Request body
	if (map_get(req_str->keyval, "body", (const void **)&tmpstr)) {
		request.body.is_some = true;
		request.body.val.len = strlen(tmpstr);
		request.body.val.ptr = (uint8_t *)strdup(tmpstr);
	}

	// Request headers
	size_t req_hdr_ct = map_count(req_hdr_str->keyval);
	if (req_hdr_ct > 0) {
		request.headers.len = req_hdr_ct;
		wasi_outbound_http_tuple2_string_string_t *headers = calloc(req_hdr_ct,
			sizeof(wasi_outbound_http_tuple2_string_string_t));
		request.headers.ptr = headers;

		miter *iter = map_first(req_hdr_str->keyval);
		const char *value;
		size_t i = 0;
		while (map_next(iter, (void **)&value)) {
			wasi_outbound_http_tuple2_string_string_t *header = &headers[i++];
			const char *key = map_key(iter);
			wasi_outbound_http_string_t header_name, header_value;
			wasi_outbound_http_string_dup(&header_name, key);
			wasi_outbound_http_string_dup(&header_value, value);
			header->f0 = header_name;
			header->f1 = header_value;
		}
		map_done(iter);
	}

	wasi_outbound_http_http_error_t code = wasi_outbound_http_request(&request, &response);
	switch (code) {
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_SUCCESS:
	case 255:
		// 255 is success (WASI_OUTBOUND_HTTP_HTTP_ERROR_SUCCESS is unused?)
		// see: https://discord.com/channels/926888690310053918/950022897160839248/1026482038028648558
		break;
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_DESTINATION_NOT_ALLOWED:
		return throw_error(q, p1, p1_ctx, "spin_error", "destination_not_allowed");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_INVALID_URL:
		return throw_error(q, p1, p1_ctx, "spin_error", "invalid_url");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_REQUEST_ERROR:
		return throw_error(q, p1, p1_ctx, "spin_error", "request_error");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_RUNTIME_ERROR:
		return throw_error(q, p1, p1_ctx, "spin_error", "runtime_error");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_TOO_MANY_REQUESTS:
		return throw_error(q, p1, p1_ctx, "spin_error", "too_many_requests");
	default:
		return throw_error(q, p1, p1_ctx, "spin_error", "unknown_error");
	}

	// Response status
	char tmpbuf[8];
	snprintf(tmpbuf, sizeof(tmpbuf), "%d", response.status);
	map_set(resp_str->keyval, strdup("status"), strdup(tmpbuf));

	// Response body
	if (response.body.is_some) {
		char *body = malloc(response.body.val.len + 1);
		body[response.body.val.len] = 0;
		memcpy(body, response.body.val.ptr, response.body.val.len);
		map_set(resp_str->keyval, strdup("body"), body);
	}

	// Response headers
	if (response.headers.is_some) {
		char *k, *v;
		for (size_t i = 0; i < response.headers.val.len; i++) {
			wasi_outbound_http_tuple2_string_string_t *header = &response.headers.val.ptr[i];

			k = malloc(header->f0.len + 1);
			k[header->f0.len] = 0;
			memcpy(k, header->f0.ptr, header->f0.len);

			v = malloc(header->f1.len + 1);
			v[header->f1.len] = 0;
			memcpy(v, header->f1.ptr, header->f1.len);

			map_set(resp_hdr_str->keyval, k, v);
		}
	}

	return true;
}
#endif

builtins g_contrib_bifs[] =
{
#ifdef WASI_TARGET_SPIN
	{"$wasi_outbound_http", 5, fn_sys_wasi_outbound_http_5, "+string,+map,+map,-map,-map", false, false, BLAH},
	{"$wasi_kv_open", 2, fn_sys_wasi_kv_open_2, "+atom,-store", false, false, BLAH},
	{"$wasi_kv_close", 1, fn_sys_wasi_kv_close_1, "+store", false, false, BLAH},
	{"$wasi_kv_get", 3, fn_sys_wasi_kv_get_3, "+store,+string,-string", false, false, BLAH},
	{"$wasi_kv_get_keys", 2, fn_sys_wasi_kv_get_keys_2, "+store,-list", false, false, BLAH},
	{"$wasi_kv_exists", 2, fn_sys_wasi_kv_exists_2, "+store,+string", false, false, BLAH},
	{"$wasi_kv_set", 3, fn_sys_wasi_kv_set_3, "+store,+string,+string", false, false, BLAH},
	{"$wasi_kv_delete", 2, fn_sys_wasi_kv_delete_2, "+store,+string", false, false, BLAH},
#endif
	{0}
};

#undef check_kv_error
