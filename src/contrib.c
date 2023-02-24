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

#ifdef WASI_TARGET_SPIN
#include "spin.h"
#include "wasi-outbound-http.h"
#include "key-value.h"

#define spin_check_kv_error(ret) {															\
		if (ret.is_err) {																	\
			char *errmsg;																	\
			switch (ret.val.err.tag) {														\
			case KEY_VALUE_ERROR_STORE_TABLE_FULL:											\
				return throw_error(q, p1, p1_ctx, "key_value_error", "store_table_full");	\
			case KEY_VALUE_ERROR_NO_SUCH_STORE:												\
				return throw_error(q, p1, p1_ctx, "key_value_error", "no_such_store");		\
			case KEY_VALUE_ERROR_ACCESS_DENIED:												\
				return throw_error(q, p1, p1_ctx, "key_value_error", "access_denied");		\
			case KEY_VALUE_ERROR_INVALID_STORE:												\
				return throw_error(q, p1, p1_ctx, "key_value_error", "invalid_store");		\
			case KEY_VALUE_ERROR_NO_SUCH_KEY:												\
				return false;																\
			case KEY_VALUE_ERROR_IO:														\
				errmsg = alloca(ret.val.err.val.io.len+1);									\
				memcpy(errmsg, ret.val.err.val.io.ptr, ret.val.err.val.io.len);				\
				errmsg[ret.val.err.val.io.len] = 0;											\
				fprintf(stderr, "key value io error: %s\n", errmsg);						\
				return throw_error(q, p1, p1_ctx, "key_value_io_error", errmsg);			\
			default:																		\
				return throw_error(q, p1, p1_ctx, "key_value_error", "unknown_error");		\
			}																				\
		}																					\
	}

static bool fn_sys_wasi_kv_open_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);

	__attribute__((cleanup(key_value_string_free)))
		key_value_string_t kv_name;
	__attribute__((cleanup(key_value_expected_store_error_free)))
		key_value_expected_store_error_t ret;
	
	key_value_string_dup(&kv_name, C_STR(q, p1));
	key_value_open(&kv_name, &ret);
	spin_check_kv_error(ret);

	cell tmp;
	make_int(&tmp, ret.val.ok);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_wasi_kv_close_1(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	key_value_store_t store = p1->val_int;
	key_value_close(store);
	return true;
}

static bool fn_sys_wasi_kv_set_3(query *q) {
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);
	GET_NEXT_ARG(p3,atom_or_list);

	key_value_store_t store = p1->val_int;

	const char *key;
	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);
		
		if (!len)
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		
		key = chars_list_to_string(q, p2, p2_ctx, len);
	} else {
		key = DUP_STR(q, p2);
	}

	const char *value;
	if (is_iso_list(p3)) {
		size_t len = scan_is_chars_list(q, p3, p3_ctx, true);
		
		if (!len)
			return throw_error(q, p3, p3_ctx, "type_error", "atom");
		
		value = chars_list_to_string(q, p3, p3_ctx, len);
	} else {
		value = DUP_STR(q, p3);
	}

	__attribute__((cleanup(key_value_string_free)))
		key_value_string_t kv_key;
	__attribute__((cleanup(key_value_list_u8_free)))
		key_value_list_u8_t kv_val;
	__attribute__((cleanup(key_value_expected_unit_error_free)))
		key_value_expected_unit_error_t ret;

	key_value_string_set(&kv_key, key);
	key_value_string_set(&kv_val, value);

	key_value_set(store, &kv_key, &kv_val, &ret);
	spin_check_kv_error(ret);

	return true;
}

static bool fn_sys_wasi_kv_get_3(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);
	GET_NEXT_ARG(p3,var);

	key_value_store_t store = p1->val_int;
	__attribute__((cleanup(key_value_string_free)))
		key_value_string_t kv_key;
	__attribute__((cleanup(key_value_expected_list_u8_error_free)))
		key_value_expected_list_u8_error_t ret;
	
	const char *key;
	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);
		
		if (!len)
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		
		key = chars_list_to_string(q, p2, p2_ctx, len);
	} else {
		key = DUP_STR(q, p2);
	}
	key_value_string_set(&kv_key, key);

	key_value_get(store, &kv_key, &ret);
	spin_check_kv_error(ret);

	cell tmp;
	check_heap_error(make_cstringn(&tmp, ret.val.ok.ptr, ret.val.ok.len));
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_wasi_kv_delete_2(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);

	key_value_store_t store = p1->val_int;
	__attribute__((cleanup(key_value_string_free)))
		key_value_string_t kv_key;
	__attribute__((cleanup(key_value_expected_bool_error_free)))
		key_value_expected_bool_error_t ret;
	
	const char *key;
	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);
		
		if (!len)
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		
		key = chars_list_to_string(q, p2, p2_ctx, len);
	} else {
		key = DUP_STR(q, p2);
	}
	key_value_string_set(&kv_key, key);

	key_value_delete(store, &kv_key, &ret);
	spin_check_kv_error(ret);

	return true;
}

static bool fn_sys_wasi_kv_exists_2(query *q)
{
	GET_FIRST_ARG(p1,smallint);
	GET_NEXT_ARG(p2,atom_or_list);

	key_value_store_t store = p1->val_int;
	__attribute__((cleanup(key_value_string_free)))
		key_value_string_t kv_key;
	__attribute__((cleanup(key_value_expected_bool_error_free)))
		key_value_expected_bool_error_t ret;
	
	const char *key;
	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);
		
		if (!len)
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		
		key = chars_list_to_string(q, p2, p2_ctx, len);
	} else {
		key = DUP_STR(q, p2);
	}
	key_value_string_set(&kv_key, key);

	key_value_exists(store, &kv_key, &ret);
	spin_check_kv_error(ret);

	return ret.val.ok;
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

	__attribute__((cleanup(wasi_outbound_http_request_free)))
		wasi_outbound_http_request_t request = {0};
	__attribute__((cleanup(wasi_outbound_http_response_free)))
		wasi_outbound_http_response_t response = {0};

	// Request URL
	const char *url;
	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		url = chars_list_to_string(q, p1, p1_ctx, len);
	} else {
		url = DUP_STR(q, p1);
	}
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
	{"$wasi_kv_open", 2, fn_sys_wasi_kv_open_2, "+atom,-integer", false, false, BLAH},
	{"$wasi_kv_close", 1, fn_sys_wasi_kv_close_1, "+integer", false, false, BLAH},
	{"$wasi_kv_get", 3, fn_sys_wasi_kv_get_3, "+integer,+string,-string", false, false, BLAH},
	{"$wasi_kv_set", 3, fn_sys_wasi_kv_set_3, "+integer,+string,+string", false, false, BLAH},
	{"$wasi_kv_delete", 2, fn_sys_wasi_kv_delete_2, "+integer,+string", false, false, BLAH},
	{"$wasi_kv_exists", 2, fn_sys_wasi_kv_exists_2, "+integer,+string", false, false, BLAH},
#endif
	{0}
};

