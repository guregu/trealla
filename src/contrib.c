#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "trealla.h"
#include "internal.h"
#include "query.h"
#include "heap.h"
#include "prolog.h"

#ifdef WASI_TARGET_SPIN
#include "wasi.h"
#include "spin.h"
#include "wasi-outbound-http.h"
#include "key-value.h"
#include "outbound-pg.h"

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

	const key_value_store_t store = p1->val_int;

	dup_string(key, p2);
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

	const key_value_store_t store = p1->val_int;

	dup_string(key, p2);
	COMPONENT(key_value_string) kv_key = {
		.ptr = key,
		.len = key_len
	};

	dup_string(val, p3);
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

#define check_pg_error(p, ret) 													\
	if (ret.is_err) { 															\
		pl_idx_t kind = 0;														\
		char *msg; 																\
		switch (ret.val.err.tag) {												\
		case OUTBOUND_PG_PG_ERROR_CONNECTION_FAILED:							\
			kind = index_from_pool(q->pl, "connection_failed");					\
			COMPONENT_DUP_STR(msg, ret.val.err.val.connection_failed);			\
			break; 																\
		case OUTBOUND_PG_PG_ERROR_BAD_PARAMETER:								\
			kind = index_from_pool(q->pl, "bad_parameter");						\
			COMPONENT_DUP_STR(msg, ret.val.err.val.bad_parameter);				\
			break;																\
		case OUTBOUND_PG_PG_ERROR_QUERY_FAILED:									\
			kind = index_from_pool(q->pl, "query_failed");						\
			COMPONENT_DUP_STR(msg, ret.val.err.val.query_failed);				\
			break; 																\
		case OUTBOUND_PG_PG_ERROR_VALUE_CONVERSION_FAILED:						\
			kind = index_from_pool(q->pl, "value_conversion_failed");			\
			COMPONENT_DUP_STR(msg, ret.val.err.val.value_conversion_failed);	\
			break; 																\
		case OUTBOUND_PG_PG_ERROR_OTHER_ERROR: 									\
			kind = index_from_pool(q->pl, "other_error");						\
			COMPONENT_DUP_STR(msg, ret.val.err.val.other_error);				\
			break; 																\
		} 																		\
		cell *tmp = alloc_on_heap(q, 3); 										\
		make_struct(tmp, g_error_s, NULL, 2, 2); 								\
		make_atom(tmp+1, kind);													\
		make_string(tmp+2, msg); 												\
		bool ok = unify(q, p, p##_ctx, tmp, q->st.curr_frame);					\
		free(msg); 																\
		return ok; 																\
	}

#define PG_ATOM_INDICES() \
	pl_idx_t row_idx = index_from_pool(q->pl, "row");			\
	pl_idx_t boolean_idx = index_from_pool(q->pl, "boolean");	\
	pl_idx_t int8_idx = index_from_pool(q->pl, "int8");			\
	pl_idx_t int16_idx = index_from_pool(q->pl, "int16");		\
	pl_idx_t int32_idx = index_from_pool(q->pl, "int32");		\
	pl_idx_t int64_idx = index_from_pool(q->pl, "int64");		\
	pl_idx_t uint8_idx = index_from_pool(q->pl, "uint8");		\
	pl_idx_t uint16_idx = index_from_pool(q->pl, "uint16");		\
	pl_idx_t uint32_idx = index_from_pool(q->pl, "uint32");		\
	pl_idx_t uint64_idx = index_from_pool(q->pl, "uint64");		\
	pl_idx_t float32_idx = index_from_pool(q->pl, "float32");	\
	pl_idx_t float64_idx = index_from_pool(q->pl, "float64");	\
	pl_idx_t string_idx = index_from_pool(q->pl, "string");		\
	pl_idx_t binary_idx = index_from_pool(q->pl, "binary");		\
	pl_idx_t null_idx = index_from_pool(q->pl, "null");			\
	pl_idx_t other_idx = index_from_pool(q->pl, "other");

#define make_pg_params(p, params)													\
	{																				\
		cell *_##p##_head = p;														\
		LIST_HANDLER(p);															\
		while (is_iso_list(p) && !g_tpl_interrupt) {								\
			cell *c = LIST_HEAD(p);													\
			params.len++;															\
			if (!is_structure(c)) {													\
				return throw_error(q, c, q->latest_ctx, "type_error", "compound");	\
			}																		\
			p = LIST_TAIL(p);														\
		}																			\
		p = _##p##_head;															\
		params.ptr = calloc(params.len, sizeof(outbound_pg_parameter_value_t));		\
		size_t i = 0;																\
		while (is_iso_list(p) && !g_tpl_interrupt) {								\
			cell *c = LIST_HEAD(p);													\
			c = deref(q, c, p##_ctx);												\
			cell *v = deref(q, c+1, p##_ctx);										\
			pl_idx_t v_ctx = q->latest_ctx;											\
			outbound_pg_parameter_value_t *value = &params.ptr[i];					\
			if (c->val_off == boolean_idx) {										\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_BOOLEAN;					\
				value->val.boolean = v->val_off == g_true_s;						\
			} else if (c->val_off == int8_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT8;						\
				value->val.int8 = v->val_int;										\
			} else if (c->val_off == int16_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT16;						\
				value->val.int16 = v->val_int;										\
			} else if (c->val_off == int32_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT32;						\
				value->val.int32 = v->val_int;										\
			} else if (c->val_off == int64_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT64;						\
				value->val.int64 = v->val_int;										\
			} else if (c->val_off == uint8_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT8;						\
				value->val.uint8 = v->val_uint;										\
			} else if (c->val_off == uint16_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT16;						\
				value->val.uint16 = v->val_uint;									\
			} else if (c->val_off == uint32_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT32;						\
				value->val.uint32 = v->val_uint;									\
			} else if (c->val_off == uint64_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_INT64;						\
				value->val.uint64 = v->val_uint;									\
			} else if (c->val_off == float32_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_FLOATING32;				\
				value->val.floating32 = v->val_float;								\
			} else if (c->val_off == float64_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_FLOATING64;				\
				value->val.floating64 = v->val_float;								\
			} else if (c->val_off == string_idx) {									\
				dup_string(str, v);													\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_STR;						\
				value->val.str.ptr = str;											\
				value->val.str.len = str_len;										\
			} else if (c->val_off == binary_idx) {									\
				dup_string(str, v);													\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_BINARY;					\
				value->val.binary.ptr = str;										\
				value->val.binary.len = str_len;									\
			} else if (c->val_off == null_idx) {									\
				value->tag = OUTBOUND_PG_PARAMETER_VALUE_DB_NULL;					\
			}																		\
			p = LIST_TAIL(p);														\
			i++;																	\
		}																			\
		p = _##p##_head;															\
	}

static bool fn_sys_outbound_pg_query_5(query *q)
{
	GET_FIRST_ARG(p1,any); 			// addr
	GET_NEXT_ARG(p2,any);			// stmt
	GET_NEXT_ARG(p3,list_or_nil); 	// params
	GET_NEXT_ARG(p4,var);			// rows
	GET_NEXT_ARG(p5,var);			// column info

	dup_string(addr, p1);
	COMPONENT(outbound_pg_string) address = {
		.ptr = addr,
		.len = addr_len
	};
	dup_string(stmt, p2);
	COMPONENT(outbound_pg_string) statement = {
		.ptr = stmt,
		.len = stmt_len
	};
	COMPONENT(outbound_pg_list_parameter_value) params = {0};
	COMPONENT(outbound_pg_expected_row_set_pg_error) ret = {0};

	PG_ATOM_INDICES();
	make_pg_params(p3, params);

	outbound_pg_query(&address, &statement, &params, &ret);
	check_pg_error(p4, ret);

	cell *rows;
	cell tmpr;
	if (ret.val.ok.rows.len == 0) {
		make_atom(&tmpr, g_nil_s);
		rows = &tmpr;
	} else {
		for (size_t i = 0; i < ret.val.ok.rows.len; i++) {
			outbound_pg_row_t row = ret.val.ok.rows.ptr[i];
			cell *tmp;
			tmp = alloc_on_heap(q, 1 + row.len*2);
			check_heap_error(tmp);
			pl_idx_t nbr_cells = 0;
			make_struct(tmp+nbr_cells++, row_idx, NULL, row.len, row.len*2);
			for (size_t i = 0; i < row.len; i++) {
				outbound_pg_db_value_t col = row.ptr[i];
				switch (col.tag) {
				case OUTBOUND_PG_DB_VALUE_BOOLEAN:
					make_struct(tmp+nbr_cells++, boolean_idx, NULL, 1, 1);
					make_atom(tmp+nbr_cells++, col.val.boolean ? g_true_s : g_false_s);
					break;
				case OUTBOUND_PG_DB_VALUE_INT8:
					make_struct(tmp+nbr_cells++, int8_idx, NULL, 1, 1);
					make_int(tmp+nbr_cells++, col.val.int8);
					break;
				case OUTBOUND_PG_DB_VALUE_INT16:
					make_struct(tmp+nbr_cells++, int16_idx, NULL, 1, 1);
					make_int(tmp+nbr_cells++, col.val.int16);
					break;
				case OUTBOUND_PG_DB_VALUE_INT32:
					make_struct(tmp+nbr_cells++, int32_idx, NULL, 1, 1);
					make_int(tmp+nbr_cells++, col.val.int32);
					break;
				case OUTBOUND_PG_DB_VALUE_INT64:
					make_struct(tmp+nbr_cells++, int64_idx, NULL, 1, 1);
					make_int(tmp+nbr_cells++, col.val.int64);
					break;
				case OUTBOUND_PG_DB_VALUE_UINT8:
					make_struct(tmp+nbr_cells++, uint8_idx, NULL, 1, 1);
					make_uint(tmp+nbr_cells++, col.val.uint8);
					break;
				case OUTBOUND_PG_DB_VALUE_UINT16:
					make_struct(tmp+nbr_cells++, uint16_idx, NULL, 1, 1);
					make_uint(tmp+nbr_cells++, col.val.uint16);
					break;
				case OUTBOUND_PG_DB_VALUE_UINT32:
					make_struct(tmp+nbr_cells++, uint32_idx, NULL, 1, 1);
					make_uint(tmp+nbr_cells++, col.val.uint32);
					break;
				case OUTBOUND_PG_DB_VALUE_UINT64:
					make_struct(tmp+nbr_cells++, uint64_idx, NULL, 1, 1);
					make_uint(tmp+nbr_cells++, col.val.uint64);
					break;
				case OUTBOUND_PG_DB_VALUE_FLOATING32:
					make_struct(tmp+nbr_cells++, float32_idx, NULL, 1, 1);
					make_float(tmp+nbr_cells++, col.val.floating32);
					break;
				case OUTBOUND_PG_DB_VALUE_FLOATING64:
					make_struct(tmp+nbr_cells++, float64_idx, NULL, 1, 1);
					make_float(tmp+nbr_cells++, col.val.floating64);
					break;
				case OUTBOUND_PG_DB_VALUE_STR:
					make_struct(tmp+nbr_cells++, string_idx, NULL, 1, 1);
					make_stringn(tmp+nbr_cells++, col.val.str.ptr, col.val.str.len);
					break;
				case OUTBOUND_PG_DB_VALUE_BINARY:
					make_struct(tmp+nbr_cells++, binary_idx, NULL, 1, 1);
					make_stringn(tmp+nbr_cells++, col.val.binary.ptr, col.val.binary.len);
					break;
				case OUTBOUND_PG_DB_VALUE_DB_NULL:
					make_struct(tmp+nbr_cells++, null_idx, NULL, 1, 1);
					make_atom(tmp+nbr_cells++, g_nil_s);
					break;
				}
			}

			if (i == 0)
				allocate_list(q, tmp);
			else
				append_list(q, tmp);
		}
		rows = end_list(q);
		check_heap_error(rows);
	}

	// Column info.
	bool has_cols = false;
	for (size_t i = 0; i < ret.val.ok.columns.len; i++) {
		outbound_pg_column_t col = ret.val.ok.columns.ptr[i];
		
		cell *tmp = alloc_on_heap(q, 3);
		check_heap_error(tmp);
		pl_idx_t type_idx = 0;
		pl_idx_t nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_minus_s, NULL, 2, 2);
		SET_OP(tmp, OP_YFX);
		make_stringn(tmp+nbr_cells++, col.name.ptr, col.name.len);
		switch (col.data_type) {
		case OUTBOUND_PG_DB_DATA_TYPE_BOOLEAN:
			type_idx = boolean_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_INT8:
			type_idx = int8_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_INT16:
			type_idx = int16_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_INT32:
			type_idx = int32_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_INT64:
			type_idx = int64_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_UINT8:
			type_idx = uint8_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_UINT16:
			type_idx = uint16_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_UINT32:
			type_idx = uint32_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_UINT64:
			type_idx = uint64_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_FLOATING32:
			type_idx = float32_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_FLOATING64:
			type_idx = float64_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_STR:
			type_idx = string_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_BINARY:
			type_idx = binary_idx;
			break;
		case OUTBOUND_PG_DB_DATA_TYPE_OTHER:
			type_idx = other_idx;
			break;
		}
		make_atom(tmp+nbr_cells++, type_idx);

		if (i == 0)
			allocate_list(q, tmp);
		else
			append_list(q, tmp);
		has_cols = true;
	}

	cell *cols;
	cell tmpc;
	if (has_cols) {
		cols = end_list(q);
		check_heap_error(cols);
	} else {
		make_atom(&tmpc, g_nil_s);
		cols = &tmpc;
	}

	bool ok1 = unify(q, p4, p4_ctx, rows, q->st.curr_frame);
	bool ok2 = unify(q, p5, p5_ctx, cols, q->st.curr_frame);
	return ok1 && ok2;
}

static bool fn_sys_outbound_pg_execute_4(query *q)
{
	GET_FIRST_ARG(p1,any); 			// addr
	GET_NEXT_ARG(p2,any);			// stmt
	GET_NEXT_ARG(p3,list_or_nil); 	// params
	GET_NEXT_ARG(p4,var);			// error or affected rows count

	dup_string(addr, p1);
	COMPONENT(outbound_pg_string) address = {
		.ptr = addr,
		.len = addr_len
	};
	dup_string(stmt, p2);
	COMPONENT(outbound_pg_string) statement = {
		.ptr = stmt,
		.len = stmt_len
	};
	COMPONENT(outbound_pg_list_parameter_value) params = {0};
	COMPONENT(outbound_pg_expected_u64_pg_error) ret = {0};

	PG_ATOM_INDICES();
	make_pg_params(p3, params);

	outbound_pg_execute(&address, &statement, &params, &ret);
	check_pg_error(p4, ret);

	cell *tmp = alloc_on_heap(q, 2);
	make_struct(tmp, g_true_s, NULL, 1, 1);
	make_uint(tmp+1, ret.val.ok);
	return unify(q, p4, p4_ctx, tmp, q->st.curr_frame);
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
	{"$outbound_pg_query", 5, fn_sys_outbound_pg_query_5, "+string,+string,+list,-list,-list", false, false, BLAH},
	{"$outbound_pg_execute", 4, fn_sys_outbound_pg_execute_4, "+string,+string,+list,-compound", false, false, BLAH},
#endif
	{0}
};

#undef check_kv_error
#undef check_pg_error

