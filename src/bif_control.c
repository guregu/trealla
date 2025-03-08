#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

bool bif_iso_true_0(query *q)
{
	return true;
}

bool bif_iso_fail_0(query *q)
{
	return false;
}

bool bif_iso_cut_0(query *q)
{
	cut(q);
	return true;
}

bool bif_sys_cut_1(query *q)
{
	GET_FIRST_ARG(p1,integer)
	choice *ch = GET_CHOICE(get_smalluint(p1));
	ch->reset = true;
	return true;
}

// goal , goal

bool bif_iso_conjunction_2(query *q)
{
	q->tot_inferences--;
	q->retry = QUERY_NOOP;
	q->st.curr_instr++;
	return true;
}

static bool bif_sys_cleanup_if_det_1(query *q)
{
	q->tot_inferences--;
	GET_FIRST_RAW_ARG(p1,integer)
	choice *ch = GET_CURR_CHOICE();

	if ((q->cp-1) != get_smalluint(p1))
		return true;

	drop_choice(q);
	ch = GET_CURR_CHOICE();

	if (!ch->register_cleanup)
		return true;

	if (ch->fail_on_retry)
		return true;

	drop_choice(q);
	ch->fail_on_retry = true;
	cell *c = deref(q, ch->st.curr_instr, ch->st.curr_frame);
	pl_idx c_ctx = q->latest_ctx;
	c = deref(q, FIRST_ARG(c), c_ctx);
	c_ctx = q->latest_ctx;
	do_cleanup(q, c, c_ctx);
	return true;
}

bool bif_call_0(query *q, cell *p1, pl_idx p1_ctx)
{
	if (!is_callable(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	cell *tmp = prepare_call(q, NOPREFIX_LEN, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_fail_on_retry(q));
	q->st.curr_instr = tmp;
	return true;
}

bool bif_sys_call_1(query *q)
{
	GET_FIRST_RAW_ARG(p1,callable);
	bool status;

	if (!call_check(q, p1, &status, true))
		return status;

	q->st.curr_instr++;
	q->retry = QUERY_NOOP;
	return true;
}

bool bif_iso_call_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if ((is_builtin(p1) && !is_evaluable(p1)) || !p1->arity) {
		check_heap_error(init_tmp_heap(q));
		p1 = deep_clone_to_tmp(q, p1, p1_ctx);
		check_heap_error(p1);
		p1_ctx = q->st.curr_frame;
		bool status;

		if (!call_check(q, p1, &status, false))
			return status;
	}

	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 3);
	check_heap_error(tmp);
	tmp[PREFIX_LEN].flags &= ~FLAG_TAIL_CALL;
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_fail_on_retry(q));

	if (is_tail_call(q->st.curr_instr))
		tmp[PREFIX_LEN].flags |= FLAG_TAIL_CALL;

	q->st.curr_instr = tmp;
	return true;
}

static bool bif_iso_call_n(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if ((p1->val_off == g_colon_s) && (p1->arity == 2)) {
		cell *cm = p1 + 1;
		cm = deref(q, cm, p1_ctx);

		if (!is_atom(cm) && !is_var(cm))
			return throw_error(q, cm, p1_ctx, "type_error", "callable");

		if (!is_var(cm)) {
			module *m = find_module(q->pl, C_STR(q, cm));
			if (m) q->st.curr_m = m;
		}

		p1 += 2;
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		if (!is_callable(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "callable");
	}

	check_heap_error(init_tmp_heap(q));
	unsigned arity = p1->arity, args = 1, xarity = q->st.curr_instr->arity;

	check_heap_error(deep_clone_to_tmp(q, p1, p1_ctx));

	while (args++ < xarity) {
		GET_NEXT_ARG(p2,any);
		check_heap_error(deep_clone_to_tmp(q, p2, p2_ctx));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;

	if (is_cstring(tmp2)) {
		share_cell(tmp2);
		convert_to_literal(q->st.curr_m, tmp2);
	}

	tmp2->match = NULL;
	bool status;

	if (!call_check(q, tmp2, &status, true))
		return status;

	cell *tmp = prepare_call(q, PREFIX_LEN, tmp2, q->st.curr_frame, 3 );
	check_heap_error(tmp);
	tmp[PREFIX_LEN].flags &= ~FLAG_TAIL_CALL;
	pl_idx nbr_cells = PREFIX_LEN + tmp2->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_fail_on_retry(q));

	if (is_tail_call(q->st.curr_instr))
		tmp[PREFIX_LEN].flags |= FLAG_TAIL_CALL;

	q->st.curr_instr = tmp;
	return true;
}

// goal, !

static bool bif_iso_once_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if ((is_builtin(p1) && !is_evaluable(p1)) || !p1->arity) {
		check_heap_error(init_tmp_heap(q));
		p1 = deep_clone_to_tmp(q, p1, p1_ctx);
		check_heap_error(p1);
		p1_ctx = q->st.curr_frame;
		bool status;

		if (!call_check(q, p1, &status, false))
			return status;
	}

	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 4);
	check_heap_error(tmp);
	tmp[PREFIX_LEN].flags &= ~FLAG_TAIL_CALL;
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_fail_on_retry(q));
	q->st.curr_instr = tmp;
	return true;
}

// if -> ! ; true

static bool bif_ignore_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	check_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp2);
	bool status;

	if (!call_check(q, tmp2, &status, false))
		return status;

	cell *tmp = prepare_call(q, PREFIX_LEN, tmp2, q->st.curr_frame, 4);
	check_heap_error(tmp);
	tmp[PREFIX_LEN].flags &= ~FLAG_TAIL_CALL;
	pl_idx nbr_cells = PREFIX_LEN + tmp2->nbr_cells;
	make_instr(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_succeed_on_retry(q, 0));
	q->st.curr_instr = tmp;
	return true;
}

// if -> then

bool bif_iso_if_then_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 3+p2->nbr_cells+2);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	nbr_cells += dup_cells_by_ref(tmp+nbr_cells, p2, p2_ctx, p2->nbr_cells);
	make_instr(tmp+nbr_cells++, g_true_s, bif_iso_true_0, 0, 0); // Why???
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_fail_on_retry(q));
	q->st.curr_instr = tmp;
	return true;
}

// if *-> then

bool bif_soft_if_then_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 2+p2->nbr_cells+2);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	nbr_cells += dup_cells_by_ref(tmp+nbr_cells, p2, p2_ctx, p2->nbr_cells);
	make_instr(tmp+nbr_cells++, g_true_s, bif_iso_true_0, 0, 0); // Why???
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_fail_on_retry(q));
	q->st.curr_instr = tmp;
	return true;
}

// if -> then ; else

static bool do_if_then_else(query *q, cell *p1, cell *p2, cell *p3)
{
	cell *tmp = prepare_call(q, PREFIX_LEN, p1, q->st.curr_frame, 3+p2->nbr_cells+2);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	nbr_cells += dup_cells_by_ref(tmp+nbr_cells, p2, q->st.curr_frame, p2->nbr_cells);
	make_instr(tmp+nbr_cells++, g_true_s, bif_iso_true_0, 0, 0); // Why???
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	q->st.curr_instr = tmp;
	return true;
}

// if *-> then ; else

static bool do_soft_if_then_else(query *q, cell *p1, cell *p2, cell *p3)
{
	cell *tmp = prepare_call(q, PREFIX_LEN, p1, q->st.curr_frame, 4+p2->nbr_cells+2);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_cut_s, bif_sys_cut_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	nbr_cells += dup_cells_by_ref(tmp+nbr_cells, p2, q->st.curr_frame, p2->nbr_cells);
	make_instr(tmp+nbr_cells++, g_true_s, bif_iso_true_0, 0, 0); // Why???
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	q->st.curr_instr = tmp;
	return true;
}

// if(if,then,else)

static bool bif_if_3(query *q)
{
	cell *p1 = q->st.curr_instr + 1;
	cell *p2 = p1 + p1->nbr_cells;
	cell *p3 = p2 + p2->nbr_cells;

	if (q->retry) {
		q->st.curr_instr = p3;
		q->retry = QUERY_NOOP;
		return true;
	}

	return do_soft_if_then_else(q, p1, p2, p3);
}

// goal ; goal

static bool bif_iso_disjunction_2(query *q)
{
	if (q->retry) {
		q->st.curr_instr++;
		return true;
	}

	cell *c = q->st.curr_instr+1;

	if (is_callable(c) && c->bif_ptr) {
		if (c->bif_ptr->fn == bif_iso_if_then_2) {
			cell *p1 = c + 1;
			cell *p2 = p1 + p1->nbr_cells;
			cell *p3 = p2 + p2->nbr_cells;
			return do_if_then_else(q, p1, p2, p3);
		}

		if (c->bif_ptr->fn == bif_soft_if_then_2) {
			cell *p1 = c + 1;
			cell *p2 = p1 + p1->nbr_cells;
			cell *p3 = p2 + p2->nbr_cells;
			return do_soft_if_then_else(q, p1, p2, p3);
		}
	}

	GET_FIRST_ARG(p1,callable);
	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 2);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_true_s, bif_iso_true_0, 0, 0); // Why???
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_choice(q));
	q->st.curr_instr = tmp;
	return true;
}

// if -> !, fail ; true

static bool bif_iso_negation_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 5);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_instr(tmp+nbr_cells++, g_fail_s, bif_iso_fail_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_succeed_on_retry(q, 0));
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_sys_block_catcher_1(query *q)
{
	GET_FIRST_RAW_ARG(p1,integer)
	pl_idx cp = get_smalluint(p1);
	choice *ch = GET_CHOICE(cp);

	if (!ch->catchme_retry)
		return false;

	if (q->retry) {
		ch->block_catcher = false;
		return false;
	}

	if (drop_barrier(q, cp))
		return true;

	ch->block_catcher = true;
	check_heap_error(push_choice(q));
	return true;
}

static bool bif_iso_catch_3(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (q->retry && q->ball) {
		GET_NEXT_ARG(p2,any);
		return unify(q, p2, p2_ctx, q->ball, q->ball_ctx);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		check_pressure(q);
		q->error = false;
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,any);
		q->retry = QUERY_OK;
		cell *tmp = prepare_call(q, PREFIX_LEN, p3, p3_ctx, 3);
		check_heap_error(tmp);
		pl_idx nbr_cells = PREFIX_LEN + p3->nbr_cells;
		make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_uint(tmp+nbr_cells++, q->cp);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_catcher(q, QUERY_EXCEPTION));
		q->st.curr_instr = tmp;
		return true;
	}

	if (q->retry)
		return false;

	// First time through? Try the primary goal...

	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_block_catcher_s, bif_sys_block_catcher_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_catcher(q, QUERY_RETRY));
	q->st.curr_instr = tmp;
	return true;
}

bool bif_sys_set_if_var_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_var(p1))
		return true;

	return unify(q, p1, p1_ctx, p2, p2_ctx);
}

static bool bif_reset_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 3+p3->nbr_cells+2);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_instr(tmp+nbr_cells++, g_sys_set_if_var_s, bif_sys_set_if_var_2, 2, p3->nbr_cells+1);
	nbr_cells += dup_cells_by_ref(tmp+nbr_cells, p3, p3_ctx, p3->nbr_cells);
	make_atom(tmp+nbr_cells++, g_none_s);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_reset_handler(q));
	q->st.curr_instr = tmp;
	return true;
}

static bool find_reset_handler(query *q)
{
	if (!q->cp)
		return false;

	choice *ch = GET_CURR_CHOICE();

	for (; ch; ch--) {
		if (ch->reset) {
			ch->reset = false;
			q->st.curr_instr = ch->st.curr_instr;
			q->st.curr_frame = ch->st.curr_frame;
			q->st.curr_m = ch->st.curr_m;
			GET_FIRST_ARG0(p1, any, ch->st.curr_instr);
			GET_NEXT_ARG(p2, any);
			GET_NEXT_ARG(p3, any);
			cell tmp;

			if (!q->ball) {
				make_atom(&tmp, g_none_s);
				return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
			}

			if (!unify(q, p2, p2_ctx, q->ball, q->ball_ctx))
				return false;

			q->ball = NULL;

			if (!unify(q, p3, p3_ctx, q->cont, q->cont_ctx))
				return false;

			return true;
		}

		if (ch == q->choices)
			break;
	}

	return false;
}

static bool bif_shift_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	q->ball = p1;
	q->ball_ctx = p1_ctx;
	cell *next = q->st.curr_instr + q->st.curr_instr->nbr_cells;
	cell *tmp2 = alloc_on_heap(q, 1+next->nbr_cells);
	make_instr(tmp2, g_cont_s, NULL, 1, next->nbr_cells);
	dup_cells_by_ref(tmp2+1, next, q->st.curr_frame, next->nbr_cells);
	q->cont = tmp2;
	q->cont_ctx = q->st.curr_frame;
	return find_reset_handler(q);
}

bool bif_sys_call_cleanup_3(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (q->retry && q->ball) {
		GET_NEXT_ARG(p2,any);
		cell *tmp = deep_clone_to_heap(q, q->ball, q->ball_ctx);
		check_heap_error(tmp);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,callable);
		q->retry = QUERY_OK;
		cell *tmp = prepare_call(q, PREFIX_LEN, p3, p3_ctx, 3);
		check_heap_error(tmp);
		pl_idx nbr_cells = PREFIX_LEN + p3->nbr_cells;
		make_instr(tmp+nbr_cells++, g_sys_cleanup_if_det_s, bif_sys_cleanup_if_det_1, 1, 1);
		make_uint(tmp+nbr_cells++, q->cp);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_catcher(q, QUERY_EXCEPTION));
		q->st.curr_instr = tmp;
		return true;
	}

	if (q->retry)
		return false;

	// First time through? Try the primary goal...

	cell *tmp = prepare_call(q, PREFIX_LEN, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_instr(tmp+nbr_cells++, g_sys_cleanup_if_det_s, bif_sys_cleanup_if_det_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_catcher(q, QUERY_RETRY));
	q->st.curr_instr = tmp;
	return true;
}

static cell *parse_to_heap(query *q, const char *src)
{
	SB(s);
	SB_sprintf(s, "%s.", src);
	parser *p2 = parser_create(q->st.curr_m);
	check_error(p2);
	frame *f = GET_CURR_FRAME();
	p2->read_term_slots = f->actual_slots;
	p2->skip = true;
	p2->srcptr = SB_cstr(s);
	tokenize(p2, false, false);
	xref_clause(p2->m, p2->cl, NULL);
	p2->read_term_slots = 0;
	SB_free(s);

	if (p2->nbr_vars) {
		if (create_vars(q, p2->nbr_vars) < 0) {
			parser_destroy(p2);
			return NULL;
		}
	}

	cell *tmp = deep_clone_to_heap(q, p2->cl->cells, q->st.curr_frame);
	check_error(tmp, parser_destroy(p2));
	parser_destroy(p2);
	return tmp;
}

static bool find_exception_handler(query *q, char *ball)
{
	while (retry_choice(q)) {
		const choice *ch = GET_CHOICE(q->cp);

		if (ch->block_catcher)
			continue;

		if (!ch->catchme_retry)
			continue;

		q->ball = parse_to_heap(q, ball);
		q->ball_ctx = q->st.curr_frame;
		q->retry = QUERY_EXCEPTION;

		if (!bif_iso_catch_3(q)) {
			q->ball = NULL;
			continue;
		}

		q->ball = NULL;
		return true;
	}

	cell *e = parse_to_heap(q, ball);
	pl_idx e_ctx = q->st.curr_frame;
	q->did_unhandled_exception = true;

	if (!strcmp(C_STR(q, e+1), "$aborted")) {
		fprintf(stdout, "%% Execution aborted\n");
		q->pl->did_dump_vars = true;
		q->ball = NULL;
		q->error = true;
		return false;
	} else {
		q->ball = deep_clone_to_heap(q, e, e_ctx);
		q->ball_ctx = q->st.curr_frame;
		rebase_term(q, q->ball, 0);
	}

	if (!q->thread_ptr) {
		prolog_lock(q->pl);

		if (!q->is_redo)
			fprintf(stdout, "   ");
		else
			fprintf(stdout, "  ");

		if (q->run_init)
			fprintf(stdout, "\rWarning: Initialization goal exception: ");

		//if (!q->run_init/*!is_interned(e) || strcmp(C_STR(q, e), "error")*/)
		//	fprintf(stdout, "throw(");

		if (is_cyclic_term(q, e, e_ctx)) {
			q->quoted = 1;
			print_term(q, stdout, e, e_ctx, 0);
		} else {
			q->quoted = 1;
			print_term(q, stdout, e, e_ctx, 1);
		}

		//if (!q->run_init/*!is_interned(e) || strcmp(C_STR(q, e), "error")*/)
		//	fprintf(stdout, ")");

		fprintf(stdout, ".\n");
		q->quoted = 0;
		prolog_unlock(q->pl);
	}

	q->pl->did_dump_vars = true;
	//q->error = true;
	q->abort = true;
	return false;
}

static bool bif_iso_throw_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	q->parens = q->numbervars = true;
	q->quoted = true;
	char *ball = print_term_to_strbuf(q, p1, p1_ctx, 1);
	clear_write_options(q);

	if (!find_exception_handler(q, ball)) {
		free(ball);
		return false;
	}

	free(ball);
	return bif_iso_catch_3(q);
}

bool throw_error3(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected, cell *goal)
{
	if (g_tpl_interrupt || q->halt || q->pl->halt)
		return false;

	q->did_throw = true;
	q->quoted = 0;

	if (!strncmp(expected, "iso_", 4))
		expected += 4;

	char tmpbuf[1024];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s", expected);
	tmpbuf[sizeof(tmpbuf)-1] = '\0';
	char *ptr;

	if (!strcmp(err_type, "type_error")
		&& ((ptr = strstr(tmpbuf, "_or")) != NULL))
		*ptr = '\0';

	if (!strcmp(err_type, "type_error") && !strcmp(expected, "stream"))
		err_type = "existence_error";

	expected = tmpbuf;
	char functor[1024];
	functor[0] = '\0';

	if (!strcmp(expected, "smallint"))
		expected = "integer";

	if (!is_var(c)) {
		char *tmpbuf = DUP_STRING(q, goal);
		snprintf(functor, sizeof(functor), "%s", tmpbuf);
		functor[sizeof(functor)-1] = '\0';
		free(tmpbuf);
	}

	int extra = 0;
	const char *eptr = expected;

	while ((eptr = strchr(eptr, ',')) != NULL) {
		extra += 1;
		eptr++;
	}

	bool is_abolish = false;

	if (!strcmp(C_STR(q, q->st.curr_instr), "abolish"))
		is_abolish = true;

	if (!strcmp(C_STR(q, q->st.curr_instr), "$call_check"))
		strcpy(functor, "call");

	bool is_builtin = false, evaluable = false;
	get_builtin_term(q->st.curr_m, c, &is_builtin, &evaluable);
	bool is_op = search_op(q->st.curr_m, C_STR(q, c), NULL, true) > 0;

	cell *tmp;

	if (is_var(c)) {
		err_type = "instantiation_error";
		//printf("error(%s,%s).\n", err_type, expected);
		tmp = alloc_on_heap(q, 3);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 2);
		make_atom(tmp+nbr_cells++, new_atom(q->pl, err_type));
		make_atom(tmp+nbr_cells, new_atom(q->pl, expected));
	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "var")) {
		err_type = "uninstantiation_error";
		//printf("error(%s(%s),(%s)/%u).\n", err_type, C_STR(q, c), functor, goal->arity);
		tmp = alloc_on_heap(q, 6+(c->nbr_cells-1));
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 5+(c->nbr_cells-1));
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 1, 1+(c->nbr_cells-1));
		dup_cells_by_ref(tmp+nbr_cells, c, c_ctx, c->nbr_cells);
		nbr_cells += c->nbr_cells;
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "evaluable")) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, C_STR(q, c), c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 8);
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 2, 4);
		make_atom(tmp+nbr_cells++, new_atom(q->pl, expected));
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "permission_error") && is_compound(c) && CMP_STRING_TO_CSTR(q, c, "/") && is_var(FIRST_ARG(c))) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9+extra);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 8+extra);
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 2+extra, 4+extra);

		if (!extra)
			make_atom(tmp+nbr_cells++, new_atom(q->pl, expected));
		else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, new_atom(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, new_atom(q->pl, ptr));
		}

		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "permission_error") && (is_builtin || (is_op && c->arity)) && !is_abolish) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9+extra);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 8+extra);
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 2+extra, 4+extra);

		if (!extra)
			make_atom(tmp+nbr_cells++, new_atom(q->pl, expected));
		else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, new_atom(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, new_atom(q->pl, ptr));
		}

		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "instantiation_error")) {
		//printf("error(%s,(%s)/%u).\n", err_type, functor, goal->arity);
		tmp = alloc_on_heap(q, 5);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 4);
		make_atom(tmp+nbr_cells++, new_atom(q->pl, err_type));
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "existence_error") && !strcmp(expected, "procedure") && is_callable(c)) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 8);
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 2, 4);
		make_atom(tmp+nbr_cells++, new_atom(q->pl, expected));
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, C_STR(q, c)));
		make_int(tmp+nbr_cells++, c->arity);
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "representation_error")
		|| !strcmp(err_type, "evaluation_error")
		|| !strcmp(err_type, "syntax_error")
		|| !strcmp(err_type, "resource_error")) {
		//printf("error(%s(%s),(%s)/%u).\n", err_type, expected, functor, goal->arity);
		tmp = alloc_on_heap(q, 6);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 5);
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 1, 1);
		make_atom(tmp+nbr_cells++, new_atom(q->pl, expected));
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else {
		//printf("error(%s(%s,(%s)),(%s)/%u).\n", err_type, expected, C_STR(q, c), functor, goal->arity);
		tmp = alloc_on_heap(q, 7+(c->nbr_cells-1)+extra);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_instr(tmp+nbr_cells++, g_error_s, NULL, 2, 6+(c->nbr_cells-1)+extra);
		make_instr(tmp+nbr_cells++, new_atom(q->pl, err_type), NULL, 2+extra, 2+(c->nbr_cells-1)+extra);

		if (!extra) {
			make_atom(tmp+nbr_cells++, new_atom(q->pl, expected));
		} else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, new_atom(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, new_atom(q->pl, ptr));
		}

		nbr_cells += dup_cells_by_ref(tmp+nbr_cells, c, c_ctx, c->nbr_cells);
		make_instr(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, new_atom(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	}

	q->parens = q->numbervars = true;
	q->quoted = true;
	char *ball = print_term_to_strbuf(q, tmp, c_ctx, 1);
	clear_write_options(q);

	if (find_exception_handler(q, ball)) {
		free(ball);
		return bif_iso_catch_3(q);
	}

	free(ball);
	return false;
}

bool throw_error2(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected, cell *goal)
{
	cell tmp;
	tmp = goal[1];
	tmp.arity = get_smallint(&goal[2]);
	return throw_error3(q, c, c_ctx, err_type, expected, &tmp);
}

bool throw_error(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected)
{
	if ((q->st.curr_m->flags.syntax_error == UNK_FAIL) && !strcmp(err_type, "syntax_error"))
		return false;

	return throw_error3(q, c, c_ctx, err_type, expected, q->st.curr_instr);
}

builtins g_control_bifs[] =
{
	{"true", 0, bif_iso_true_0, NULL, true, false, BLAH},
	{"fail", 0, bif_iso_fail_0, NULL, true, false, BLAH},
	{"false", 0, bif_iso_fail_0, NULL, true, false, BLAH},
	{"!", 0, bif_iso_cut_0, NULL, true, false, BLAH},
	{",", 2, bif_iso_conjunction_2, ":callable,:callable", true, false, BLAH},
	{";", 2, bif_iso_disjunction_2, ":callable,:callable", true, false, BLAH},
	{"\\+", 1, bif_iso_negation_1, ":callable", true, false, BLAH},
	{"->", 2, bif_iso_if_then_2, ":callable,:callable", true, false, BLAH},
	{"call", 1, bif_iso_call_1, ":callable", true, false, BLAH},
	{"call", 2, bif_iso_call_n, ":callable,?term", true, false, BLAH},
	{"call", 3, bif_iso_call_n, ":callable,?term,term", true, false, BLAH},
	{"call", 4, bif_iso_call_n, ":callable,?term,?term,?term", true, false, BLAH},
	{"call", 5, bif_iso_call_n, ":callable,?term,?term,?term,?term", true, false, BLAH},
	{"call", 6, bif_iso_call_n, ":callable,?term,?term,?term,?term,?term", true, false, BLAH},
	{"call", 7, bif_iso_call_n, ":callable,?term,?term,?term,?term,?term,?term", true, false, BLAH},
	{"call", 8, bif_iso_call_n, ":callable,?term,?term,?term,?term,?term,?term,?term", true, false, BLAH},
	{"throw", 1, bif_iso_throw_1, "+term", true, false, BLAH},
	{"once", 1, bif_iso_once_1, ":callable", true, false, BLAH},

	{"*->", 2, bif_soft_if_then_2, ":callable,:callable", false, false, BLAH},
	{"if", 3, bif_if_3, ":callable,:callable,:callable", false, false, BLAH},
	{"ignore", 1, bif_ignore_1, ":callable", false, false, BLAH},
	{"reset", 3, bif_reset_3, ":callable,?term,-term", false, false, BLAH},
	{"shift", 1, bif_shift_1, "+term", false, false, BLAH},

	{"$cut", 1, bif_sys_cut_1, "+integer", false, false, BLAH},
	{"$call", 1, bif_sys_call_1, ":callable", true, false, BLAH},
	{"$catch", 3, bif_iso_catch_3, ":callable,?term,:callable", true, false, BLAH},
	{"$block_catcher", 1, bif_sys_block_catcher_1, NULL, false, false, BLAH},
	{"$set_if_var", 2, bif_sys_set_if_var_2, "?term,+term", false, false, BLAH},
	{"$cleanup_if_det", 1, bif_sys_cleanup_if_det_1, NULL, false, false, BLAH},

	{0}
};
