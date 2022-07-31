#define STB_DS_IMPLEMENTATION
#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
emitln(struct context *ctx, char *c, ...) {
	va_list args;

	fprintf(ctx->outfile, "\n");
	ctx->qbefmt.line_len = 0;
	for (int i = 0; i < ctx->qbefmt.indent; i++) {
		ctx->qbefmt.line_len += fprintf(ctx->outfile, "    ");
	}
	va_start(args, c);
	ctx->qbefmt.line_len += vfprintf(ctx->outfile, c, args);
	va_end(args);
}

static void
emit(struct context *ctx, char *c, ...) {
	va_list args;

	va_start(args, c);
	ctx->qbefmt.line_len += vfprintf(ctx->outfile, c, args);
	va_end(args);
}

static void
annotate(struct context *ctx, char *c, ...) {
	va_list args;

	// align start of comment
	for (int i = ctx->qbefmt.line_len; i < 32; i++) {
		fputc(' ', ctx->outfile);
	}
	fprintf(ctx->outfile, "# ");
	va_start(args, c);
	vfprintf(ctx->outfile, c, args);
	va_end(args);
}

// This *has* to be called at stack pusth time to ensure that it is safe to use
// val->qbe_text afterwards.
static char *
qbe_val_name(struct qbeval *val) {
	int len = 0;

	memset(val->text, 0, sizeof(val->text));
	switch (val->kind) {
	case VAL_PARAM:
		len = snprintf(val->text, sizeof(val->text), "%%%s", val->param_name);
		break;
	case VAL_TEMP:
		len = snprintf(val->text, sizeof(val->text), "%%.%d", val->temp_id);
		break;
	case VAL_ADDR:
		len = snprintf(val->text, sizeof(val->text), "%%A%d", val->addr_id);
		break;
	default:
		assert(!"unknown valstack kind");
	}

	if (len < 0 || (size_t)len >= sizeof(val->text)) {
		fatal("%s(): snprintf error", __func__);
	}
	return val->text;
}

static void
stack_push_param(struct context *ctx, const char *name) {
	struct qbeval v = {VAL_PARAM, .param_name = name,
	                   .data_size = 4 /* FIXME */};
	qbe_val_name(&v);
	arrput(ctx->valstack.data, v);
}

// Make a new temp value from valstack using the current ID, but don't push it.
// This is useful when a statement is generating a new value, but we don't want
// to push it as we want to access the old stack values that are used as
// operands that produce the new value.
static struct qbeval
stack_make_temp(struct context *ctx) {
	struct qbeval v = {VAL_TEMP, .temp_id = ctx->valstack.next_temp_id,
	                   .data_size = 4 /* FIXME */};
	ctx->valstack.next_temp_id++;
	qbe_val_name(&v);
	// We don't increment next_temp_id here, we only do that at the push time.
	return v;
}

static struct qbeval
stack_make_addr(struct context *ctx, int addr_id) {
	struct qbeval v = {VAL_ADDR, .addr_id = addr_id, .data_size = pointer_size};
	(void)ctx;
	qbe_val_name(&v);
	return v;
}

static void
stack_push_temp(struct context *ctx, const struct qbeval val) {
	arrput(ctx->valstack.data, val);
}

static void
stack_push_addr(struct context *ctx, const struct qbeval val) {
	arrput(ctx->valstack.data, val);
}

static struct qbeval
stack_pop(struct context *ctx) {
	assert(arrlen(ctx->valstack.data) > 0);
	return arrpop(ctx->valstack.data);
}

// Generate a memory load directive.
static void
gen_load(struct context *ctx, struct qbeval val_addr, size_t size) {
	struct qbeval val = stack_make_temp(ctx);
	if (size == 8) {
		emitln(ctx, "%s =l loadl %s", val.text, val_addr.text);
	} else {
		emitln(ctx, "%s =w loadw %s", val.text, val_addr.text);
	}
	stack_push_temp(ctx, val);
}

// Generate a memory store directive.
// TODO: also generate operands in this
static void
gen_store(struct context *ctx, size_t size) {
	if (size == 8) {
		emitln(ctx, "storel");
	} else {
		emitln(ctx, "storew");
	}
}

static void gen_expr_value(struct context *ctx, struct ast_node *n);
static void gen_expr_addr(struct context *ctx, struct ast_node *n);

static int
is_param(const struct ast_node *func, const struct ast_node *var) {
	struct ast_node *p = func->func.params;
	while (p) {
		if (strcmp(var->tok.name, p->tok.name) == 0)
			return 1;
		p = p->next;
	}
	return 0;
}

// Generate address where the "buf" field of an array or a slice resides.
// Note that this does not push the value to the stack but returns it.
static struct qbeval
gen_array_buf(struct context *ctx, struct ast_node *array_decl,
              struct qbeval array_addr) {
	struct qbeval addr = stack_make_temp(ctx);
	struct ast_node *f = lookup_struct_field(array_decl->type, "buf");
	assert(f && f->kind == NFIELD);
	emitln(ctx, "%s =l add %s, %d", addr.text, array_addr.text,
	       f->field.offset);
	annotate(ctx, "'buf' of array");
	return addr;
}

// Generate address where the element at `index` of the array `array_decl`
// resides.
// Note that this does not push the value to the stack but returns it.
static struct qbeval
gen_array_element(struct context *ctx, struct ast_node *array_decl,
                  struct qbeval array_addr, struct qbeval index) {
	struct qbeval buf_addr = gen_array_buf(ctx, array_decl, array_addr);
	// load where the buffer heap memory is actually allocated at
	gen_load(ctx, buf_addr, pointer_size);
	annotate(ctx, "load '%s'", array_decl->tok.name);
	struct qbeval buf_ptr_val = stack_pop(ctx);

	struct qbeval elem_addr = stack_make_temp(ctx);
	emitln(ctx, "%s =l mul %d, %s", elem_addr.text,
	       array_decl->type->base_type->size, index.text);
	annotate(ctx, "stride of array index");
	emitln(ctx, "%s =l add %s, %s", elem_addr.text, elem_addr.text,
	       buf_ptr_val.text);
	annotate(ctx, "offset to array '%s'", array_decl->tok.name);
	return elem_addr;
}

// 'value' is whether gen_expr() has to generate the actual value of the
// expression and put it on the valstack.  If its value is 0, only the address
// of the expression (which has to be lvalue) will be put on the valstack.
static void
gen_expr(struct context *ctx, struct ast_node *n, int value) {
	char buf[TOKLEN];
	struct qbeval val, val_lhs, val_rhs;

	assert(n);
	switch (n->kind) {
	case NLITERAL: {
		tokenstr(ctx->src->text, n->tok, buf, sizeof(buf));
		val = stack_make_temp(ctx);
		char wl = (n->type->size == 8) ? 'l' : 'w';
		// There seems to be no direct way to assign a number literal to a
		// temporary in QBE, so use a bogus add 0.
		emitln(ctx, "%s =%c add 0, %s", val.text, wl, buf);
		annotate(ctx, "constant");
		stack_push_temp(ctx, val);
		break;
	}
	case NIDEXPR:
		if (ctx->scope->decl->kind == NFUNC && is_param(ctx->scope->decl, n)) {
			// Do nothing, as function parameters are already handled in NFUNC.
			break;
		}
		assert(ctx->scope);
		val = stack_make_addr(ctx, n->decl->local_id);
		stack_push_addr(ctx, val);
		if (value) {
			struct qbeval val_addr = stack_pop(ctx);
			gen_load(ctx, val_addr, n->type->size);
			annotate(ctx, "load '%s'\n", n->tok.name);
		}
		break;
	case NBINEXPR: {
		gen_expr_value(ctx, n->bin.lhs);
		gen_expr_value(ctx, n->bin.rhs);
		// 'id_rhs' comes first because lhs is pushed to the stack first.
		assert(arrlen(ctx->valstack.data) >= 2);
		val_rhs = stack_pop(ctx);
		val_lhs = stack_pop(ctx);
		assert(val_rhs.kind != VAL_ADDR);
		assert(val_lhs.kind != VAL_ADDR);
		val = stack_make_temp(ctx);
		// @copypaste from NLITERAL
		char wl = (n->type->size == 8) ? 'l' : 'w';
		emitln(ctx, "%s =%c add %s, %s", val.text, wl, val_lhs.text,
		       val_rhs.text);
		stack_push_temp(ctx, val);
		break;
	}
	case NDEREFEXPR:
		gen_expr_value(ctx, n->deref.target);
		// Right now, the target of this derefexpr's value is generated and
		// pushed onto the stack: i.e. value of 'c' in '*c'.  This is the
		// memory location of where the decl '*c' sits.  If we want to generate
		// value of '*c' itself, we have to generate another load.
		if (value) {
			val_rhs = stack_pop(ctx);
			gen_load(ctx, val_rhs, val_rhs.data_size);
		}
		break;
	case NREFEXPR:
		gen_expr_addr(ctx, n->ref.target);
		break;
	case NSUBSCRIPT: {
		gen_expr_addr(ctx, n->subscript.array);
		struct qbeval array_base_addr = stack_pop(ctx);
		gen_expr_value(ctx, n->subscript.index);
		struct qbeval index_value = stack_pop(ctx);
		struct qbeval element_addr = gen_array_element(
		    ctx, n->subscript.array, array_base_addr, index_value);
		stack_push_temp(ctx, element_addr);
		if (value) {
			element_addr = stack_pop(ctx);
			val = stack_make_temp(ctx);
			// TODO: use gen_load
			emitln(ctx, "%s =w loadw %s", val.text, element_addr.text);
			stack_push_temp(ctx, val);
		}
		break;
	}
	case NCALL: {
		if (!n->call.func->type->return_type)
			assert(!"func without return value not implemented");
		if (strcmp(n->call.func->tok.name, "len") == 0) {
			assert(!"should not happen as this is rewritten in check");
			break;
		} else if (strcmp(n->call.func->tok.name, "alloc") == 0) {
			// Generate malloc(bytesize) which will be assigned to the 'buf'
			// field of the LHS array.
			gen_expr_value(ctx, n->call.args /*first*/);
			struct qbeval arrlen = stack_pop(ctx);
			struct qbeval bytes = stack_make_temp(ctx);
			emitln(ctx, "%s =l mul %d, %s", bytes.text,
			       n->type->base_type->size, arrlen.text);
			struct qbeval heapptr = stack_make_temp(ctx);
			heapptr.data_size = pointer_size; // FIXME arbitrary; can we extract
			                                  // this from the AST node?
			emitln(ctx, "%s =l call $%s(l %s)", heapptr.text, "malloc",
			       bytes.text);

			// Push value of 'len' back so that we can assign it to the 'len'
			// field of the RHS array in gen_assign.
			stack_push_temp(ctx, arrlen);
			stack_push_temp(ctx, heapptr);
			break;
		}
		// Push parameters in reverse order so that they are in correct order
		// when popped.
		for (struct ast_node *arg = n->call.args; arg; arg = arg->next) {
			gen_expr_value(ctx, arg);
		}
		val_lhs = stack_make_temp(ctx);
		// FIXME: hardcoded type size
		emitln(ctx, "%s =w ", val_lhs.text);
		emit(ctx, "call $%s(", n->call.func->tok.name);
		for (long i = 0; i < nodelistlen(n->call.args); i++) {
			val_rhs = stack_pop(ctx);
			emit(ctx, "w %s, ", val_rhs.text);
		}
		emit(ctx, ")");
		arrput(ctx->valstack.data, val_lhs);
		ctx->valstack.next_temp_id++;
		break;
	}
	case NMEMBER: {
		gen_expr_addr(ctx, n->member.parent);
		struct qbeval parent_addr = stack_pop(ctx);
		struct qbeval member_addr = stack_make_temp(ctx);
		emitln(ctx, "%s =l add %s, %d", member_addr.text, parent_addr.text,
		       n->member.offset);
		annotate(ctx, "offset for member '%s'", n->tok.name);
		stack_push_temp(ctx, member_addr);
		if (value) {
			member_addr = stack_pop(ctx);
			gen_load(ctx, member_addr, n->type->size);
			annotate(ctx, "load member '%s'", n->tok.name);
		}
		break;
	}
	default:
		assert(!"unknown expr kind");
	}
}

static void
gen_expr_value(struct context *ctx, struct ast_node *n) {
	gen_expr(ctx, n, 1);
}

static void
gen_expr_addr(struct context *ctx, struct ast_node *n) {
	gen_expr(ctx, n, 0);
}

// This function cannot accept just the ast_nodes instead of qbe_val, because
// it doesn't work for NVARDECL; NVARDECL does not have an LHS expression.
// TODO: rewrite AST at check so that we can just handle NASSIGN.
static void
gen_assign(struct context *ctx, struct ast_node *to, struct ast_node *from,
           struct qbeval to_addr, struct qbeval from_val) {
	struct qbeval target = to_addr;

	if (to->type->kind == TYPE_SLICE) {
		if (from->kind == NCALL) {
			assert(!"this shouldn't happen after 'buf' rewrite in check");

			// old
			gen_store(ctx, from_val.data_size);
			emit(ctx, " %s, %s", from_val.text, target.text);
			return;
		} else {
			// TODO: array to array copy
			assert(!"TODO: only array = func() form of assignment handled");
		}
	}

	gen_store(ctx, from_val.data_size);
	emit(ctx, " %s, %s", from_val.text, target.text);
	annotate(ctx, "assign");
}

static void
gen_decl(struct context *ctx, struct ast_node *n) {
	char buf[TOKLEN];

	assert(n->decl);
	tokenstr(ctx->src->text, n->decl->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NVARDECL: {
		// FIXME: is it better to assign decl_id here or in check?
		n->local_id = ctx->curr_decl_id++;
		// TODO: proper alignment
		struct qbeval val_lhs = stack_make_addr(ctx, n->local_id);
		emitln(ctx, "%s =l alloc4 %ld", val_lhs.text, n->type->size);
		annotate(ctx, "stack variable '%s'", n->tok.name);
		// no need to generate init_expr; AST is rewritten in check
		break;
	}
	case NSTRUCT: {
		struct ast_node *f = n->struct_.fields;
		while (f) {
			gen_decl(ctx, f);
			f = f->next;
		}
		break;
	}
	case NFIELD:
		// no codegen
		break;
	default:
		assert(!"unreachable");
	}
}

static void
gen_stmt(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		gen_expr_value(ctx, n->expr_stmt.expr);
		break;
	case NASSIGN: {
		gen_expr_value(ctx, n->assign_expr.rhs);
		gen_expr_addr(ctx, n->assign_expr.lhs);
		struct qbeval val_lhs = stack_pop(ctx);
		struct qbeval val_rhs = stack_pop(ctx);
		gen_assign(ctx, n->assign_expr.lhs, n->assign_expr.rhs, val_lhs,
		           val_rhs);
		break;
	}
	case NRETURN:
		gen(ctx, n->return_expr.expr);
		emitln(ctx, "ret %s", stack_pop(ctx).text);
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void
gen(struct context *ctx, struct ast_node *n) {
	assert(n);

	switch (n->kind) {
	case NFILE: {
		struct ast_node *b = n->file.body;
		while (b) {
			gen(ctx, b);
			b = b->next;
		}
		break;
	}
	case NFUNC: {
		emitln(ctx, "export function w $%s(", n->tok.name);

		struct ast_node *p = n->func.params;
		while (p) {
			if (p != n->func.params) {
				emit(ctx, ", ");
			}
			emit(ctx, "w %%%s", p->tok.name);
			p = p->next;
		}
		emit(ctx, ") {");
		emitln(ctx, "@start");

		ctx->qbefmt.indent++;
		assert(n->scope);
		scope_open_with(ctx, n->scope);
		// generate stores of args to stack
		p = n->func.params;
		while (p) {
			stack_push_param(ctx, p->tok.name);
			p = p->next;
		}
		// body
		struct ast_node *s = n->func.stmts;
		while (s) {
			gen(ctx, s);
			s = s->next;
		}
		scope_close(ctx);
		ctx->qbefmt.indent--;
		// TODO: free scope here?

		emitln(ctx, "}");
		emitln(ctx, "");
		break;
	}
	default:
		if (NEXPR <= n->kind && n->kind < NDECL) {
			gen_expr_value(ctx, n);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			gen_decl(ctx, n);
		} else if (NSTMT <= n->kind) {
			gen_stmt(ctx, n);
		} else {
			assert(!"unknown node kind");
		}
		break;
	}
}
