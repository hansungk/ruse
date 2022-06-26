#define STB_DS_IMPLEMENTATION
#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void emit(struct context *ctx, char *c, ...) {
	va_list args;

	va_start(args, c);
	vfprintf(ctx->outfile, c, args);
	va_end(args);
}

// This *has* to be called at stack pusth time to ensure that it is safe to use
// val->qbe_text afterwards.
static char *qbe_val_name(struct qbe_val *val) {
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

static void stack_push_param(struct context *ctx, const char *name) {
	struct qbe_val v = {VAL_PARAM, .param_name = name,
	                    .data_size = 4 /* FIXME */};
	qbe_val_name(&v);
	arrput(ctx->valstack.data, v);
}

// Make a new temp value from valstack using the current ID, but don't push it.
// This is useful when a statement is generating a new value, but we don't want
// to push it as we want to access the old stack values that are used as
// operands that produce the new value.
static struct qbe_val stack_make_temp(struct context *ctx) {
	struct qbe_val v = {VAL_TEMP, .temp_id = ctx->valstack.next_temp_id,
	                    .data_size = 4 /* FIXME */};
	ctx->valstack.next_temp_id++;
	qbe_val_name(&v);
	// We don't increment next_temp_id here, we only do that at the push time.
	return v;
}

static void stack_push_temp(struct context *ctx, const struct qbe_val val) {
	arrput(ctx->valstack.data, val);
}

static void stack_push_addr(struct context *ctx, int addr_id) {
	struct qbe_val v = {VAL_ADDR, .addr_id = addr_id, .data_size = 8};
	qbe_val_name(&v);
	arrput(ctx->valstack.data, v);
}

static struct qbe_val stack_pop(struct context *ctx) {
	return arrpop(ctx->valstack.data);
}

// Generate a memory load directive.
static void gen_load(struct context *ctx, struct qbe_val val_addr,
                     size_t size) {
	struct qbe_val val = stack_make_temp(ctx);
	if (size == 8) {
		emit(ctx, "    %s =l loadl %s\n", val.text, val_addr.text);
	} else {
		emit(ctx, "    %s =w loadw %s\n", val.text, val_addr.text);
	}
	stack_push_temp(ctx, val);
}

// Generate a memory store directive.
static void gen_store(struct context *ctx, size_t size) {
	if (size == 8) {
		emit(ctx, "    storel");
	} else {
		emit(ctx, "    storew");
	}
}

static void gen_expr_value(struct context *ctx, struct ast_node *n);
static void gen_expr_addr(struct context *ctx, struct ast_node *n);

static int is_param(const struct ast_node *func, const struct ast_node *var) {
	for (long i = 0; i < arrlen(func->func.params); i++) {
		const struct ast_node *arg = func->func.params[i];
		if (strcmp(var->tok.name, arg->tok.name) == 0) {
			return 1;
		}
	}
	return 0;
}

// 'value' is whether gen_expr() has to generate the actual value of the
// expression and put it on the valstack.  If its value is 0, only the address
// of the expression (which has to be lvalue) will be put on the valstack.
static void gen_expr(struct context *ctx, struct ast_node *n, int value) {
	char buf[TOKLEN];
	struct qbe_val val, val_lhs, val_rhs;

	assert(n);
	switch (n->kind) {
	case NLITERAL: {
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		val = stack_make_temp(ctx);
		char wl = (n->type->size == 8) ? 'l' : 'w';
		// There seems to be no direct way to assign a number literal to a
		// temporary in QBE, so use a bogus add 0.
		emit(ctx, "    %s =%c add 0, %s\n", val.text, wl, buf);
		stack_push_temp(ctx, val);
		break;
	}
	case NIDEXPR:
		if (ctx->scope->decl->kind == NFUNC && is_param(ctx->scope->decl, n)) {
			// Do nothing, as function parameters are already handled in NFUNC.
			break;
		}
		assert(ctx->scope);
		stack_push_addr(ctx, n->decl->local_id);
		if (value) {
			struct qbe_val val_addr = stack_pop(ctx);
			gen_load(ctx, val_addr, n->type->size);
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
		emit(ctx, "    %s =%c add %s, %s\n", val.text, wl, val_lhs.text,
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
		// This relies on the fact that the 'buf' pointer of an array struct is
		// at offset 0.
		// FIXME: can probably merge this with NMEMBER?
		struct qbe_val array_base_addr = stack_pop(ctx);
		gen_expr_value(ctx, n->subscript.index);
		assert(n->subscript.index->type->size == 8 /* XXX */);
		struct qbe_val index_value = stack_pop(ctx);
		struct qbe_val element_addr = stack_make_temp(ctx);
		emit(ctx, "    %s =l mul %d, %s\n", element_addr.text,
		     n->subscript.array->type->size, index_value.text);
		emit(ctx, "    %s =l add %s, %s\n", element_addr.text,
		     element_addr.text, array_base_addr.text);
		stack_push_temp(ctx, element_addr);
		if (value) {
			element_addr = stack_pop(ctx);
			val = stack_make_temp(ctx);
			emit(ctx, "    %s =w loadw %s\n", val.text, element_addr.text);
			stack_push_temp(ctx, val);
		}
		break;
	}
	case NCALL: {
		if (!n->call.func->type->return_type) {
			assert(!"func without return value not implemented");
		}
		if (strcmp(n->call.func->tok.name, "len") == 0) {
			assert(value && "taking address of len()?");
			gen_expr_addr(ctx, n->call.args[0]);
			struct qbe_val base_addr = stack_pop(ctx);
			struct qbe_val element_addr = stack_make_temp(ctx);
			emit(ctx, "    %s =l add %s, %d\n", element_addr.text,
			     base_addr.text, 8);
			gen_load(ctx, element_addr, val_rhs.data_size);
			break;
		} else if (strcmp(n->call.func->tok.name, "alloc") == 0) {
			gen_expr_value(ctx, n->call.args[0]);
			struct qbe_val alloc_size = stack_pop(ctx);
			struct qbe_val val = stack_make_temp(ctx);
			emit(ctx, "    %s =l call $%s(l %s)\n", val.text, "malloc",
			     alloc_size.text);
			stack_push_temp(ctx, val);
			break;
		}
		// Push parameters in reverse order so that they are in correct order
		// when popped.
		for (long i = arrlen(n->call.args) - 1; i >= 0; i--) {
			gen_expr_value(ctx, n->call.args[i]);
		}
		val_lhs = stack_make_temp(ctx);
		// FIXME: hardcoded type size
		emit(ctx, "    %s =w ", val_lhs.text);
		emit(ctx, "call $%s(", n->call.func->tok.name);
		for (long i = 0; i < arrlen(n->call.args); i++) {
			val_rhs = stack_pop(ctx);
			emit(ctx, "w %s, ", val_rhs.text);
		}
		emit(ctx, ")\n");
		arrput(ctx->valstack.data, val_lhs);
		ctx->valstack.next_temp_id++;
		break;
	}
	case NMEMBER: {
		gen_expr_addr(ctx, n->member.parent);
		struct qbe_val parent_addr = stack_pop(ctx);
		struct qbe_val member_addr = stack_make_temp(ctx);
		emit(ctx, "    %s =l add %s, %d\n", member_addr.text, parent_addr.text,
		     n->member.offset);
		stack_push_temp(ctx, member_addr);
		if (value) {
			member_addr = stack_pop(ctx);
			gen_load(ctx, member_addr, n->type->size);
		}
		break;
	}
	default:
		assert(!"unknown expr kind");
	}
}

static void gen_expr_value(struct context *ctx, struct ast_node *n) {
	gen_expr(ctx, n, 1);
}

static void gen_expr_addr(struct context *ctx, struct ast_node *n) {
	gen_expr(ctx, n, 0);
}

static void gen_decl(struct context *ctx, struct ast_node *n) {
	char buf[TOKLEN];
	struct qbe_val val;

	assert(n->decl);
	tokenstr(ctx->src->buf, n->decl->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NVARDECL:
		// FIXME: is it better to assign decl_id here or in check?
		n->local_id = ctx->curr_decl_id++;
		// TODO: proper datasize handling
		// TODO: proper alignment
		emit(ctx, "    %%A%d =l alloc4 %ld\n", n->local_id, n->type->size);
		if (!n->var_decl.init_expr) {
			break;
		}
		codegen(ctx, n->var_decl.init_expr);
		assert(arrlen(ctx->valstack.data) > 0);
		val = stack_pop(ctx);
		// TODO: unify this with assignment
		gen_store(ctx, val.data_size);
		emit(ctx, " %s, %%A%d\n", qbe_val_name(&val), n->local_id);
		break;
	case NSTRUCT:
		for (long i = 0; i < arrlen(n->struct_.fields); i++) {
			gen_decl(ctx, n->struct_.fields[i]);
		}
		break;
	case NFIELD:
		// no codegen
		break;
	default:
		assert(!"unreachable");
	}
}

static void gen_stmt(struct context *ctx, struct ast_node *n) {
	struct qbe_val val_lhs, val_rhs;

	switch (n->kind) {
	case NEXPRSTMT:
		gen_expr_value(ctx, n->expr_stmt.expr);
		break;
	case NASSIGN:
		gen_expr_value(ctx, n->assign_expr.init_expr);
		gen_expr_addr(ctx, n->assign_expr.lhs);
		val_lhs = stack_pop(ctx);
		val_rhs = stack_pop(ctx);
		gen_store(ctx, val_rhs.data_size);
		emit(ctx, " %s, %s\n", qbe_val_name(&val_rhs), qbe_val_name(&val_lhs));
		break;
	case NRETURN:
		codegen(ctx, n->return_expr.expr);
		emit(ctx, "    ret %s\n", stack_pop(ctx).text);
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void codegen(struct context *ctx, struct ast_node *n) {
	assert(n);

	switch (n->kind) {
	case NFILE:
		for (int i = 0; i < arrlen(n->file.body); i++) {
			codegen(ctx, n->file.body[i]);
		}
		break;
	case NFUNC:
		emit(ctx, "export function w $%s(", n->tok.name);
		for (int i = 0; i < arrlen(n->func.params); i++) {
			if (i > 0) {
				emit(ctx, ", ");
			}
			const struct ast_node *arg = n->func.params[i];
			emit(ctx, "w %%%s", arg->tok.name);
		}
		emit(ctx, ") {\n");
		emit(ctx, "@start\n");

		assert(n->scope);
		scope_open_with(ctx, n->scope);
		// generate stores of args to stack
		for (int i = 0; i < arrlen(n->func.params); i++) {
			const struct ast_node *arg = n->func.params[i];
			stack_push_param(ctx, arg->tok.name);
		}
		// body
		for (int i = 0; i < arrlen(n->func.stmts); i++) {
			codegen(ctx, n->func.stmts[i]);
		}
		scope_close(ctx);
		// TODO: free scope here?

		emit(ctx, "}\n");
		emit(ctx, "\n");
		break;
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
