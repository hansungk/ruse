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
		len = snprintf(val->text, sizeof(val->text), "%%%s",
		               val->param_name);
		break;
	case VAL_TEMP:
		len = snprintf(val->text, sizeof(val->text), "%%.%d",
		               val->temp_id);
		break;
	case VAL_ADDR:
		len = snprintf(val->text, sizeof(val->text), "%%A%d",
		               val->addr_id);
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
	qbe_val_name(&v);
	// We don't increment next_temp_id here, we only do that at the push time.
	return v;
}

static void stack_push_temp(struct context *ctx, const struct qbe_val val) {
	arrput(ctx->valstack.data, val);
	ctx->valstack.next_temp_id++;
}

static void stack_push_addr(struct context *ctx, int addr_id) {
	struct qbe_val v = {VAL_ADDR, .addr_id = addr_id, .data_size = 8};
	qbe_val_name(&v);
	arrput(ctx->valstack.data, v);
}

static struct qbe_val stack_pop(struct context *ctx) {
	return arrpop(ctx->valstack.data);
}

static void codegen_expr_value(struct context *ctx, struct ast_node *n);
static void codegen_expr_addr(struct context *ctx, struct ast_node *n);

static int is_param(const struct ast_node *func, const struct ast_node *var) {
	for (long i = 0; i < arrlen(func->func.params); i++) {
		const struct ast_node *arg = func->func.params[i];
		if (strcmp(var->tok.name, arg->tok.name) == 0) {
			return 1;
		}
	}
	return 0;
}

// 'value' is whether codegen_expr() has to generate the actual value of the
// expression and put it on the valstack.  If its value is 0, only the address
// of the expression (which has to be lvalue) will be put on the valstack.
static void codegen_expr(struct context *ctx, struct ast_node *n, int value) {
	char buf[TOKLEN];
	struct qbe_val val, val_lhs, val_rhs;

	assert(n);
	switch (n->kind) {
	case NLITERAL: {
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		val = stack_make_temp(ctx);
		char w_or_l = (n->type->size == 8) ? 'l' : 'w';
		emit(ctx, "    %s =%c add 0, %s\n", val.text, w_or_l, buf);
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
			if (n->decl->type->size == 8) {
				val_lhs = stack_make_temp(ctx);
				emit(ctx, "    %s =l loadl %s\n", val_lhs.text,
				     stack_pop(ctx).text);
				stack_push_temp(ctx, val_lhs);
			} else {
				val_lhs = stack_make_temp(ctx);
				emit(ctx, "    %s =w loadw %s\n", val_lhs.text,
				     stack_pop(ctx).text);
				stack_push_temp(ctx, val_lhs);
			}
		}
		break;
	case NBINEXPR:
		codegen_expr_value(ctx, n->bin.lhs);
		codegen_expr_value(ctx, n->bin.rhs);
		// 'id_rhs' comes first because lhs is pushed to the stack first.
		assert(arrlen(ctx->valstack.data) >= 2);
		val_rhs = stack_pop(ctx);
		val_lhs = stack_pop(ctx);
		assert(val_rhs.kind != VAL_ADDR);
		assert(val_lhs.kind != VAL_ADDR);
		val = stack_make_temp(ctx);
		emit(ctx, "    %s =w add %s, %s\n", val.text, val_lhs.text,
		     val_rhs.text);
		stack_push_temp(ctx, val);
		break;
	case NDEREFEXPR:
		codegen_expr_value(ctx, n->deref.target);
		// Right now, the target of this derefexpr's value is generated and
		// pushed onto the stack: i.e. value of 'c' in '*c'.  This is the
		// memory location of where the decl '*c' sits.  If we want to generate
		// value of '*c' itself, we have to generate another load.
		if (value) {
			val_rhs = stack_pop(ctx);
			val_lhs = stack_make_temp(ctx);
			if (val_rhs.data_size == 8) {
				emit(ctx, "    %s =l loadl %s\n", val_lhs.text,
				     val_rhs.text);
			} else {
				emit(ctx, "    %s =w loadw %s\n", val_lhs.text,
				     val_rhs.text);
			}
			stack_push_temp(ctx, val_lhs);
		}
		break;
	case NREFEXPR:
		codegen_expr_addr(ctx, n->ref.target);
		break;
	case NSUBSCRIPT: {
		codegen_expr_addr(ctx, n->subscript.array);
		// FIXME: can probably merge this with NMEMBER?
		struct qbe_val array_base_addr = stack_pop(ctx);
		codegen_expr_value(ctx, n->subscript.index);
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
	case NCALL:
		if (!n->call.func->type->return_type) {
			assert(!"func without return value not implemented");
		}
		// Push parameters in reverse order so that they are in correct order
		// when popped.
		for (long i = arrlen(n->call.args) - 1; i >= 0; i--) {
			codegen_expr_value(ctx, n->call.args[i]);
		}
		val_lhs = stack_make_temp(ctx);
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
	case NMEMBER: {
		codegen_expr_addr(ctx, n->member.parent);
		struct qbe_val parent_addr = stack_pop(ctx);
		struct qbe_val member_addr = stack_make_temp(ctx);
		emit(ctx, "    %s =l add %s, %d\n", member_addr.text,
		     parent_addr.text, n->member.offset);
		stack_push_temp(ctx, member_addr);
		if (value) {
			member_addr = stack_pop(ctx);
			val = stack_make_temp(ctx);
			emit(ctx, "    %s =w loadw %s\n", val.text, member_addr.text);
			stack_push_temp(ctx, val);
		}
		break;
	}
	default:
		assert(!"unknown expr kind");
	}
}

static void codegen_expr_value(struct context *ctx, struct ast_node *n) {
	codegen_expr(ctx, n, 1);
}

static void codegen_expr_addr(struct context *ctx, struct ast_node *n) {
	codegen_expr(ctx, n, 0);
}

static void codegen_decl(struct context *ctx, struct ast_node *n) {
	char buf[TOKLEN];
	struct qbe_val val;

	assert(n->decl);
	tokenstr(ctx->src->buf, n->decl->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NVARDECL:
		// FIXME: is it better to assign decl_id here or in check?
		n->local_id = ctx->curr_decl_id++;
		emit(ctx, "    %%A%d =l alloc4 4\n", n->local_id);
		if (!n->var_decl.init_expr) {
			break;
		}
		codegen(ctx, n->var_decl.init_expr);
		assert(arrlen(ctx->valstack.data) > 0);
		val = stack_pop(ctx);
		// TODO: proper datasize handling
		// TODO: unify this with assignment
		if (val.data_size == 8) {
			emit(ctx, "    storel");
		} else {
			emit(ctx, "    storew");
		}
		emit(ctx, " %s, %%A%d\n", qbe_val_name(&val), n->local_id);
		break;
	case NSTRUCT:
		for (long i = 0; i < arrlen(n->struct_.fields); i++) {
			codegen_decl(ctx, n->struct_.fields[i]);
		}
		break;
	case NFIELD:
		// no codegen
		break;
	default:
		assert(!"unreachable");
	}
}

static void codegen_stmt(struct context *ctx, struct ast_node *n) {
	struct qbe_val val_lhs, val_rhs;

	switch (n->kind) {
	case NEXPRSTMT:
		codegen_expr_value(ctx, n->expr_stmt.expr);
		break;
	case NASSIGN:
		codegen_expr_value(ctx, n->assign_expr.init_expr);
		codegen_expr_addr(ctx, n->assign_expr.lhs);
		val_lhs = stack_pop(ctx);
		val_rhs = stack_pop(ctx);
		if (val_rhs.data_size == 8) {
			emit(ctx, "    storel");
		} else {
			emit(ctx, "    storew");
		}
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
			codegen_expr_value(ctx, n);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			codegen_decl(ctx, n);
		} else if (NSTMT <= n->kind) {
			codegen_stmt(ctx, n);
		} else {
			assert(!"unknown node kind");
		}
		break;
	}
}
