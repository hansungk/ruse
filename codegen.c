#define STB_DS_IMPLEMENTATION
#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void emit(char *c, ...) {
	va_list args;

	va_start(args, c);
	vprintf(c, args);
	va_end(args);

#if 0
    char *s;
    char fmt_buf[1024];
    va_list lst;
    va_start(lst, c);

    char *found = NULL;
    while ((found = strstr(c, "%v")) != NULL) {
        size_t len = found - c;
        memset(fmt_buf, 0, sizeof(fmt_buf));
        strncpy(fmt_buf, c, len);

        printf("before %%v: [%s]\n", fmt_buf);

        fputs(va_arg(lst, char *), stdout);

        c = found + strlen("%v");
    }

    // process trailing fmt string
    printf("trailing: [%s]\n", c);

    va_end(lst);
#endif

#if 0
    while (*c != '\0') {
        if (*c != '%') {
            putchar(*c);
            c++;
            continue;
        }

        c++;

        if (*c == '\0') {
            break;
        }

        switch (*c) {
        case 's':
            fputs(va_arg(lst, char *), stdout);
            break;
        case 'c':
            putchar(va_arg(lst, int));
            break;
	default:
	    // TODO: start here
	    break;
        }
	c++;
    }

    va_end(lst);
#endif
}

// This *has* to be called at stack pusth time to ensure that it is safe to use
// val->qbe_text afterwards.
static char *qbe_val_name(struct qbe_val *val) {
	int len = 0;

	memset(val->qbe_text, 0, sizeof(val->qbe_text));
	switch (val->kind) {
	case VAL_PARAM:
		len = snprintf(val->qbe_text, sizeof(val->qbe_text), "%%%s",
		               val->param_name);
		break;
	case VAL_TEMP:
		len = snprintf(val->qbe_text, sizeof(val->qbe_text), "%%.%d",
		               val->temp_id);
		break;
	case VAL_ADDR:
		len = snprintf(val->qbe_text, sizeof(val->qbe_text), "%%A%d",
		               val->addr_id);
		break;
	default:
		assert(!"unknown valstack kind");
	}

	if (len < 0 || (size_t)len >= sizeof(val->qbe_text)) {
		fatal("%s(): snprintf error", __func__);
	}
	return val->qbe_text;
}

static void valstack_push_param(struct context *ctx, const char *name) {
	struct qbe_val v = {VAL_PARAM, .param_name = name,
	                    .data_size = 4 /* FIXME */};
	qbe_val_name(&v);
	arrput(ctx->valstack.data, v);
}

// Make a new temp value from valstack using the current ID, but don't push it.
// This is useful when a statement is generating a new value, but we don't want
// to push it as we want to access the old stack values that are used as
// operands that produce the new value.
static struct qbe_val valstack_make_temp(struct context *ctx) {
	struct qbe_val v = {VAL_TEMP, .temp_id = ctx->valstack.next_temp_id,
	                    .data_size = 4 /* FIXME */};
	qbe_val_name(&v);
	// We don't increment next_temp_id here, we only do that at the push time.
	return v;
}

static void valstack_push_temp(struct context *ctx, const struct qbe_val val) {
	arrput(ctx->valstack.data, val);
	ctx->valstack.next_temp_id++;
}

static void valstack_push_addr(struct context *ctx, int addr_id) {
	struct qbe_val v = {VAL_ADDR, .addr_id = addr_id, .data_size = 8};
	qbe_val_name(&v);
	arrput(ctx->valstack.data, v);
}

static void codegen_expr_value(struct context *ctx, struct node *n);
static void codegen_expr_addr(struct context *ctx, struct node *n);

static int is_param(const struct node *func, const struct node *var) {
	for (long i = 0; i < arrlen(func->func.params); i++) {
		const struct node *arg = func->func.params[i];
		if (strcmp(var->tok.name, arg->tok.name) == 0) {
			return 1;
		}
	}
	return 0;
}

// 'value' is whether codegen_expr() has to generate the actual value of the
// expression and put it on the valstack.  If its value is 0, only the address
// of the expression (which has to be lvalue) will be put on the valstack.
static void codegen_expr(struct context *ctx, struct node *n, int value) {
	char buf[TOKLEN]; // FIXME: stack usage
	struct qbe_val val, val_lhs, val_rhs;

	assert(n);
	switch (n->kind) {
	case NLITERAL:
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		val = valstack_make_temp(ctx);
		emit("    %s =w add 0, %s\n", val.qbe_text, buf);
		valstack_push_temp(ctx, val);
		break;
	case NIDEXPR:
		assert(ctx->scope);
		if (value) {
			if (ctx->scope->decl->kind == NFUNC &&
			    is_param(ctx->scope->decl, n)) {
				// Do nothing, as parameters are already pushed to the valstack
				// when handling NFUNC.
			} else if (n->decl->type->size == 8) {
				val_lhs = valstack_make_temp(ctx);
				emit("    %s =l loadl %%A%d\n", val_lhs.qbe_text,
				     n->decl->local_id);
				valstack_push_temp(ctx, val_lhs);
			} else {
				val_lhs = valstack_make_temp(ctx);
				emit("    %s =w loadw %%A%d\n", val_lhs.qbe_text,
				     n->decl->local_id);
				valstack_push_temp(ctx, val_lhs);
			}
		} else {
			valstack_push_addr(ctx, n->decl->local_id);
		}
		break;
	case NBINEXPR:
		codegen_expr_value(ctx, n->bin.lhs);
		codegen_expr_value(ctx, n->bin.rhs);
		// 'id_rhs' comes first because lhs is pushed to the stack first
		// during the post-order traversal.
		assert(arrlen(ctx->valstack.data) >= 2);
		val_rhs = arrpop(ctx->valstack.data);
		val_lhs = arrpop(ctx->valstack.data);
		assert(val_rhs.kind != VAL_ADDR);
		assert(val_lhs.kind != VAL_ADDR);
		val = valstack_make_temp(ctx);
		emit("    %s =w add %s, %s\n", val.qbe_text, val_lhs.qbe_text,
		     val_rhs.qbe_text);
		valstack_push_temp(ctx, val);
		break;
	case NDEREFEXPR:
		codegen_expr_value(ctx, n->deref.target);
		// Right now, the target of this derefexpr's value is generated and
		// pushed onto the stack: i.e. value of 'c' in '*c'.  This is the
		// memory location of where the decl '*c' sits.  If we want to generate
		// value of '*c' itself, we have to generate another load.
		if (value) {
			val_rhs = arrpop(ctx->valstack.data);
			val_lhs = valstack_make_temp(ctx);
			if (val_rhs.data_size == 8) {
				emit("    %s =l loadl %s\n", val_lhs.qbe_text,
				     val_rhs.qbe_text);
			} else {
				emit("    %s =w loadw %s\n", val_lhs.qbe_text,
				     val_rhs.qbe_text);
			}
			valstack_push_temp(ctx, val_lhs);
		}
		break;
	case NREFEXPR:
		codegen_expr_addr(ctx, n->ref.target);
		break;
	case NCALL:
		if (!n->call.func->type->rettype) {
			assert(!"func without return value not implemented");
		}
		// Push parameters in reverse order so that they are in correct order
		// when popped.
		for (long i = arrlen(n->call.args) - 1; i >= 0; i--) {
			codegen_expr_value(ctx, n->call.args[i]);
		}
		val_lhs = valstack_make_temp(ctx);
		emit("    %s =w ", val_lhs.qbe_text);
		emit("call $%s(", n->call.func->tok.name);
		for (long i = 0; i < arrlen(n->call.args); i++) {
			val_rhs = arrpop(ctx->valstack.data);
			emit("w %s, ", val_rhs.qbe_text);
		}
		emit(")\n");
		arrput(ctx->valstack.data, val_lhs);
		ctx->valstack.next_temp_id++;
		break;
	default:
		assert(!"unknown expr kind");
	}
}

static void codegen_expr_value(struct context *ctx, struct node *n) {
	codegen_expr(ctx, n, 1);
}

static void codegen_expr_addr(struct context *ctx, struct node *n) {
	codegen_expr(ctx, n, 0);
}

static void codegen_decl(struct context *ctx, struct node *n) {
	char buf[TOKLEN];
	struct qbe_val val;

	tokenstr(ctx->src->buf, n->decl->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NVARDECL:
		// FIXME: is it better to assign decl_id here or in check?
		n->local_id = ctx->curr_decl_id++;
		emit("    %%A%d =l alloc4 4\n", n->local_id);
		codegen(ctx, n->var_decl.init_expr);
		assert(arrlen(ctx->valstack.data) > 0);
		val = arrpop(ctx->valstack.data);
		// TODO: proper datasize handling
		// TODO: unify this with assignment
		if (val.data_size == 8) {
			emit("    storel");
		} else {
			emit("    storew");
		}
		emit(" %s, %%A%d\n", qbe_val_name(&val), n->local_id);
		break;
	default:
		assert(!"unreachable");
	}
}

static void codegen_stmt(struct context *ctx, struct node *n) {
	struct qbe_val val_lhs, val_rhs;

	switch (n->kind) {
	case NEXPRSTMT:
		codegen_expr_value(ctx, n->expr_stmt.expr);
		break;
	case NASSIGN:
		codegen_expr_value(ctx, n->assign_expr.init_expr);
		codegen_expr_addr(ctx, n->assign_expr.lhs);
		val_lhs = arrpop(ctx->valstack.data);
		val_rhs = arrpop(ctx->valstack.data);
		if (val_rhs.data_size == 8) {
			emit("    storel");
		} else {
			emit("    storew");
		}
		emit(" %s, %s\n", qbe_val_name(&val_rhs), qbe_val_name(&val_lhs));
		break;
	case NRETURN:
		codegen(ctx, n->return_expr.expr);
		emit("    ret %s\n", arrpop(ctx->valstack.data).qbe_text);
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void codegen(struct context *ctx, struct node *n) {
	assert(n);

	switch (n->kind) {
	case NFILE:
		for (int i = 0; i < arrlen(n->file.body); i++) {
			codegen(ctx, n->file.body[i]);
		}
		break;
	case NFUNC:
		emit("export function w $%s(", n->tok.name);
		for (int i = 0; i < arrlen(n->func.params); i++) {
			if (i > 0) {
				emit(", ");
			}
			const struct node *arg = n->func.params[i];
			emit("w %%%s", arg->tok.name);
		}
		emit(") {\n");
		emit("@start\n");

		assert(n->scope);
		scope_open_with(ctx, n->scope);
		// generate stores of args to stack
		for (int i = 0; i < arrlen(n->func.params); i++) {
			const struct node *arg = n->func.params[i];
			valstack_push_param(ctx, arg->tok.name);
		}
		// body
		for (int i = 0; i < arrlen(n->func.stmts); i++) {
			codegen(ctx, n->func.stmts[i]);
		}
		scope_close(ctx);
		// TODO: free scope here?

		emit("}\n");
		emit("\n");
		break;
	default:
		if (NEXPR <= n->kind && n->kind < NDECL) {
			codegen_expr_value(ctx, n);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			codegen_decl(ctx, n);
		} else if (NSTMT <= n->kind && n->kind) {
			codegen_stmt(ctx, n);
		}
		break;
	}
}
