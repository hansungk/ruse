#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#include "ruse.h"
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

static int valstack_push_value(struct context *ctx) {
	struct value_handle v = {VAL_TEMP, .temp_id = ctx->valstack.curr_temp_id,
	                         .data_size = 4};
	arrput(ctx->valstack.data, v);
	return ctx->valstack.curr_temp_id++;
}

static void valstack_push_addr(struct context *ctx, int addr_id) {
	struct value_handle v = {VAL_ADDR, .addr_id = addr_id, .data_size = 8};
	arrput(ctx->valstack.data, v);
}

static char *val_qbe_name(struct value_handle *val) {
	// TODO: skip generating if already generated
	int len;

	memset(val->name, 0, sizeof(val->name));
	switch (val->kind) {
	case VAL_TEMP:
		len = snprintf(val->name, sizeof(val->name), "%%.%d", val->temp_id);
		break;
	case VAL_ADDR:
		len = snprintf(val->name, sizeof(val->name), "%%A%d", val->addr_id);
		break;
	default:
		assert(!"unknown valstack kind");
	}

	assert(len >= 0 && "sprintf error");
	assert((size_t)len < sizeof(val->name) && "sprintf too long for buf");
	return val->name;
}

static void codegen_expr_value(struct context *ctx, struct node *n);
static void codegen_expr_addr(struct context *ctx, struct node *n);

// 'value' is whether codegen_expr() has to generate the actual value of the
// expression and put it on the valstack.  If its value is 0, only the address
// of the expression (which has to be lvalue) will be put on the valstack.
static void codegen_expr(struct context *ctx, struct node *n, int value) {
	char buf[TOKLEN]; // FIXME: stack usage
	struct value_handle val_lhs, val_rhs;

	switch (n->kind) {
	case NLITERAL:
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		emit("    %%.%d =w add 0, %s\n", ctx->valstack.curr_temp_id, buf);
		valstack_push_value(ctx);
		break;
	case NIDEXPR:
		if (value) {
			if (n->decl->type->size == 8) {
				emit("    %%.%d =l loadl %%A%d\n",
				     ctx->valstack.curr_temp_id, n->decl->id);
			} else {
				emit("    %%.%d =w loadw %%A%d\n",
				     ctx->valstack.curr_temp_id, n->decl->id);
			}
			valstack_push_value(ctx);
		} else {
			valstack_push_addr(ctx, n->decl->id);
		}
		break;
	case NBINEXPR:
		codegen_expr_value(ctx, n->lhs);
		codegen_expr_value(ctx, n->rhs);

		// 'id_rhs' comes first because lhs is pushed to the stack first
		// during the post-order traversal.
		assert(arrlen(ctx->valstack.data) >= 2);
		val_rhs = arrpop(ctx->valstack.data);
		val_lhs = arrpop(ctx->valstack.data);
		assert(val_rhs.kind == VAL_TEMP);
		assert(val_lhs.kind == VAL_TEMP);
		emit("    %%.%d =w add %%.%d, %%.%d\n", ctx->valstack.curr_temp_id,
		     val_lhs.temp_id, val_rhs.temp_id);
		valstack_push_value(ctx);
		break;
	case NREFEXPR:
		codegen_expr_addr(ctx, n->rhs);
		break;
	case NDEREFEXPR:
		codegen_expr_value(ctx, n->rhs);
		// Right now, the target of this derefexpr's value is generated and
		// pushed onto the stack: i.e. value of 'c' in '*c'.  This is the
		// memory location of where the decl '*c' sits.  If we want to generate
		// value of '*c' itself, we have to generate another load.
		if (value) {
			struct value_handle val = arrpop(ctx->valstack.data);
			if (val.data_size == 8) {
				emit("    %%.%d =l loadl %s\n", ctx->valstack.curr_temp_id,
				     val_qbe_name(&val));
			} else {
				emit("    %%.%d =w loadw %s\n", ctx->valstack.curr_temp_id,
				     val_qbe_name(&val));
			}
			valstack_push_value(ctx);
		}
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

static void codegen_decl(Context *ctx, struct node *n) {
	char buf[TOKLEN];
	struct value_handle val;

	tokenstr(ctx->src->buf, n->decl->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NVARDECL:
		// FIXME: is it better to assign decl_id here or in check?
		n->id = ctx->curr_decl_id++;
		emit("    %%A%d =l alloc4 4\n", n->id);
		codegen(ctx, n->rhs);
		assert(arrlen(ctx->valstack.data) > 0);
		val = arrpop(ctx->valstack.data);
		// TODO: proper datasize handling
		if (val.data_size == 8) {
			emit("    storel");
		} else {
			emit("    storew");
		}
		emit(" %s, %%A%d\n", val_qbe_name(&val), n->id);
		break;
	default:
		assert(!"unreachable");
	}
}

static void codegen_stmt(Context *ctx, struct node *n) {
	struct value_handle val_lhs, val_rhs;

	switch (n->kind) {
	case NEXPRSTMT:
		codegen_expr_value(ctx, n->rhs);
		break;
	case NASSIGN:
		codegen_expr_value(ctx, n->rhs);
		codegen_expr_addr(ctx, n->lhs);
		val_lhs = arrpop(ctx->valstack.data);
		val_rhs = arrpop(ctx->valstack.data);
		if (val_rhs.data_size == 8) {
			emit("    storel");
		} else {
			emit("    storew");
		}
		emit(" %s, %s\n", val_qbe_name(&val_rhs), val_qbe_name(&val_lhs));
		break;
	case NRETURN:
		codegen(ctx, n->rhs);
		emit("    ret %%.%d\n", arrpop(ctx->valstack.data).temp_id);
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void codegen(Context *ctx, struct node *n) {
	assert(n);

	switch (n->kind) {
	case NFILE:
		emit("export function w $main() {\n");
		emit("@start\n");
		for (int i = 0; i < arrlen(n->children); i++) {
			codegen(ctx, n->children[i]);
		}
		emit("}\n");
		break;
	case NFUNC:
		for (int i = 0; i < arrlen(n->children); i++) {
			codegen(ctx, n->children[i]);
		}
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
