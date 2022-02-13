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

static int valstack_push(struct context *ctx) {
	struct val v = {VAL_TEMP, .temp_id = ctx->valstack.curr_temp_id};
	arrput(ctx->valstack.data, v);
	return ctx->valstack.curr_temp_id++;
}

static void valstack_push_addr(struct context *ctx, int addr_id) {
	struct val v = {VAL_ADDR, .addr_id = addr_id};
    arrput(ctx->valstack.data, v);
}

static char *val_qbe_name(const struct val *val, char *buf, size_t blen) {
	int len;

	switch (val->kind) {
	case VAL_TEMP:
		len = snprintf(buf, blen, "%%.%d", val->temp_id);
		break;
	case VAL_ADDR:
		len = snprintf(buf, blen, "%%A%d", val->addr_id);
		break;
	default:
		assert(!"unknown valstack kind");
	}

	assert(len >= 0 && "sprintf error");
	assert((size_t)len < blen && "sprintf too long for buf");
	return buf;
}

// 'value' is whether codegen_expr() has to generate the actual value of the
// expression and put it on the valstack.  If its value is 0, only the address
// of the expression (which has to be lvalue) will be put on the valstack.
static void codegen_expr(struct context *ctx, struct node *n, int value) {
	char buf[TOKLEN]; // FIXME: stack usage
	struct val val_lhs, val_rhs;

	switch (n->kind) {
	case NLITERAL:
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		emit("    %%.%d =w add 0, %s\n", ctx->valstack.curr_temp_id, buf);
		valstack_push(ctx);
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
			valstack_push(ctx);
		} else {
			valstack_push_addr(ctx, n->decl->id);
		}
		break;
	case NBINEXPR:
		codegen_expr(ctx, n->lhs, 1);
		codegen_expr(ctx, n->rhs, 1);

		// 'id_rhs' comes first because lhs is pushed to the stack first
		// during the post-order traversal.
		assert(arrlen(ctx->valstack.data) >= 2);
		val_rhs = arrpop(ctx->valstack.data);
		val_lhs = arrpop(ctx->valstack.data);
		assert(val_rhs.kind == VAL_TEMP);
		assert(val_lhs.kind == VAL_TEMP);
		emit("    %%.%d =w add %%.%d, %%.%d\n", ctx->valstack.curr_temp_id,
		     val_lhs.temp_id, val_rhs.temp_id);
		valstack_push(ctx);
		break;
	case NREFEXPR:
		codegen_expr(ctx, n->rhs, 0);
		break;
	case NDEREFEXPR:
		// have to generate value here, because dereference
		codegen_expr(ctx, n->rhs, 1);
		if (value) {
			// TODO: generate load here
			assert(!"TODO");
		}
		// n->id = ctx->curr_decl_id++;
		break;
	default:
		assert(!"unknown expr kind");
	}
}

static void codegen_decl(Context *ctx, struct node *n) {
	char buf[TOKLEN];
	struct val val;

	tokenstr(ctx->src->buf, n->decl->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NVAR:
		// FIXME: is it better to assign decl_id here or in check?
		n->id = ctx->curr_decl_id++;
		emit("    %%A%d =l alloc4 4\n", n->id);
		codegen(ctx, n->rhs);
		assert(arrlen(ctx->valstack.data) > 0);
		val = arrpop(ctx->valstack.data);
		// TODO: proper datasize handling
		if (val.kind == VAL_TEMP) {
			emit("    storew %%.%d, %%A%d\n", val.temp_id, n->id);
		} else {
			emit("    storel %%A%d, %%A%d\n", val.addr_id, n->id);
		}
		break;
	default:
		assert(!"unreachable");
	}
}

static void codegen_stmt(Context *ctx, struct node *n) {
	struct val val_lhs, val_rhs;
	char buf[VALLEN];

	switch (n->kind) {
	case NEXPRSTMT:
		codegen_expr(ctx, n->rhs, 1);
		break;
	case NASSIGN:
		codegen_expr(ctx, n->rhs, 1);
		codegen_expr(ctx, n->lhs, 0);
		val_lhs = arrpop(ctx->valstack.data);
		val_rhs = arrpop(ctx->valstack.data);
		val_qbe_name(&val_rhs, buf, sizeof(buf));
		emit("    storew %s", buf);
		val_qbe_name(&val_lhs, buf, sizeof(buf));
		emit(", %s\n", buf);
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
			codegen_expr(ctx, n, 1);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			codegen_decl(ctx, n);
		} else if (NSTMT <= n->kind && n->kind) {
			codegen_stmt(ctx, n);
		}
		break;
	}
}
