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

static int valstack_push_and_incr(Context *ctx) {
    arrput(ctx->valstack.stack, ctx->valstack.curr_id);
    return ctx->valstack.curr_id++;
}

static void codegen_expr(Context *ctx, struct node *n) {
    char buf[TOKLEN]; // FIXME: stack usage
    int id_lhs, id_rhs;

    tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));

    switch (n->kind) {
    case NLITERAL:
        emit("    %%_%d =w add 0, %s\n", ctx->valstack.curr_id, buf);
        valstack_push_and_incr(ctx);
        break;
    case NIDEXPR:
        emit("    %%_%d =w add 0, %%%s\n", ctx->valstack.curr_id, buf);
        valstack_push_and_incr(ctx);
        break;
    case NBINEXPR:
        codegen_expr(ctx, n->lhs);
        codegen_expr(ctx, n->rhs);

        // 'id_rhs' comes first because lhs is pushed to the stack first
        // during the post-order traversal.
        id_rhs = arrpop(ctx->valstack.stack);
        id_lhs = arrpop(ctx->valstack.stack);
        emit("    %%_%d =w add %%_%d, %%_%d\n", ctx->valstack.curr_id,
             id_lhs, id_rhs);
        valstack_push_and_incr(ctx);
        break;
    default:
        assert(!"unknown expr kind");
    }
}

static void codegen_decl(Context *ctx, struct node *n) {
    char buf[TOKLEN];

	codegen(ctx, n->rhs);

	tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
	int id = arrpop(ctx->valstack.stack);
	emit("    %%%s =w add 0, %%_%d\n", buf, id);
}

static void codegen_stmt(Context *ctx, struct node *n) {
    char buf[TOKLEN];

    switch (n->kind) {
    case NEXPRSTMT:
        codegen(ctx, n->rhs);
        break;
    case NASSIGN:
        codegen(ctx, n->rhs);
		// FIXME
        // tokenstr(ctx->src->buf, n->lhs->decl->name, buf, sizeof(buf));
        emit("    %%%s =w add 0, %%_%d\n", buf,
             arrpop(ctx->valstack.stack));
        break;
    case NRETURN:
        codegen(ctx, n->rhs);
        emit("    ret %%_%d\n", arrpop(ctx->valstack.stack));
        break;
    default:
        assert(!"unknown stmt kind");
    }
}

void codegen(Context *ctx, struct node *n) {
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
			codegen_expr(ctx, n);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			codegen_decl(ctx, n);
		} else if (NSTMT <= n->kind && n->kind) {
			codegen_stmt(ctx, n);
		}
		break;
	}
}
