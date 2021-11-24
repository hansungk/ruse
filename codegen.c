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

static void codegen_expr(Context *ctx, Node *n) {
    char buf[MAXTOKLEN]; // FIXME: stack usage
    int id_lhs, id_rhs;

    tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));

    switch (n->kind) {
    case ND_LITERAL:
        emit("    %%_%d =w add 0, %s\n", ctx->valstack.curr_id, buf);
        valstack_push_and_incr(ctx);
        break;
    case ND_IDEXPR:
        emit("    %%_%d =w add 0, %%%s\n", ctx->valstack.curr_id, buf);
        valstack_push_and_incr(ctx);
        break;
    case ND_BINEXPR:
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

static void codegen_stmt(Context *ctx, Node *n) {
    char buf[MAXTOKLEN];

    switch (n->kind) {
    case ND_EXPRSTMT:
        codegen(ctx, n->rhs);
        break;
    case ND_ASSIGN:
        codegen(ctx, n->rhs);
        tokenstr(ctx->src->src, n->lhs->decl->name, buf, sizeof(buf));
        emit("    %%%s =w add 0, %%_%d\n", buf,
             arrpop(ctx->valstack.stack));
        break;
    case ND_RETURN:
        codegen(ctx, n->rhs);
        emit("    ret %%_%d\n", arrpop(ctx->valstack.stack));
        break;
    default:
        assert(!"unknown stmt kind");
    }
}

void codegen(Context *ctx, Node *n) {
    char buf[MAXTOKLEN];
    int id;

    switch (n->kind) {
    case ND_FILE:
        emit("export function w $main() {\n");
        emit("@start\n");
        for (int i = 0; i < arrlen(n->stmts); i++) {
            codegen(ctx, n->stmts[i]);
        }
        emit("}\n");
        break;
    case ND_FUNC:
        for (int i = 0; i < arrlen(n->stmts); i++) {
            codegen(ctx, n->stmts[i]);
        }
        break;
    case ND_DECL:
        codegen(ctx, n->rhs);

        tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));
        id = arrpop(ctx->valstack.stack);
        emit("    %%%s =w add 0, %%_%d\n", buf, id);
        break;
    default:
        if (ND_START_EXPR < n->kind && n->kind < ND_END_EXPR) {
            codegen_expr(ctx, n);
        } else if (ND_START_STMT < n->kind && n->kind < ND_END_STMT) {
            codegen_stmt(ctx, n);
        }
        break;
    }
}
