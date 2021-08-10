#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#include "ruse.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void emit(const char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

static int valstack_push_and_incr(struct Context *ctx) {
    arrput(ctx->valstack.stack, ctx->valstack.curr_id);
    return ctx->valstack.curr_id++;
}

static void codegen_expr(struct Context *ctx, struct Node *n) {
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

static void codegen_stmt(struct Context *ctx, struct Node *n) {
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

void codegen(struct Context *ctx, struct Node *n) {
    char buf[MAXTOKLEN];
    int id;

    switch (n->kind) {
    case ND_FILE:
        emit("export function w $main() {\n");
        emit("@start\n");
        for (int i = 0; i < sb_count(n->stmts); i++) {
            codegen(ctx, n->stmts[i]);
        }
        emit("}\n");
        break;
    case ND_FUNC:
        for (int i = 0; i < sb_count(n->stmts); i++) {
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
