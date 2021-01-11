#include "ruse.h"
#include "stretchy_buffer.h"
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

static void codegen_expr(struct Context *ctx, struct Node *n) {
	char buf[MAXTOKLEN];
	tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case ND_LITERAL:
		emit("%s", buf);
		break;
	case ND_IDEXPR:
		emit("%%%s", buf);
		break;
	case ND_BINEXPR:
		codegen_expr(ctx, n->lhs);
		codegen_expr(ctx, n->rhs);
		break;
	default:
		break;
	}
}

void codegen(struct Context *ctx, struct Node *n) {
	char buf[MAXTOKLEN];

	switch (n->kind) {
	case ND_FILE:
		emit("export function w $main() {\n");
		emit("@start\n");
		for (int i = 0; i < sb_count(n->children); i++) {
			codegen(ctx, n->children[i]);
		}
		emit("}\n");
		break;
	case ND_FUNC:
		// TODO: push/pop scope
		for (int i = 0; i < sb_count(n->children); i++) {
			codegen(ctx, n->children[i]);
		}
		break;
	case ND_DECL:
		tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));
		emit("	%%%s =w add 0, ", buf);
		codegen(ctx, n->rhs);
		emit("\n");
		break;
	case ND_EXPRSTMT:
		emit("	%%_ =w add 0, ");
		codegen(ctx, n->rhs);
		emit("\n");
		break;
	case ND_RETURN:
		emit("	ret ");
		codegen(ctx, n->rhs);
		emit("\n");
		break;
	default:
		if (ND_START_EXPR < n->kind && n->kind < ND_END_EXPR) {
			codegen_expr(ctx, n);
		}
		break;
	}
}
