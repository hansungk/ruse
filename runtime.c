#include "ruse.h"
#include "stretchy_buffer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void context_init(struct Context *ctx, const char *src) {
	memset(ctx, 0, sizeof(struct Context));
	ctx->src = src;
}

static void eval_expr(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_LITERAL:
		printf("literal: ");
		tokenprint(ctx->src, n->tok);
		printf("\n");
		break;
	case ND_IDEXPR:
		printf("id: ");
		tokenprint(ctx->src, n->tok);
		printf("\n");
		break;
	case ND_BINEXPR:
		eval_expr(ctx, n->lhs);
		eval_expr(ctx, n->rhs);
		break;
	default:
		break;
	}
}

void run(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_FILE:
		for (int i = 0; i < sb_count(n->children); i++) {
			run(ctx, n->children[i]);
		}
		break;
	case ND_FUNC:
		// TODO: push/pop scope
		for (int i = 0; i < sb_count(n->children); i++) {
			run(ctx, n->children[i]);
		}
		break;
	default:
		if (ND_START_EXPR < n->kind && n->kind < ND_END_EXPR) {
			eval_expr(ctx, n);
		}
		break;
	}
}
