#include "ruse.h"
#include "stretchy_buffer.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void context_init(struct Context *ctx, struct Source *src) {
	memset(ctx, 0, sizeof(struct Context));
	ctx->src = src;
	ctx->symtab = calloc(sizeof(struct SymbolTable), 1);
}

void context_free(struct Context *ctx) {
	for (int i = 0; i < sb_count(ctx->symtab->tab); i++) {
		if (ctx->symtab->tab[i].val) {
			free(ctx->symtab->tab[i].val);
		}
	}
	sb_free(ctx->symtab->tab);
	free(ctx->symtab);
}

// TODO: merge this with the one in parse.c
static void error(struct Context *ctx, long loc, const char *fmt, ...) {
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	struct SrcLoc srcloc = locate(ctx->src, loc);
	fprintf(stderr, "error in %s:%d:%d:(%ld): %s\n", srcloc.filename,
		srcloc.line, srcloc.col, loc, msg);
	exit(EXIT_FAILURE);
}

// Push a variable to the current scope.  `n` should be a declaration node.
struct Value *push_var(struct Context *ctx, struct Node *n) {
	printf("decl: ");
	tokenprint(ctx->src->src, n->tok);
	printf("\n");
	struct Value *val = calloc(sizeof(struct Value), 1);
	struct Map map = {.name = n->tok, .val = val};
	sb_push(ctx->symtab->tab, map);
	return val;
}

// Compare the string content of the two tokens.
static int token_equal(const char *src, struct Token t1, struct Token t2) {
	char buf1[MAXTOKLEN], buf2[MAXTOKLEN];
	tokenstr(src, t1, buf1, sizeof(buf1));
	tokenstr(src, t2, buf2, sizeof(buf2));
	return (strcmp(buf1, buf2) == 0);
}

struct Value *lookup_var(struct Context *ctx, struct Node *n) {
	for (int i = 0; i < sb_count(ctx->symtab->tab); i++) {
		if (token_equal(ctx->src->src, n->tok, ctx->symtab->tab[i].name)) {
			return ctx->symtab->tab[i].val;
		}
	}
	return NULL;
}

static void eval_expr(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_LITERAL:
		printf("literal: ");
		tokenprint(ctx->src->src, n->tok);
		printf("\n");
		break;
	case ND_IDEXPR:
		printf("id: ");
		tokenprint(ctx->src->src, n->tok);
		printf("\n");
		n->val = lookup_var(ctx, n);
		if (!n->val) {
			char buf[MAXTOKLEN];
			tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));
			error(ctx, n->tok.range.start,
			      "undeclared variable '%s'", buf);
		}
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
	case ND_DECL:
		push_var(ctx, n);
		break;
	default:
		if (ND_START_EXPR < n->kind && n->kind < ND_END_EXPR) {
			eval_expr(ctx, n);
		}
		break;
	}
}
