#include "ruse.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void context_init(struct Context *ctx, struct Source *src) {
	memset(ctx, 0, sizeof(struct Context));
	ctx->src = src;
	ctx->scope = calloc(sizeof(struct Scope), 1);
}

void context_free(struct Context *ctx) {
	for (int i = 0; i < sb_count(ctx->scope->tab); i++) {
		if (ctx->scope->tab[i].decl) {
			free(ctx->scope->tab[i].decl);
		}
	}
	sb_free(ctx->scope->tab);
	free(ctx->scope);
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
struct Decl *push_var(struct Context *ctx, struct Node *n) {
	struct Decl *val = calloc(sizeof(struct Decl), 1);
	val->name = n->tok;
	struct Map map = {.name = n->tok, .decl = val};
	sb_push(ctx->scope->tab, map);
	return val;
}

struct Decl *lookup_var(struct Context *ctx, struct Node *n) {
	for (int i = 0; i < sb_count(ctx->scope->tab); i++) {
		if (tokeneq(ctx->src->src, n->tok, ctx->scope->tab[i].name)) {
			return ctx->scope->tab[i].decl;
		}
	}
	return NULL;
}

static void eval_expr(struct Context *ctx, struct Node *n) {
	char buf[MAXTOKLEN];
	tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case ND_LITERAL:
		break;
	case ND_IDEXPR:
		n->decl = lookup_var(ctx, n);
		if (!n->decl) {
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

static void eval_stmt(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_EXPRSTMT:
		eval_expr(ctx, n->rhs);
		break;
	case ND_ASSIGN:
		eval_expr(ctx, n->rhs);
		eval_expr(ctx, n->lhs);
		break;
	case ND_RETURN:
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void eval(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_FILE:
		for (int i = 0; i < sb_count(n->stmts); i++) {
			eval(ctx, n->stmts[i]);
		}
		break;
	case ND_FUNC:
		// TODO: push/pop scope
		for (int i = 0; i < sb_count(n->stmts); i++) {
			eval(ctx, n->stmts[i]);
		}
		break;
	case ND_DECL:
		push_var(ctx, n);
		break;
	default:
		if (ND_START_EXPR < n->kind && n->kind < ND_END_EXPR) {
			eval_expr(ctx, n);
		} else if (ND_START_STMT < n->kind && n->kind < ND_END_STMT) {
			eval_stmt(ctx, n);
		}
		break;
	}
}
