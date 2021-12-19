#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void context_init(struct Context *ctx, Source *src) {
	memset(ctx, 0, sizeof(struct Context));
	ctx->src = src;
	ctx->scope = calloc(sizeof(struct Scope), 1);
}

void context_free(struct Context *ctx) {
	for (int i = 0; i < arrlen(ctx->scope->tab); i++) {
		if (ctx->scope->tab[i].decl) {
			free(ctx->scope->tab[i].decl);
		}
	}
	arrfree(ctx->scope->tab);
	free(ctx->scope);
	arrfree(ctx->valstack.stack);
}

// TODO: merge this with the one in parse.c
static void error(struct Context *ctx, long loc, const char *fmt, ...) {
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	SrcLoc srcloc = locate(ctx->src, loc);
	fprintf(stderr, "error in %s:%d:%d:(%ld): %s\n", srcloc.filename,
		srcloc.line, srcloc.col, loc, msg);
	exit(EXIT_FAILURE);
}

// Push a variable to the current scope.  `n` should be a declaration.
struct Decl *push_var(struct Context *ctx, struct Node *n) {
	struct Decl *val = calloc(sizeof(struct Decl), 1);
	val->name = n->tok;
	struct DeclMap map = {.name = n->tok, .decl = val};
	arrput(ctx->scope->tab, map);
	return val;
}

struct Decl *lookup_var(struct Context *ctx, struct Node *n) {
	for (int i = 0; i < arrlen(ctx->scope->tab); i++) {
		if (tokeneq(ctx->src->src, n->tok, ctx->scope->tab[i].name)) {
			return ctx->scope->tab[i].decl;
		}
	}
	return NULL;
}

static void typecheck_expr(struct Context *ctx, struct Node *n) {
	char buf[MAXTOKLEN];
	tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NLITERAL:
		break;
	case NIDEXPR:
		n->decl = lookup_var(ctx, n);
		if (!n->decl) {
			error(ctx, n->tok.range.start,
			      "undeclared variable '%s'", buf);
		}
		break;
	case NBINEXPR:
		typecheck_expr(ctx, n->lhs);
		typecheck_expr(ctx, n->rhs);
		break;
	default:
		break;
	}
}

static void typecheck_stmt(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		typecheck_expr(ctx, n->rhs);
		break;
	case NASSIGN:
		typecheck_expr(ctx, n->rhs);
		typecheck_expr(ctx, n->lhs);
		break;
	case NRETURN:
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void typecheck(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case NFILE:
		for (int i = 0; i < arrlen(n->children); i++) {
			typecheck(ctx, n->children[i]);
		}
		break;
	case NFUNC:
		// TODO: push/pop scope
		for (int i = 0; i < arrlen(n->children); i++) {
			typecheck(ctx, n->children[i]);
		}
		break;
	case NDECL:
		push_var(ctx, n);
		break;
	default:
		if (NEXPR <= n->kind && n->kind < NDECL) {
			typecheck_expr(ctx, n);
		} else if (NSTMT <= n->kind && n->kind) {
			typecheck_stmt(ctx, n);
		}
		break;
	}
}
