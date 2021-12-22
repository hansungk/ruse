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
	makemap(&ctx->scope->map);
}

void context_free(struct Context *ctx) {
	freemap(&ctx->scope->map);
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
struct Node *push_var(struct Context *ctx, struct Node *n) {
	mapput(&ctx->scope->map, n->tok.name, n);
	return n;
}

struct Node *lookup_var(struct Context *c, struct Node *n) {
	struct Node *found = mapget(&c->scope->map, n->tok.name);
	return found;
}

static void typecheck_expr(struct Context *c, struct Node *n) {
	char buf[TOKLEN];
	tokenstr(c->src->buf, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NLITERAL:
		break;
	case NIDEXPR:
		if (!lookup_var(c, n))
			error(c, n->tok.range.start,
			      "undeclared variable '%s'", buf);
		break;
	case NBINEXPR:
		typecheck_expr(c, n->lhs);
		typecheck_expr(c, n->rhs);
		break;
	case NMEMBER:
		typecheck_expr(c, n->parent);
		// TODO: existing member check
		break;
	default:
		break;
	}
}

static void typecheck_stmt(struct Context *c, struct Node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		typecheck_expr(c, n->rhs);
		break;
	case NASSIGN:
		typecheck_expr(c, n->rhs);
		typecheck_expr(c, n->lhs);
		break;
	case NBLOCKSTMT:
		for (long i = 0; i < arrlen(n->children); i++) {
			typecheck(c, n->children[i]);
		}
		break;
	case NRETURN:
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void typecheck(struct Context *c, struct Node *n) {
	switch (n->kind) {
	case NFILE:
		for (long i = 0; i < arrlen(n->children); i++) {
			typecheck(c, n->children[i]);
		}
		break;
	case NFUNC:
		// TODO: push/pop scope
		for (long i = 0; i < arrlen(n->children); i++) {
			typecheck(c, n->children[i]);
		}
		break;
	case NDECL:
		push_var(c, n);
		break;
	default:
		if (NEXPR <= n->kind && n->kind < NDECL) {
			typecheck_expr(c, n);
		} else if (NSTMT <= n->kind) {
			typecheck_stmt(c, n);
		}
		break;
	}
}
