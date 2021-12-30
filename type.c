#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: merge this with the one in parse.c?
static void error(struct context *ctx, struct SrcLoc loc, const char *fmt, ...) {
	struct Error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	assert(len < (int)sizeof(e.msg));

	arrput(ctx->errors, e);
}

void do_errors(struct context *ctx) {
	for (long i = 0; i < arrlen(ctx->errors); i++) {
		struct Error e = ctx->errors[i];
		fprintf(stderr, "%s:%d:%d: error: %s\n",
		        e.loc.filename, e.loc.line, e.loc.col, e.msg);
	}
	if (arrlen(ctx->errors))
		exit(EXIT_FAILURE);
}

static struct Scope *makescope(void) {
	struct Scope *s = calloc(sizeof(struct Scope), 1);
	makemap(&s->map);
	return s;
}

static void freescope(struct Scope *s) {
	freemap(&s->map);
	free(s);
}

void context_init(struct context *ctx, Source *src) {
	memset(ctx, 0, sizeof(struct context));
	ctx->src = src;
	ctx->scope = makescope();
	ctx->errors = NULL;
}

void context_free(struct context *ctx) {
	freescope(ctx->scope);
	arrfree(ctx->valstack.stack);
	// FIXME: free errors[i].msg
	free(ctx->errors);
}

void push_scope(struct context *ctx) {
	struct Scope *new_scope = makescope();
	new_scope->outer = ctx->scope;
	ctx->scope = new_scope;
}

void pop_scope(struct context *ctx) {
	struct Scope *innermost = ctx->scope;
	ctx->scope = ctx->scope->outer;
	freescope(innermost);
}

// Pushes a variable to the current scope.  `n` should be a declaration.
struct node *push_var(struct context *ctx, struct node *n) {
	if (!mapput(&ctx->scope->map, n->tok.name, n)) {
		char buf[TOKLEN];
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		error(ctx, n->tok.loc, "'%s' is already declared", buf);
		return NULL;
	}
	return n;
}

// Finds the declaration node that first declared the variable referenced by
// 'n'.
struct node *lookup_var(struct context *ctx, struct node *n) {
	struct Scope *s = ctx->scope;
	while (s) {
		struct node *found = mapget(&s->map, n->tok.name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

static void check_expr(struct context *ctx, struct node *n) {
	char buf[TOKLEN];
	struct node *decl = NULL;

	tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NLITERAL:
		break;
	case NIDEXPR:
		decl = lookup_var(ctx, n);
		if (!decl) {
			error(ctx, n->tok.loc, "undeclared variable '%s'", buf);
			return;
		}
		n->decl = decl;
		break;
	case NBINEXPR:
		check_expr(ctx, n->lhs);
		check_expr(ctx, n->rhs);
		break;
	case NCALL:
		for (long i = 0; i < arrlen(n->children); i++) {
			check_expr(ctx, n->children[i]);
		}
		break;
	case NMEMBER:
		check_expr(ctx, n->parent);

		// TODO: existing member check
		// Lookup parent's decl
		if (!n->parent->decl)
			return;
		if (!arrlen(n->parent->decl->children)) {
			error(ctx, n->tok.loc, "member access to a non-struct");
			return;
		}
		for (long i = 0; i < arrlen(n->parent->decl->children); i++) {
			printf("looking at field %s\n", n->parent->decl->children[i]->tok.name);
		}
		break;
	default:
		break;
	}
}

static void check_decl(struct context *ctx, struct node *n) {
	switch (n->kind) {
	case NVAR:
		n->decl = push_var(ctx, n);
		// TODO: what about my type?
		break;
	case NSTRUCT:
		for (long i = 0; i < arrlen(n->children); i++) {
			struct node *child = n->children[i];
			check_decl(ctx, child);
			assert(child->decl);
			arrput(n->type->members, child);
		}
		break;
	default:
		printf("n->kind=%d\n", n->kind);
		assert(!"TODO");
	}
}

static void check_stmt(struct context *ctx, struct node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		check_expr(ctx, n->rhs);
		break;
	case NASSIGN:
		check_expr(ctx, n->rhs);
		check_expr(ctx, n->lhs);
		break;
	case NBLOCKSTMT:
		push_scope(ctx);
		for (long i = 0; i < arrlen(n->children); i++) {
			check(ctx, n->children[i]);
		}
		pop_scope(ctx);
		break;
	case NRETURN:
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void check(struct context *ctx, struct node *n) {
	switch (n->kind) {
	case NFILE:
		for (long i = 0; i < arrlen(n->children); i++) {
			check(ctx, n->children[i]);
		}
		break;
	case NFUNC:
		push_scope(ctx);
		for (long i = 0; i < arrlen(n->children); i++) {
			check(ctx, n->children[i]);
		}
		pop_scope(ctx);
		break;
	default:
		if (NEXPR <= n->kind && n->kind < NDECL) {
			check_expr(ctx, n);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			check_decl(ctx, n);
		} else if (NSTMT <= n->kind) {
			check_stmt(ctx, n);
		} else {
			assert(!"unknown node kind");
		}
		break;
	}
}
