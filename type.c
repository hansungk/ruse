#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: merge this with the one in parse.c?
static void error(struct context *c, struct SrcLoc loc, const char *fmt, ...) {
	struct Error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	assert(len < (int)sizeof(e.msg));

	arrput(c->errors, e);
}

void do_errors(struct context *c) {
	for (long i = 0; i < arrlen(c->errors); i++) {
		struct Error e = c->errors[i];
		fprintf(stderr, "%s:%d:%d: error: %s\n",
		        e.loc.filename, e.loc.line, e.loc.col, e.msg);
	}
	if (arrlen(c->errors))
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

void context_init(struct context *c, Source *src) {
	memset(c, 0, sizeof(struct context));
	c->src = src;
	c->scope = makescope();
	c->errors = NULL;
}

void context_free(struct context *c) {
	freescope(c->scope);
	arrfree(c->valstack.stack);
	// FIXME: free errors[i].msg
	free(c->errors);
}

void push_scope(struct context *c) {
	struct Scope *new_scope = makescope();
	new_scope->outer = c->scope;
	c->scope = new_scope;
}

void pop_scope(struct context *c) {
	struct Scope *innermost = c->scope;
	c->scope = c->scope->outer;
	freescope(innermost);
}

// Pushes a variable to the current scope.  `n` should be a declaration.
struct Node *push_var(struct context *c, struct Node *n) {
	if (!mapput(&c->scope->map, n->tok.name, n)) {
		char buf[TOKLEN];
		tokenstr(c->src->buf, n->tok, buf, sizeof(buf));
		error(c, n->tok.loc, "'%s' is already declared", buf);
		return NULL;
	}
	return n;
}

// Finds the declaration node that first declared the variable referenced by
// 'n'.
struct Node *lookup_var(struct context *c, struct Node *n) {
	struct Scope *s = c->scope;
	while (s) {
		struct Node *found = mapget(&s->map, n->tok.name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

static void check_expr(struct context *c, struct Node *n) {
	char buf[TOKLEN];
	struct Node *decl = NULL;

	tokenstr(c->src->buf, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NLITERAL:
		break;
	case NIDEXPR:
		decl = lookup_var(c, n);
		if (!decl)
			error(c, n->tok.loc, "undeclared variable '%s'", buf);
		n->decl = decl;
		break;
	case NBINEXPR:
		check_expr(c, n->lhs);
		check_expr(c, n->rhs);
		break;
	case NCALL:
		for (long i = 0; i < arrlen(n->children); i++) {
			check_expr(c, n->children[i]);
		}
		break;
	case NMEMBER:
		check_expr(c, n->parent);

		// TODO: existing member check
		// Lookup parent's decl
		assert(n->parent->decl);
		if (!arrlen(n->parent->decl->children)) {
			error(c, n->tok.loc, "member access to a non-struct");
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

static void check_decl(struct context *ctx, struct Node *n) {
	push_var(ctx, n);

	// TODO: add children of the original struct declaration to 'n' as well
	if (n->type) {
	}
}

static void check_stmt(struct context *c, struct Node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		check_expr(c, n->rhs);
		break;
	case NASSIGN:
		check_expr(c, n->rhs);
		check_expr(c, n->lhs);
		break;
	case NBLOCKSTMT:
		push_scope(c);
		for (long i = 0; i < arrlen(n->children); i++) {
			check(c, n->children[i]);
		}
		pop_scope(c);
		break;
	case NRETURN:
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void check(struct context *c, struct Node *n) {
	switch (n->kind) {
	case NFILE:
		for (long i = 0; i < arrlen(n->children); i++) {
			check(c, n->children[i]);
		}
		break;
	case NFUNC:
		push_scope(c);
		for (long i = 0; i < arrlen(n->children); i++) {
			check(c, n->children[i]);
		}
		pop_scope(c);
		break;
	default:
		if (NEXPR <= n->kind && n->kind < NDECL) {
			check_expr(c, n);
		} else if (NDECL <= n->kind && n->kind < NSTMT) {
			check_decl(c, n);
		} else if (NSTMT <= n->kind) {
			check_stmt(c, n);
		}
		break;
	}
}
