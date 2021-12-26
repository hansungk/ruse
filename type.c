#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: merge this with the one in parse.c?
static void error(struct Context *c, struct SrcLoc loc, const char *fmt, ...) {
	struct Error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	assert(len < (int)sizeof(e.msg));

	arrput(c->errors, e);
}

void do_errors(struct Context *c) {
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

void context_init(struct Context *c, Source *src) {
	memset(c, 0, sizeof(struct Context));
	c->src = src;
	c->scope = makescope();
	c->errors = NULL;
}

void context_free(struct Context *c) {
	freescope(c->scope);
	arrfree(c->valstack.stack);
	// FIXME: free errors[i].msg
	free(c->errors);
}

void push_scope(struct Context *c) {
	struct Scope *new_scope = makescope();
	new_scope->outer = c->scope;
	c->scope = new_scope;
}

void pop_scope(struct Context *c) {
	struct Scope *innermost = c->scope;
	c->scope = c->scope->outer;
	freescope(innermost);
}

// Pushes a variable to the current scope.  `n` should be a declaration.
struct Node *push_var(struct Context *c, struct Node *n) {
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
struct Node *lookup_var(struct Context *c, struct Node *n) {
	struct Scope *s = c->scope;
	while (s) {
		struct Node *found = mapget(&s->map, n->tok.name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

static void typecheck_expr(struct Context *c, struct Node *n) {
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
		typecheck_expr(c, n->lhs);
		typecheck_expr(c, n->rhs);
		break;
	case NCALL:
		for (long i = 0; i < arrlen(n->children); i++) {
			typecheck_expr(c, n->children[i]);
		}
		break;
	case NMEMBER:
		typecheck_expr(c, n->parent);

		// TODO: existing member check
		// Lookup parent's decl
		assert(n->parent->decl);
		if (!arrlen(n->parent->decl->children)) {
			error(c, n->tok.loc, "member access to a non-struct");
			return;
		}
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
		push_scope(c);
		for (long i = 0; i < arrlen(n->children); i++) {
			typecheck(c, n->children[i]);
		}
		pop_scope(c);
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
		push_scope(c);
		for (long i = 0; i < arrlen(n->children); i++) {
			typecheck(c, n->children[i]);
		}
		pop_scope(c);
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
