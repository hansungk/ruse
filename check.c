#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Type *push_type(Context *ctx, Type *ty);

// TODO: merge this with the one in parse.c?
static void error(Context *ctx, struct SrcLoc loc, const char *fmt, ...) {
	struct Error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	assert(len < (int)sizeof(e.msg));

	arrput(ctx->errors, e);
}

void do_errors(Context *ctx) {
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

static void init_builtin_types(Context *ctx) {
	// FIXME: free(ty_int)
	Type *ty_int = calloc(1, sizeof(Type));
	ty_int->tok = (Token){.type = TINT, .name = "int"};
	push_type(ctx, ty_int);
}

void context_init(Context *ctx, Source *src) {
	memset(ctx, 0, sizeof(Context));
	ctx->src = src;
	ctx->scope = makescope();
	ctx->typescope = makescope();
	ctx->errors = NULL;
	init_builtin_types(ctx);
}

void context_free(Context *ctx) {
	freescope(ctx->scope);
	arrfree(ctx->valstack.stack);
	// FIXME: free errors[i].msg
	free(ctx->errors);
}

void push_scope(Context *ctx) {
	struct Scope *new_scope = makescope();
	new_scope->outer = ctx->scope;
	ctx->scope = new_scope;
	// TODO: typescope
}

void pop_scope(Context *ctx) {
	struct Scope *innermost = ctx->scope;
	ctx->scope = ctx->scope->outer;
	freescope(innermost);
	// TODO: typescope
}

// Pushes a variable to the current scope.  `n` should be a declaration.
struct node *push_var(Context *ctx, struct node *n) {
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
struct node *lookup_var(Context *ctx, struct node *n) {
	struct Scope *s = ctx->scope;
	while (s) {
		struct node *found = mapget(&s->map, n->tok.name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

// Pushes a new type to the current scope.
static Type *push_type(Context *ctx, Type *ty) {
	if (!mapput(&ctx->typescope->map, ty->tok.name, ty)) {
		char buf[TOKLEN];
		tokenstr(ctx->src->buf, ty->tok, buf, sizeof(buf));
		error(ctx, ty->tok.loc, "'%s' is already declared", buf);
		return NULL;
	}
	return ty;
}

// Finds the type object that first declared the type whose name is 'name'.
Type *lookup_type(Context *ctx, const char *name) {
	struct Scope *s = ctx->typescope;
	while (s) {
		Type *found = mapget(&s->map, name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

static void check_expr(Context *ctx, struct node *n) {
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
		n->type = decl->type;
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
		if (!n->parent->type)
			return;
		if (!arrlen(n->parent->type->members))
			return error(ctx, n->tok.loc,
			             "member access to a non-struct");
		struct node *member_match = NULL;
		for (long i = 0; i < arrlen(n->parent->type->members); i++) {
			struct node *m = n->parent->type->members[i];
			if (strcmp(m->tok.name, n->tok.name) == 0) {
				member_match = m;
				break;
			}
		}
		if (!member_match)
			return error(ctx, n->tok.loc,
			             "'%s' is not a member of type '%s'",
			             n->tok.name, n->parent->type->tok.name);
		n->type = member_match->type;
		break;
	default:
		break;
	}
}

static void check_decl(Context *ctx, struct node *n) {
	switch (n->kind) {
	case NVAR:
		if (!(n->decl = push_var(ctx, n)))
			return;
		if (!n->type) {
			// TODO: What about my type?  Can't get n->decl->type
			// because this is the first place that this variable is
			// declared.

			assert(!"TODO");
		}
		// n->type might not be the same as the Type object of the original
		// type declaration, because n->type has been constructed as a new AST
		// node in the parsing stage.  Therefore we have to be more mindful
		// when looking up the members.
		assert(n->type);
		Type *orig_ty = lookup_type(ctx, n->type->tok.name);
		if (!orig_ty)
			return error(ctx, n->type->tok.loc, "unknown type '%s'",
			             n->type->tok.name);
		// copy over members from original type object
		// TODO: generalize this for other members of struct Type
		n->type->members = orig_ty->members;
		break;
	case NSTRUCT:
		push_type(ctx, n->type);
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

static void check_stmt(Context *ctx, struct node *n) {
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

void check(Context *ctx, struct node *n) {
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
