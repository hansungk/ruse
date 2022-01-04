#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Type *ty_int;

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

void do_errors(const Error *errors) {
	for (long i = 0; i < arrlen(errors); i++) {
		struct Error e = errors[i];
		fprintf(stderr, "%s:%d:%d: error: %s\n", e.loc.filename,
		        e.loc.line, e.loc.col, e.msg);
	}
	if (arrlen(errors))
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
	ty_int = calloc(1, sizeof(Type));
	ty_int->kind = TYVAL;
	ty_int->tok = (Token){.type = TINT, .name = "int"};
	push_type(ctx, ty_int);
}

// NOTE: This *has* to be called after parse(), as it copies over the error
// list from the parser.
void context_init(Context *ctx, Parser *p) {
	memset(ctx, 0, sizeof(Context));
	ctx->src = &p->l.src;
	ctx->scope = makescope();
	ctx->typescope = makescope();
	// copy over errors
	for (long i = 0; i < arrlen(p->errors); i++) {
		arrput(ctx->errors, p->errors[i]);
	}
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

// Declare 'n' in the current scope.  'n' can be a variable or a function.
struct node *declare(Context *ctx, struct node *n) {
	if (!mapput(&ctx->scope->map, n->tok.name, n)) {
		char buf[TOKLEN];
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		// FIXME: should this be done in the caller?
		error(ctx, n->tok.loc, "'%s' is already declared", buf);
		return NULL;
	}
	return n;
}

// Finds the declaration node that first declared the variable referenced by
// 'n'.
struct node *lookup(Context *ctx, struct node *n) {
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
		// FIXME: non-int literals
		n->type = ty_int;
		break;
	case NIDEXPR:
		if (!(decl = lookup(ctx, n)))
			return error(ctx, n->tok.loc,
			             "undeclared variable '%s'", buf);
		n->decl = decl;
		n->type = decl->type;
		break;
	case NBINEXPR:
		check_expr(ctx, n->lhs);
		check_expr(ctx, n->rhs);
		if (!n->lhs->type || !n->rhs->type)
			return;
		break;
	case NCALL:
		// callee name is a node (n->lhs), not a token!
		check_expr(ctx, n->lhs);
		if (!n->lhs->type)
			return;
		if (n->lhs->type->kind != TYFUNC) {
			tokenstr(ctx->src->buf, n->lhs->tok, buf, sizeof(buf));
			return error(ctx, n->lhs->tok.loc,
			             "'%s' is not a function", buf);
		}
		if (arrlen(n->lhs->type->params) != arrlen(n->children))
			return error(ctx, n->lhs->tok.loc,
			             "argument mismatch: expected %ld, got %ld",
			             arrlen(n->lhs->type->params),
			             arrlen(n->children));
		for (long i = 0; i < arrlen(n->children); i++) {
			check_expr(ctx, n->children[i]);
			if (!n->children[i]->type)
				break;
			assert(!"TODO: type compatibility check");
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
		if (!(n->decl = declare(ctx, n)))
			return;
		if (!n->type) {
			// when there was no explicit type specification, e.g. var i = 4
			assert(n->rhs);
			check_expr(ctx, n->rhs);
			if (!n->rhs->type)
				return;
			n->type = n->rhs->type;
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
	case NFUNC:
		if (!declare(ctx, n))
			return;
		n->type = maketype(TYFUNC, n->tok);

		push_scope(ctx);
		// declare arguments
		for (long i = 0; i < arrlen(n->args); i++) {
			check(ctx, n->args[i]);
			if (n->args[i]->type)
				arrput(n->type->params, n->args[i]);
			else
				assert(!"FIXME: what now?");
		}
		// check body
		for (long i = 0; i < arrlen(n->children); i++) {
			check(ctx, n->children[i]);
		}
		pop_scope(ctx);
		break;
	case NSTRUCT:
		n->type = maketype(TYVAL, n->tok);
		push_type(ctx, n->type);
		// fields
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
