#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Type *ty_int;
static Type *ty_string;

static Type *push_type(Context *ctx, Type *ty);

Type *maketype(enum type_kind kind, Token tok) {
	Type *t = calloc(1, sizeof(struct node));
	if (!t) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	t->kind = kind;
	t->tok = tok;
	t->size = 4;
	// arrput(p->nodeptrbuf, node); // FIXME
	return t;
}

// FIXME: remove 'tok'
Type *makepointertype(Type *target, Token tok) {
	Type *t = maketype(TYPTR, tok);
	t->target = target;
	t->size = 8;
	return t;
}

// TODO: merge this with the one in parse.c?
static void error(Context *ctx, struct src_loc loc, const char *fmt, ...) {
	struct Error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	assert(len < (int)sizeof(e.msg));

	arrput(ctx->errors, e);
}

int do_errors(const Error *errors) {
	for (long i = 0; i < arrlen(errors); i++) {
		struct Error e = errors[i];
		fprintf(stderr, "%s:%d:%d: error: %s\n", e.loc.filename,
		        e.loc.line, e.loc.col, e.msg);
	}
	return arrlen(errors) == 0;
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

static void setup_builtin_types(Context *ctx) {
	// FIXME: free(ty_int), calloc() check
	ty_int = calloc(1, sizeof(Type));
	ty_int->kind = TYVAL;
	ty_int->tok = (Token){.type = TINT, .name = "int"};
	push_type(ctx, ty_int);
	ty_string = calloc(1, sizeof(Type));
	ty_string->kind = TYVAL;
	ty_string->tok = (Token){.type = TSTRING_, .name = "string"};
	push_type(ctx, ty_string);
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
	setup_builtin_types(ctx);
}

void context_free(Context *ctx) {
	freescope(ctx->scope);
	arrfree(ctx->valstack.data);
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

	assert(n);
	tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NLITERAL:
		// TODO: non-int literals
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
		// TODO: proper type compatibility check
		if (n->lhs->type != n->rhs->type)
			return error(ctx, n->tok.loc, "incompatible types for binary operation");
		break;
	case NDEREFEXPR:
		check_expr(ctx, n->rhs);
		if (!n->rhs->type)
			return;
		// Declare itself.  Don't put this in the symbol map as we don't need
		// these temporary decls to be referrable by any specific name.
		n->decl = n;
		if (n->rhs->type->kind != TYPTR)
			return error(ctx, n->tok.loc, "cannot dereference a non-pointer");
		n->type = n->rhs->type->target;
		break;
	case NREFEXPR:
		check_expr(ctx, n->rhs);
		if (!n->rhs->type)
			return;
		// lvalue check
		if (!n->rhs->decl)
			return error(ctx, n->tok.loc,
			             "cannot take reference of a non-lvalue");
		n->type = makepointertype(n->rhs->type, n->tok);
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
			assert(n->lhs->type->params[i]->type);
			// TODO: proper type compatibility check
			if (n->children[i]->type != n->lhs->type->params[i]->type) {
				return error(ctx, n->children[i]->tok.loc,
				             "argument type mismatch: expected %s, got %s",
				             "TODO" /*n->lhs->type->params[i]->type*/,
				             "TODO" /*n->children[i]->type*/);
			}
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
	case NTYPEEXPR:
		if (n->typekind == TYVAL) {
			Type *orig_ty = lookup_type(ctx, n->tok.name);
			if (!orig_ty)
				return error(ctx, n->tok.loc,
				             "unknown type '%s'", n->tok.name);
			n->type = orig_ty;
		} else {
			assert(!"TODO");
		}
		assert(n->type);
		break;
	default:
		assert(!"unknown expr kind");
	}
}

static void check_decl(Context *ctx, struct node *n) {
	switch (n->kind) {
	case NVARDECL:
		// vardecl has a type specifier, e.g. var i: int
		if (n->typeexpr) {
			check_expr(ctx, n->typeexpr);
			if (!n->typeexpr->type)
				return;
			n->type = n->typeexpr->type;
		}
		// infer type from the rhs expression, e.g. var i = 4
		else {
			assert(n->rhs);
			check_expr(ctx, n->rhs);
			if (!n->rhs->type)
				return;
			n->type = n->rhs->type;
		}
		// only declare after the whole thing succeeds typecheck
		// FIXME: This is a little awkward because original declarations would
		// have 'n == n->decl'.  Or is this a good thing?  Maybe make a
		// separate Decl struct?
		if (!(n->decl = declare(ctx, n)))
			return;
		assert(n->type);
		break;
	case NFUNC:
		if (!declare(ctx, n))
			return;
		n->type = maketype(TYFUNC, n->tok);
		// !n->rettypeexpr is possible for void return type
		if (n->rettypeexpr) {
			n->type->rettype =
			    lookup_type(ctx, n->rettypeexpr->tok.name);
			if (!n->type->rettype)
				return error(ctx, n->rettypeexpr->tok.loc,
				             "unknown type '%s'",
				             n->rettypeexpr->tok.name);
		}

		push_scope(ctx);
		// declare parameters
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

		// TODO: check return stmts
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
		if (!n->lhs->type || !n->rhs->type)
			return;
		break;
	case NBLOCKSTMT:
		push_scope(ctx);
		for (long i = 0; i < arrlen(n->children); i++) {
			check(ctx, n->children[i]);
		}
		pop_scope(ctx);
		break;
	case NRETURN:
		check_expr(ctx, n->rhs);
		if (!n->rhs->type)
			return;
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
