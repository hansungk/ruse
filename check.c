#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct type *ty_int;
static struct type *ty_string;
static struct type *push_type(struct context *ctx, struct type *ty);

void fatal(const char *fmt, ...) {
	va_list args;

	fprintf(stderr, "fatal: ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");

	exit(EXIT_FAILURE);
}

struct type *maketype(enum type_kind kind, struct token tok) {
	struct type *t = calloc(1, sizeof(struct node));
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
struct type *makepointertype(struct type *target, struct token tok) {
	struct type *t = maketype(TYPE_POINTER, tok);
	t->target = target;
	t->size = 8;
	return t;
}

static char *typename(const struct type *type, char *buf, size_t buflen) {
	int wlen = 0;
	char *cur = NULL;
	size_t curlen = 0;

	switch (type->kind) {
	case TYPE_VAL:
		strncpy(buf, type->tok.name, buflen - 1);
		buf[buflen - 1] = '\0';
		break;
	case TYPE_POINTER:
		wlen = snprintf(buf, buflen, "*");
		curlen = buflen - wlen;
		cur = buf + wlen;
		typename(type->target, cur, curlen);
		break;
	case TYPE_FUNC:
		assert(!"unimplemented");
	default:
		assert(!"unknown type kind");
	}

	if (wlen < 0 || (size_t)wlen > buflen - 1) {
		fatal("%s(): snprintf error", __func__);
	}
	return buf;
}

// XXX: @copypaste from typename()
static char *typeexprname(const struct node *typeexpr, char *buf,
                          size_t buflen) {
	int wlen = 0;
	char *cur = NULL;
	size_t curlen = 0;

	switch (typeexpr->typekind) {
	case TYPE_VAL:
		strncpy(buf, typeexpr->tok.name, buflen - 1);
		buf[buflen - 1] = '\0';
		break;
	case TYPE_POINTER:
		wlen = snprintf(buf, buflen, "*");
		curlen = buflen - wlen;
		cur = buf + wlen;
		typeexprname(typeexpr->rhs, cur, curlen);
		break;
	case TYPE_FUNC:
		assert(!"unimplemented");
	default:
		assert(!"unknown type kind");
	}

	if (wlen < 0 || (size_t)wlen > buflen - 1) {
		fatal("%s(): snprintf error", __func__);
	}
	return buf;
}

// TODO: merge this with the one in parse.c?
static void error(struct context *ctx, struct src_loc loc, const char *fmt,
                  ...) {
	struct error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	if ((size_t)len >= sizeof(e.msg))
		fatal("%s(): vsnprintf error", __func__);

	arrput(ctx->errors, e);
}

int do_errors(const struct error *errors) {
	for (long i = 0; i < arrlen(errors); i++) {
		struct error e = errors[i];
		fprintf(stderr, "%s:%d:%d: error: %s\n", e.loc.filename,
		        e.loc.line, e.loc.col, e.msg);
	}
	return arrlen(errors) == 0;
}

static struct scope *makescope(void) {
	struct scope *s = calloc(sizeof(struct scope), 1);
	makemap(&s->map);
	return s;
}

static void freescope(struct scope *s) {
	freemap(&s->map);
	free(s);
}

static void setup_builtin_types(struct context *ctx) {
	// FIXME: free(ty_int), calloc() check
	ty_int = calloc(1, sizeof(struct type));
	ty_int->kind = TYPE_VAL;
	ty_int->tok = (struct token){.type = TINT, .name = "int"};
	push_type(ctx, ty_int);
	ty_string = calloc(1, sizeof(struct type));
	ty_string->kind = TYPE_VAL;
	ty_string->tok = (struct token){.type = TSTRING_, .name = "string"};
	push_type(ctx, ty_string);
}

// NOTE: This *has* to be called after parse(), as it copies over the error
// list from the parser.
void context_init(struct context *ctx, struct parser *p) {
	memset(ctx, 0, sizeof(struct context));
	ctx->src = &p->l.src;
	ctx->scope = makescope();
	ctx->typescope = makescope();
	// copy over errors
	for (long i = 0; i < arrlen(p->errors); i++) {
		arrput(ctx->errors, p->errors[i]);
	}
	setup_builtin_types(ctx);
}

void context_free(struct context *ctx) {
	freescope(ctx->scope);
	arrfree(ctx->valstack.data);
	// FIXME: free errors[i].msg
	free(ctx->errors);
}

void push_scope(struct context *ctx) {
	struct scope *new_scope = makescope();
	new_scope->outer = ctx->scope;
	ctx->scope = new_scope;
	// TODO: typescope
}

void pop_scope(struct context *ctx) {
	struct scope *innermost = ctx->scope;
	ctx->scope = ctx->scope->outer;
	freescope(innermost);
	// TODO: typescope
}

// Declare 'n' in the current scope.  'n' can be a variable or a function.
static struct node *declare(struct context *ctx, struct node *n) {
	if (!mapput(&ctx->scope->map, n->tok.name, n)) {
		char buf[TOKLEN];
		tokenstr(ctx->src->buf, n->tok, buf, sizeof(buf));
		// FIXME: should this be done in the caller?
		error(ctx, n->loc, "'%s' is already declared", buf);
		return NULL;
	}
	return n;
}

// Finds the declaration node that first declared the variable referenced by
// 'n'.
static struct node *lookup(struct context *ctx, struct node *n) {
	struct scope *s = ctx->scope;
	while (s) {
		struct node *found = mapget(&s->map, n->tok.name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

// Pushes a new type to the current scope.
static struct type *push_type(struct context *ctx, struct type *ty) {
	char buf[TOKLEN];

	typename(ty, buf, sizeof(buf));
	if (!mapput(&ctx->typescope->map, buf, ty)) {
		error(ctx, ty->tok.loc, "'%s' is already declared", buf);
		return NULL;
	}
	return ty;
}

// Pushes a new type to the global scope.  This can be used to push derived
// types of builtin types, e.g. *int.
// FIXME: not used.
static struct type *push_type_global(struct context *ctx, struct type *ty) {
	char buf[TOKLEN];

	struct scope *scope = ctx->typescope;
	while (scope->outer) {
		scope = scope->outer;
	}

	typename(ty, buf, sizeof(buf));
	if (!mapput(&scope->map, buf, ty)) {
		error(ctx, ty->tok.loc, "'%s' is already declared", buf);
		return NULL;
	}
	return ty;
}

// Finds the type object that first declared the type whose name is 'name'.
static struct type *lookup_type(struct context *ctx, const char *name) {
	struct scope *s = ctx->typescope;
	while (s) {
		struct type *found = mapget(&s->map, name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

static void check_expr(struct context *ctx, struct node *n) {
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
		if (!(decl = lookup(ctx, n))) {
			return error(ctx, n->loc, "undeclared variable '%s'", buf);
		}
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
			return error(ctx, n->tok.loc,
			             "incompatible types for binary operation");
		n->type = n->lhs->type;
		break;
	case NDEREFEXPR:
		check_expr(ctx, n->rhs);
		if (!n->rhs->type)
			return;
		// FIXME: This way every (*c) will generate a new decl.
		// Declare itself.  Don't put this in the symbol map as we don't need
		// these temporary decls to be referrable by any specific name.
		n->decl = n;
		if (n->rhs->type->kind != TYPE_POINTER)
			return error(ctx, n->loc, "cannot dereference a non-pointer");
		n->type = n->rhs->type->target;
		break;
	case NREFEXPR:
		check_expr(ctx, n->rhs);
		if (!n->rhs->type)
			return;
		// lvalue check
		if (!n->rhs->decl)
			return error(ctx, n->loc, "cannot take reference of a non-lvalue");
		n->type = makepointertype(n->rhs->type, n->tok);
		break;
	case NCALL:
		// callee name is a node (n->lhs), not a token!
		check_expr(ctx, n->lhs);
		if (!n->lhs->type) {
			return;
        }
		if (n->lhs->type->kind != TYPE_FUNC) {
			tokenstr(ctx->src->buf, n->lhs->tok, buf, sizeof(buf));
			return error(ctx, n->lhs->loc, "'%s' is not a function", buf);
		}
		if (arrlen(n->lhs->type->params) != arrlen(n->children)) {
			return error(ctx, n->lhs->loc,
			             "argument mismatch: expected %ld, got %ld",
			             arrlen(n->lhs->type->params), arrlen(n->children));
		}
		for (long i = 0; i < arrlen(n->children); i++) {
			check_expr(ctx, n->children[i]);
			if (!n->children[i]->type)
				break;
			assert(n->lhs->type->params[i]->type);
			// TODO: proper type compatibility check
			if (n->children[i]->type != n->lhs->type->params[i]->type) {
				char expect_buf[TOKLEN];
				char got_buf[TOKLEN];
				typename(n->lhs->type->params[i]->type, expect_buf,
				         sizeof(expect_buf));
				typename(n->children[i]->type, got_buf, sizeof(got_buf));
				return error(ctx, n->children[i]->loc,
				             "argument type mismatch: expected %s, got %s",
				             expect_buf, got_buf);
			}
		}
		n->type = n->lhs->type->rettype;
		break;
	case NMEMBER:
		check_expr(ctx, n->parent);
		if (!n->parent->type)
			return;
		if (!arrlen(n->parent->type->members))
			return error(ctx, n->loc, "member access to a non-struct");
		struct node *member_match = NULL;
		for (long i = 0; i < arrlen(n->parent->type->members); i++) {
			struct node *m = n->parent->type->members[i];
			if (strcmp(m->tok.name, n->tok.name) == 0) {
				member_match = m;
				break;
			}
		}
		if (!member_match)
			return error(ctx, n->loc, "'%s' is not a member of type '%s'",
			             n->tok.name, n->parent->type->tok.name);
		n->type = member_match->type;
		break;
	case NTYPEEXPR:
		// type expressions are recursive themselves; make sure to recurse down
		// to instantiate all underlying types
		if (n->rhs) {
			check_expr(ctx, n->rhs);
			if (!n->rhs->type)
				return;
		}
		typeexprname(n, buf, sizeof(buf));
		n->type = lookup_type(ctx, buf);
		if (!n->type) {
			if (n->typekind == TYPE_VAL) {
				return error(ctx, n->loc, "unknown type '%s'", buf);
			} else {
				assert(n->typekind == TYPE_POINTER);
				n->type = makepointertype(n->rhs->type, n->tok);
			}
		}
		assert(n->type);
		break;
	default:
		assert(!"unknown expr kind");
	}
	assert(n->type);
}

static int check_assignment(struct context *ctx, struct node *asignee,
                             struct node *expr) {
	// TODO: proper type compatibility check
	assert(asignee->type && expr->type);
	if (asignee->type != expr->type) {
		error(ctx, asignee->tok.loc,
		             "cannot assign to an incompatible type");
		return 0;
	}
	return 1;
}

static void check_decl(struct context *ctx, struct node *n) {
	switch (n->kind) {
	case NVARDECL:
		// var decl has an init expression, ex. var i = 4
		if (n->rhs) {
			check_expr(ctx, n->rhs);
			if (!n->rhs->type) {
				return;
            }
			n->type = n->rhs->type;
		}
		// var decl has a type specifier, ex. var i: int
		if (n->typeexpr) {
			check_expr(ctx, n->typeexpr);
			if (!n->typeexpr->type)
				return;
			n->type = n->typeexpr->type;
		}
		if (n->typeexpr && n->rhs) {
			// if both type and init expr is specified, check assignability
			if (!check_assignment(ctx, n, n->rhs))
				return;
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
		n->type = maketype(TYPE_FUNC, n->tok);
		// !n->rettypeexpr is possible for void return type
		if (n->rettypeexpr) {
			n->type->rettype =
			    lookup_type(ctx, n->rettypeexpr->tok.name);
			if (!n->type->rettype)
				return error(ctx, n->rettypeexpr->loc, "unknown type '%s'",
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
		n->type = maketype(TYPE_VAL, n->tok);
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

static void check_stmt(struct context *ctx, struct node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		check_expr(ctx, n->rhs);
		break;
	case NASSIGN:
		check_expr(ctx, n->rhs);
		check_expr(ctx, n->lhs);
		if (!n->lhs->type || !n->rhs->type)
			return;
		if (!check_assignment(ctx, n->lhs, n->rhs))
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

void check(struct context *ctx, struct node *n) {
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
