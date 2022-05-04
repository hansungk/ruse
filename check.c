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
	struct type *t = calloc(1, sizeof(struct ast_node));
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
static char *typeexprname(const struct ast_type_expr *type_expr, char *buf,
                          size_t buflen) {
	int wlen = 0;
	char *cur = NULL;
	size_t curlen = 0;

	assert(type_expr);
	switch (type_expr->typekind) {
	case TYPE_VAL:
		strncpy(buf, type_expr->tok.name, buflen - 1);
		buf[buflen - 1] = '\0';
		break;
	case TYPE_POINTER:
		wlen = snprintf(buf, buflen, "*");
		curlen = buflen - wlen;
		cur = buf + wlen;
		typeexprname(type_expr->pointee, cur, curlen);
		break;
	case TYPE_FUNC:
		assert(!"unimplemented");
	default:
		printf("type kind=%d\n", type_expr->typekind);
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
		fprintf(stderr, "%s:%d:%d: error: %s\n", e.loc.filename, e.loc.line,
		        e.loc.col, e.msg);
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

// Open a scope, creating a new one.
void scope_open(struct context *ctx) {
	struct scope *new_scope = makescope();
	new_scope->outer = ctx->scope;
	ctx->scope = new_scope;
	// TODO: typescope
}

// Open a scope using an existing scope that is already constructed.
void scope_open_with(struct context *ctx, struct scope *scope) {
	scope->outer = ctx->scope;
	ctx->scope = scope;
}

void scope_close(struct context *ctx) {
	// struct scope *current = ctx->scope;
	ctx->scope = ctx->scope->outer;
	// TODO: freescope(current);
	// TODO: typescope
}

// Declare 'n' in the current scope.  'n' can be a variable or a function.
static struct ast_node *declare(struct context *ctx, struct ast_node *n) {
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
struct ast_node *lookup(struct context *ctx, const struct ast_node *n) {
	struct scope *s = ctx->scope;
	while (s) {
		struct ast_node *found = mapget(&s->map, n->tok.name);
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

// This handles error report on its own.
static struct type *resolve_type_expr(struct context *ctx,
                                      struct ast_type_expr *type_expr) {
	char buf[TOKLEN];
	struct type *type = NULL;

	// type expressions are recursive themselves; make sure to recurse down
	// to instantiate all underlying types
	if (type_expr->pointee) {
		assert(type_expr->typekind == TYPE_POINTER);
		struct type *pointee_type = resolve_type_expr(ctx, type_expr->pointee);
		if (!pointee_type) {
			return NULL;
		}
		return makepointertype(pointee_type, type_expr->pointee->tok);
	}
	typeexprname(type_expr, buf, sizeof(buf));
	type = lookup_type(ctx, buf);
	if (!type) {
		assert(type_expr->typekind == TYPE_VAL);
		error(ctx, type_expr->loc, "unknown type '%s'", buf);
		return NULL;
	}
	assert(type);
	return type;
}

static void check_expr(struct context *ctx, struct ast_node *n) {
	char buf[TOKLEN];
	struct ast_node *decl = NULL;

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
		check_expr(ctx, n->bin.lhs);
		check_expr(ctx, n->bin.rhs);
		if (!n->bin.lhs->type || !n->bin.rhs->type)
			return;
		// TODO: proper type compatibility check
		if (n->bin.lhs->type != n->bin.rhs->type)
			return error(ctx, n->tok.loc,
			             "incompatible types for binary operation");
		n->type = n->bin.lhs->type;
		break;
	case NDEREFEXPR:
		check_expr(ctx, n->deref.target);
		if (!n->deref.target->type)
			return;
		// FIXME: This way every (*c) will generate a new decl.
		// Declare itself.  Don't put this in the symbol map as we don't need
		// these temporary decls to be referrable by any specific name.
		n->decl = n;
		if (n->deref.target->type->kind != TYPE_POINTER)
			return error(ctx, n->loc, "cannot dereference a non-pointer");
		n->type = n->deref.target->type->target;
		break;
	case NREFEXPR:
		check_expr(ctx, n->ref.target);
		if (!n->ref.target->type)
			return;
		// lvalue check
		if (!n->ref.target->decl)
			return error(ctx, n->loc, "cannot take reference of a non-lvalue");
		n->type = makepointertype(n->ref.target->type, n->tok);
		break;
	case NCALL:
		// callee name is a node (n->call.func), not a token!
		check_expr(ctx, n->call.func);
		if (!n->call.func->type) {
			return;
		}
		if (n->call.func->type->kind != TYPE_FUNC) {
			tokenstr(ctx->src->buf, n->call.func->tok, buf, sizeof(buf));
			return error(ctx, n->call.func->loc, "'%s' is not a function", buf);
		}
		if (arrlen(n->call.func->type->params) != arrlen(n->call.args)) {
			return error(ctx, n->call.func->loc,
			             "wrong number of arguments: expected %ld, got %ld",
			             arrlen(n->call.func->type->params),
			             arrlen(n->call.args));
		}
		for (long i = 0; i < arrlen(n->call.args); i++) {
			check_expr(ctx, n->call.args[i]);
			if (!n->call.args[i]->type)
				break;
			assert(n->call.func->type->params[i]->type);
			// TODO: proper type compatibility check
			if (n->call.args[i]->type != n->call.func->type->params[i]->type) {
				char expect_buf[TOKLEN];
				char got_buf[TOKLEN];
				typename(n->call.func->type->params[i]->type, expect_buf,
				         sizeof(expect_buf));
				typename(n->call.args[i]->type, got_buf, sizeof(got_buf));
				return error(ctx, n->call.args[i]->loc,
				             "wrong type of argument: expected %s, got %s",
				             expect_buf, got_buf);
			}
		}
		n->type = n->call.func->type->rettype;
		break;
	case NMEMBER:
		check_expr(ctx, n->member.parent);
		if (!n->member.parent->type)
			return;
		if (!arrlen(n->member.parent->type->members))
			return error(ctx, n->loc, "member access to a non-struct");
		const struct ast_node *field_match = NULL;
		for (long i = 0; i < arrlen(n->member.parent->type->members); i++) {
			const struct ast_node *f = n->member.parent->type->members[i];
			if (strcmp(f->tok.name, n->tok.name) == 0) {
				field_match = f;
				break;
			}
		}
		if (!field_match) {
			return error(ctx, n->loc, "'%s' is not a member of type '%s'",
			             n->tok.name, n->member.parent->type->tok.name);
		}
		n->type = field_match->type;
		n->member.offset = field_match->field.offset;
		break;
	default:
		assert(!"unknown expr kind");
	}
	assert(n->type);
}

static int check_assignment(struct context *ctx, struct ast_node *asignee,
                            struct ast_node *expr) {
	// TODO: proper type compatibility check
	assert(asignee->type && expr->type);
	if (asignee->type != expr->type) {
		error(ctx, asignee->tok.loc, "cannot assign to an incompatible type");
		return 0;
	}
	return 1;
}

static void check_decl(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NVARDECL:
		// var decl has an init expression, ex. var i = 4
		if (n->var_decl.init_expr) {
			check_expr(ctx, n->var_decl.init_expr);
			if (!n->var_decl.init_expr->type) {
				return;
			}
			n->type = n->var_decl.init_expr->type;
		}
		// var decl has a type specifier, ex. var i: int
		if (n->type_expr) {
			n->type = resolve_type_expr(ctx, n->type_expr);
			if (!n->type) {
				return;
			}
		}
		if (n->type_expr && n->var_decl.init_expr) {
			// if both type and init expr is specified, check assignability
			if (!check_assignment(ctx, n, n->var_decl.init_expr)) {
				return;
			}
		}
		// only declare after the whole thing succeeds typecheck
		// FIXME: This is a little awkward because original declarations would
		// have 'n == n->decl'.  Or is this a good thing?  Maybe make a
		// separate Decl struct?
		if (!(n->decl = declare(ctx, n))) {
			return;
		}
		assert(n->type);
		break;
	case NFUNC:
		if (!declare(ctx, n))
			return;
		n->type = maketype(TYPE_FUNC, n->tok);
		// !n->rettypeexpr is possible for void return type
		if (n->func.ret_type_expr) {
			n->type->rettype = resolve_type_expr(ctx, n->func.ret_type_expr);
			if (!n->type->rettype)
				return error(ctx, n->func.ret_type_expr->loc,
				             "unknown type '%s'",
				             n->func.ret_type_expr->tok.name);
		}

		scope_open(ctx);
		n->scope = ctx->scope;
		ctx->scope->decl = n;
		// declare parameters
		for (long i = 0; i < arrlen(n->func.params); i++) {
			assert(n->func.params[i]->kind == NVARDECL);
			check(ctx, n->func.params[i]);
			if (n->func.params[i]->type) {
				arrput(n->type->params, n->func.params[i]);
			} else {
				assert(!"FIXME: what now?");
			}
		}
		// check body
		for (long i = 0; i < arrlen(n->func.stmts); i++) {
			check(ctx, n->func.stmts[i]);
		}
		scope_close(ctx);

		// TODO: check return stmts
		break;
	case NSTRUCT:
		n->type = maketype(TYPE_VAL, n->tok);
		push_type(ctx, n->type);
		// fields
		// Clear the field offset accumulation at struct entry.  This shouldn't
		// be done for inner structs (TODO).
		ctx->accum_field_offset = 0;
		scope_open(ctx);
		for (long i = 0; i < arrlen(n->struct_.fields); i++) {
			struct ast_node *child = n->struct_.fields[i];
			check_decl(ctx, child);
			assert(child->decl);
			arrput(n->type->members, child);
		}
		scope_close(ctx);
		if (!(n->decl = declare(ctx, n))) {
			return;
		}
		assert(n->type);
		break;
	case NFIELD:
		assert(n->type_expr);
		if (!(n->type = resolve_type_expr(ctx, n->type_expr))) {
			return;
		}
		if (!(n->decl = declare(ctx, n))) {
			return;
		}
		n->field.offset = ctx->accum_field_offset;
		ctx->accum_field_offset += 4; // FIXME
		break;
	default:
		printf("n->kind=%d\n", n->kind);
		assert(!"TODO");
	}
}

static void check_stmt(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		check_expr(ctx, n->expr_stmt.expr);
		break;
	case NASSIGN: {
		struct ast_assign_expr assign = n->assign_expr;
		check_expr(ctx, assign.init_expr);
		check_expr(ctx, assign.lhs);
		if (!assign.lhs->type || !assign.init_expr->type)
			return;
		if (!check_assignment(ctx, assign.lhs, assign.init_expr))
			return;
		break;
	}
	case NBLOCKSTMT:
		scope_open(ctx);
		for (long i = 0; i < arrlen(n->block.stmts); i++) {
			check(ctx, n->block.stmts[i]);
		}
		scope_close(ctx);
		break;
	case NRETURN:
		check_expr(ctx, n->return_expr.expr);
		if (!n->return_expr.expr->type)
			return;
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void check(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NFILE:
		for (long i = 0; i < arrlen(n->file.body); i++) {
			check(ctx, n->file.body[i]);
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
