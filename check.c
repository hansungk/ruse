#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct type *ty_undef;
static struct type *ty_int;
static struct type *ty_int64;
static struct type *ty_string;
struct ast_node *fn_alloc = NULL;
static struct ast_node *declare(struct context *ctx, struct ast_node *n);
static struct type *push_type(struct context *ctx, struct type *ty);
static void check_decl(struct context *ctx, struct ast_node *n);

void
fatal(const char *fmt, ...) {
	va_list args;

	fprintf(stderr, "fatal: ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");

	exit(EXIT_FAILURE);
}

struct type *
maketype(enum type_kind kind, struct token tok) {
	struct type *t = calloc(1, sizeof(struct ast_node));
	if (!t) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	t->kind = kind;
	t->tok = tok;
	t->size = 4; // FIXME
	// arrput(p->nodeptrbuf, node); // FIXME
	return t;
}

// FIXME: remove 'tok'
static struct type *
makearraytype(struct type *elem_type, struct token tok) {
	struct type *t = maketype(TYPE_ARRAY, tok);
	t->base_type = elem_type;
	t->size = array_struct_size;
	return t;
}

// FIXME: remove 'tok'
static struct type *
makeslicetype(struct context *ctx, struct type *elem_type, struct token tok) {
	struct type *t = maketype(TYPE_SLICE, tok);
	t->base_type = elem_type;
	t->size = array_struct_size; // FIXME: should be different for slices?

	struct ast_node *te_int =
	    maketypeexpr(ctx->parser, TYPE_ATOM,
	                 (struct token){.type = TINT, .name = strdup("int")});
	struct ast_node *te_intp =
	    maketypeexpr(ctx->parser, TYPE_POINTER,
	                 (struct token){.type = TINT, .name = "int"} /*bogus*/);
	te_intp->type_expr.base_type = te_int;
	struct ast_node *field_decl = NULL;

	scope_open(ctx);

	// TODO: this is almost copy-paste from NSTRUCT:
	struct token tok_len =
	    (struct token){.type = TIDENT, .name = strdup("len")};
	field_decl = makefielddecl(ctx->parser, tok_len, te_int);
	check_decl(ctx, field_decl);
	assert(field_decl->decl);
	arrput(t->members, field_decl);

	struct token tok_buf =
	    (struct token){.type = TIDENT, .name = strdup("buf")};
	field_decl = makefielddecl(ctx->parser, tok_buf, te_intp);
	check_decl(ctx, field_decl);
	assert(field_decl->decl);
	arrput(t->members, field_decl);

	scope_close(ctx);

	return t;
}

// FIXME: remove 'tok'
static struct type *
makepointertype(struct type *target, struct token tok) {
	struct type *t = maketype(TYPE_POINTER, tok);
	t->base_type = target;
	t->size = 8;
	return t;
}

static char *typename(const struct type *type, char *buf, size_t buflen) {
	int wlen = 0;
	char *cur = NULL;
	size_t curlen = 0;

	switch (type->kind) {
	case TYPE_ATOM:
		strncpy(buf, type->tok.name, buflen - 1);
		buf[buflen - 1] = '\0';
		break;
	case TYPE_POINTER:
		wlen = snprintf(buf, buflen, "*");
		curlen = buflen - wlen;
		cur = buf + wlen;
		typename(type->base_type, cur, curlen);
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
static char *
type_expr_name(const struct ast_type_expr *type_expr, char *buf,
               size_t buflen) {
	int wlen = 0;
	char *cur = NULL;
	size_t curlen = 0;

	assert(type_expr);
	switch (type_expr->typekind) {
	case TYPE_ATOM:
		strncpy(buf, type_expr->tok.name, buflen - 1);
		buf[buflen - 1] = '\0';
		break;
	case TYPE_ARRAY:
		wlen = snprintf(buf, buflen, "[]");
		curlen = buflen - wlen;
		cur = buf + wlen;
		type_expr_name(&type_expr->base_type->type_expr, cur, curlen);
		break;
	case TYPE_SLICE:
		wlen = snprintf(buf, buflen, "[N]"); // TODO: output actual number
		curlen = buflen - wlen;
		cur = buf + wlen;
		type_expr_name(&type_expr->base_type->type_expr, cur, curlen);
		break;
	case TYPE_POINTER:
		wlen = snprintf(buf, buflen, "*");
		curlen = buflen - wlen;
		cur = buf + wlen;
		type_expr_name(&type_expr->base_type->type_expr, cur, curlen);
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
static int
error(struct context *ctx, struct src_loc loc, const char *fmt, ...) {
	struct error e;
	va_list args;

	e.loc = loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);
	if ((size_t)len >= sizeof(e.msg))
		fatal("%s(): vsnprintf error", __func__);

	arrput(ctx->errors, e);
	return 0; // false
}

int
do_errors(const struct error *errors) {
	for (long i = 0; i < arrlen(errors); i++) {
		struct error e = errors[i];
		fprintf(stderr, "%s:%d:%d: error: %s\n", e.loc.filename, e.loc.line,
		        e.loc.col, e.msg);
	}
	return arrlen(errors) == 0;
}

static struct scope *
makescope(void) {
	struct scope *s = calloc(sizeof(struct scope), 1);
	makemap(&s->map);
	return s;
}

static void
freescope(struct scope *s) {
	freemap(&s->map);
	free(s);
}

static void
setup_builtin_types(struct context *ctx) {
	// FIXME: free(ty_int), calloc() check
	// `ty_any` is a dummy type created for built-in functions like len() that
	// can take a class of types, but have no way to represent their type
	// because the langauge doesn't support generic types currently.
	ty_undef = calloc(1, sizeof(struct type));
	ty_undef->kind = TYPE_ATOM;
	// FIXME: ty_any shouldn't really have a name
	ty_undef->tok = (struct token){.type = TINT, .name = "any"};
	ty_undef->size = 4;
	push_type(ctx, ty_undef);
	ty_int = calloc(1, sizeof(struct type));
	ty_int->kind = TYPE_ATOM;
	ty_int->tok = (struct token){.type = TINT, .name = "int"};
	ty_int->size = 4;
	push_type(ctx, ty_int);
	ty_int64 = calloc(1, sizeof(struct type));
	ty_int64->kind = TYPE_ATOM;
	ty_int64->tok = (struct token){.type = TINT, .name = "int64"};
	ty_int64->size = 8;
	push_type(ctx, ty_int64);
	ty_string = calloc(1, sizeof(struct type));
	ty_string->kind = TYPE_ATOM;
	ty_string->tok = (struct token){.type = TSTRING_, .name = "string"};
	ty_string->size = 8; // FIXME
	push_type(ctx, ty_string);
}

static void
setup_builtin_funcs(struct context *ctx) {
	struct ast_node *n;

	// FIXME: because tok.name is free()ed in parser_cleanup, we need to
	// explicitly allocate a string, which is a little arbitrary
	char *str = strdup("len");
	struct token len_tok = {.type = TIDENT, .name = str};
	n = makefunc(ctx->parser, len_tok);
	if (!declare(ctx, n)) {
		return;
	}
	n->type = maketype(TYPE_FUNC, n->tok);
	n->type->return_type = ty_int64;
	arrput(n->type->params, ty_undef);

	str = strdup("alloc");
	struct token alloc_tok = {.type = TIDENT, .name = str};
	fn_alloc = makefunc(ctx->parser, alloc_tok);
	if (!declare(ctx, fn_alloc)) {
		return;
	}
	fn_alloc->type = maketype(TYPE_FUNC, fn_alloc->tok);
	fn_alloc->type->return_type =
	    makeslicetype(ctx, ty_undef, fn_alloc->tok /*arbitrary*/);
	arrput(fn_alloc->type->params, ty_int64);
}

// NOTE: This *has* to be called after parse(), as it copies over the error
// list from the parser.
void
context_init(struct context *ctx, struct parser *p) {
	memset(ctx, 0, sizeof(struct context));
	ctx->src = &p->l.src;
	ctx->parser = p;
	ctx->outfile = fopen("out.qbe", "w");
	if (!ctx->outfile) {
		fatal("fopen() failed");
	}
	ctx->scope = makescope();
	ctx->typescope = makescope();
	// copy over errors
	for (long i = 0; i < arrlen(p->errors); i++) {
		arrput(ctx->errors, p->errors[i]);
	}
	setup_builtin_types(ctx);
	setup_builtin_funcs(ctx);
}

void
context_free(struct context *ctx) {
	freescope(ctx->scope);
	arrfree(ctx->valstack.data);
	// FIXME: free errors[i].msg
	free(ctx->errors);
	fclose(ctx->outfile);
}

// Open a scope, creating a new one.
void
scope_open(struct context *ctx) {
	struct scope *new_scope = makescope();
	new_scope->outer = ctx->scope;
	ctx->scope = new_scope;
	// TODO: typescope
}

// Open a scope using an existing scope that is already constructed.
void
scope_open_with(struct context *ctx, struct scope *scope) {
	scope->outer = ctx->scope;
	ctx->scope = scope;
}

void
scope_close(struct context *ctx) {
	// struct scope *current = ctx->scope;
	ctx->scope = ctx->scope->outer;
	// TODO: freescope(current);
	// TODO: typescope
}

// Declare 'n' in the current scope.  'n' can be a variable or a function.
static struct ast_node *
declare(struct context *ctx, struct ast_node *n) {
	if (!mapput(&ctx->scope->map, n->tok.name, n)) {
		char buf[TOKLEN];
		tokenstr(ctx->src->text, n->tok, buf, sizeof(buf));
		// FIXME: should this be done in the caller?
		error(ctx, n->loc, "'%s' is already declared", buf);
		return NULL;
	}
	return n;
}

// Finds the declaration node that first declared the variable referenced by
// 'n'.
struct ast_node *
lookup(struct context *ctx, const struct ast_node *n) {
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
static struct type *
push_type(struct context *ctx, struct type *ty) {
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
static struct type *
push_type_global(struct context *ctx, struct type *ty) {
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
static struct type *
lookup_type(struct context *ctx, const char *name) {
	struct scope *s = ctx->typescope;
	while (s) {
		struct type *found = mapget(&s->map, name);
		if (found)
			return found;
		s = s->outer;
	}
	return NULL;
}

// Get the matching type object specified by the `type_expr`.
// This handles error report on its own.
static struct type *
resolve_type_expr(struct context *ctx, struct ast_type_expr *type_expr) {
	char buf[TOKLEN];
	struct type *type = NULL;

	// Type expressions are recursive themselves; make sure to recurse down
	// to reesolve all underlying types.
	//
	// Note that all derived types, e.g. arrays and pointers, are newly
	// constructed even if they are seen before in the source.
	if (type_expr->typekind == TYPE_ARRAY) {
		assert(type_expr->base_type);
		struct type *base_type =
		    resolve_type_expr(ctx, &type_expr->base_type->type_expr);
		if (!base_type) {
			return NULL;
		}
		return makearraytype(base_type, type_expr->base_type->tok);
	} else if (type_expr->typekind == TYPE_SLICE) {
		assert(type_expr->base_type);
		struct type *base_type =
		    resolve_type_expr(ctx, &type_expr->base_type->type_expr);
		if (!base_type) {
			return NULL;
		}
		return makeslicetype(ctx, base_type, type_expr->base_type->tok);
	} else if (type_expr->typekind == TYPE_POINTER) {
		assert(type_expr->base_type);
		struct type *base_type =
		    resolve_type_expr(ctx, &type_expr->base_type->type_expr);
		if (!base_type) {
			return NULL;
		}
		return makepointertype(base_type, type_expr->base_type->tok);
	}
	type_expr_name(type_expr, buf, sizeof(buf));
	type = lookup_type(ctx, buf);
	if (!type) {
		assert(type_expr->typekind == TYPE_ATOM &&
		       "derived type should have been handled before");
		error(ctx, type_expr->loc, "unknown type '%s'", buf);
		return NULL;
	}
	assert(type);
	return type;
}

// Check if type `from` can be assigned to type `to`.  Also does automatic type
// promotion on `from`, e.g. from int to int64.
static int
type_compatible(struct type *to, struct type **from) {
	if (to->kind == TYPE_ARRAY || to->kind == TYPE_SLICE ||
	    to->kind == TYPE_POINTER) {
		if (to->kind != (*from)->kind)
			return 0;
		return type_compatible(to->base_type, &(*from)->base_type);
	}
	if (to == ty_undef) {
		return 1;
	}
	if (to == ty_int64 && *from == ty_int) {
		*from = ty_int64;
		return 1;
	}
	return to == *from;
}

struct ast_node *
lookup_struct_field(struct type *parent_type, const char *field_name) {
	struct ast_node *field_match = NULL;

	for (long i = 0; i < arrlen(parent_type->members); i++) {
		struct ast_node *f = parent_type->members[i];
		if (strcmp(f->tok.name, field_name) == 0) {
			field_match = f;
			break;
		}
	}

	return field_match;
}

static int
check_expr(struct context *ctx, struct ast_node *n) {
	char buf[TOKLEN];
	struct ast_node *decl = NULL;

	assert(n);
	tokenstr(ctx->src->text, n->tok, buf, sizeof(buf));

	switch (n->kind) {
	case NLITERAL:
		// TODO: non-int literals
		// TODO: treat as ty_long for indices in subscript exprs
		n->type = ty_int;
		if (ctx->prefer_long) {
			n->type = ty_int64;
		}
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
			return 0;
		// a + b: if a's type is assignable to b's type, *or* the other way
		// around, a binary operator typechecks.
		if (!type_compatible(n->bin.lhs->type, &n->bin.rhs->type) &&
		    !type_compatible(n->bin.rhs->type, &n->bin.lhs->type)) {
			return error(ctx, n->tok.loc,
			             "incompatible types for binary operation");
		}
		n->type = n->bin.lhs->type;
		break;
	case NDEREFEXPR:
		check_expr(ctx, n->deref.target);
		if (!n->deref.target->type)
			return 0;
		// FIXME: This way every (*c) will generate a new decl.
		// Declare itself.  Don't put this in the symbol map as we don't need
		// these temporary decls to be referrable by any specific name.
		n->decl = n;
		if (n->deref.target->type->kind != TYPE_POINTER)
			return error(ctx, n->loc, "cannot dereference a non-pointer");
		n->type = n->deref.target->type->base_type;
		break;
	case NREFEXPR:
		check_expr(ctx, n->ref.target);
		if (!n->ref.target->type)
			return 0;
		// lvalue check
		if (!n->ref.target->decl)
			return error(ctx, n->loc, "cannot take reference of a non-lvalue");
		n->type = makepointertype(n->ref.target->type, n->tok);
		break;
	case NSUBSCRIPT:
		check_expr(ctx, n->subscript.array);
		if (!n->subscript.array->type)
			return 0;
		ctx->prefer_long = 1;
		check_expr(ctx, n->subscript.index);
		ctx->prefer_long = 0;
		if (!n->subscript.index->type)
			return 0;
		type_compatible(ty_int64, &n->subscript.index->type);
		// if (n->subscript.index->type == ty_int) {
		// 	// Promote to int64 so that it can be used for address calculation
		// 	// without a hassle.
		// 	n->subscript.index->type = ty_int64;
		// }
		assert(n->subscript.array->decl);
		n->decl = maketempdecl(ctx->parser);
		n->type = n->subscript.array->type->base_type;
		break;
	case NCALL:
		// callee name is a node (n->call.func), not a token!
		check_expr(ctx, n->call.func);
		if (!n->call.func->type) {
			return 0 ;
		}
		if (n->call.func->type->kind != TYPE_FUNC) {
			tokenstr(ctx->src->text, n->call.func->tok, buf, sizeof(buf));
			return error(ctx, n->call.func->loc, "'%s' is not a function", buf);
		}
		// FIXME: can't use arrlen on args
		if (arrlen(n->call.func->type->params) != nodelistlen(n->call.args)) {
			return error(ctx, n->call.func->loc,
			             "wrong number of arguments: expected %ld, got %ld",
			             arrlen(n->call.func->type->params),
			             nodelistlen(n->call.args));
		}
		int i = 0;
		for (struct ast_node *arg = n->call.args; arg; arg = arg->next, i++) {
			check_expr(ctx, arg);
			if (!arg->type)
				break;
			assert(n->call.func->type->params[i]);
			if (!type_compatible(n->call.func->type->params[i], &arg->type)) {
				char expect_buf[TOKLEN];
				char got_buf[TOKLEN];
				typename(n->call.func->type->params[i], expect_buf,
				         sizeof(expect_buf));
				typename(arg->type, got_buf, sizeof(got_buf));
				return error(ctx, arg->loc,
				             "wrong type of argument: expected %s, got %s",
				             expect_buf, got_buf);
			}
		}
		n->type = n->call.func->type->return_type;

		// TODO: turn this into check_builtin_func()
		if (strcmp(n->call.func->tok.name, "len") == 0) {
			assert(nodelistlen(n->call.args) == 1);
			struct ast_node *firstarg = n->call.args;
			// first arg
			if (firstarg->type->kind != TYPE_ARRAY &&
			    firstarg->type->kind != TYPE_SLICE) {
				return error(ctx, firstarg->loc,
				             "len() can be used only for arrays and slices");
			}
			// FIXME: dirty in-place rewrite
			n->kind = NMEMBER;
			n->tok = (struct token){.type = TIDENT, .name = strdup("len")};
			n->member.parent = firstarg;
			if (!check_expr(ctx, n))
				assert(!"cannot fail");
		}
		break;
	case NMEMBER: {
		check_expr(ctx, n->member.parent);
		if (!n->member.parent->type) {
			return 0;
		}
		if (!arrlen(n->member.parent->type->members)) {
			return error(ctx, n->loc, "member access to a non-struct");
		}
		struct ast_node *field_match =
		    lookup_struct_field(n->member.parent->type, n->tok.name);
		if (!field_match) {
			return error(ctx, n->loc, "'%s' is not a member of type '%s'",
			             n->tok.name, n->member.parent->type->tok.name);
		}
		assert(n->member.parent->decl);
		n->decl = maketempdecl(ctx->parser);
		n->type = field_match->type;
		n->member.offset = field_match->field.offset;
		break;
	}
	default:
		printf("n->kind=%d\n", n->kind);
		assert(!"unknown expr kind");
	}

	assert(n->type);
	return 1;
}

static int
check_assign(struct context *ctx, struct ast_node *to, struct ast_node *from) {
	assert(to->type && from->type);

	// TODO: maketempdecl() currently simply creates an empty decl with no name
	// or location info.  This is no different than using a simple boolean flag
	// to mark the expression as lvalue.  It would be nice to keep more metadata
	// in the temporary decl for better diagnostics.
	if (!to->decl) {
		error(ctx, to->loc, "cannot assign to a non-lvalue");
		return 0;
	}

	if (from->kind == NCALL &&
	    strcmp(from->call.func->tok.name, "alloc") == 0) {
		if (to->type->kind != TYPE_SLICE) {
			error(ctx, to->loc, "alloc() can be only used for slices");
			return 0;
		}
		from->type = to->type;
	}
	if (!type_compatible(to->type, &from->type)) {
		error(ctx, to->loc, "cannot assign to an incompatible type");
		return 0;
	}

	return 1;
}

static struct token
tokdup(const struct token tok) {
	struct token t = tok;
	t.name = strdup(tok.name);
	return t;
}

static void
check_decl(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NVARDECL:
		// var decl has an init expression, ex. var i = 4
		if (n->var_decl.init_expr) {
			check_expr(ctx, n->var_decl.init_expr);
			if (!n->var_decl.init_expr->type)
				return;
			n->type = n->var_decl.init_expr->type;
		}
		// var decl has a type specifier, ex. var i: int
		if (n->var_decl.type_expr) {
			n->type = resolve_type_expr(ctx, &n->var_decl.type_expr->type_expr);
			if (!n->type)
				return;
		}
		// FIXME: This is a little awkward because original declarations would
		// have 'n == n->decl'.  Or is this a good thing?  Maybe make a
		// separate Decl struct?
		if (!(n->decl = declare(ctx, n)))
			return;

		if (n->var_decl.init_expr) {
			// rewrite into vardecl + assign expr
			// This way, there's no need to explicitly call check_assign()
			// inside NVARDECL because that will be handled when the newly added
			// assignment node is visited.
			struct ast_node *id = makenode(ctx->parser, NIDEXPR, n->tok.loc);
			id->tok = tokdup(n->tok);
			check_expr(ctx, id);
			assert(id->type); // should succeed as `id` is already declared
			struct ast_node *assign =
			    makeassign(ctx->parser, id, n->var_decl.init_expr);
			addnode(&n, assign);
		}
		assert(n->type);
		break;
	case NFUNC: {
		if (!declare(ctx, n)) {
			return;
		}
		n->type = maketype(TYPE_FUNC, n->tok);
		// !n->rettypeexpr is possible for void return type
		if (n->func.ret_type_expr) {
			n->type->return_type =
			    resolve_type_expr(ctx, &n->func.ret_type_expr->type_expr);
			if (!n->type->return_type) {
				error(ctx, n->func.ret_type_expr->loc,
				             "unknown type '%s'",
				             n->func.ret_type_expr->tok.name);
				return;
			}
		}

		scope_open(ctx);
		n->scope = ctx->scope;
		ctx->scope->decl = n;
		// declare parameters
		struct ast_node *p = n->func.params;
		while (p) {
			assert(p->kind == NVARDECL);
			check(ctx, p);
			if (p->type) {
				arrput(n->type->params, p->type);
			} else {
				assert(!"FIXME: what now?");
			}
			p = p->next;
		}
		// check body
		struct ast_node *s = n->func.stmts;
		while (s) {
			check(ctx, s);
			s = s->next;
		}
		scope_close(ctx);

		// TODO: check return stmts
		break;
	}
	case NSTRUCT: {
		n->type = maketype(TYPE_ATOM, n->tok);
		push_type(ctx, n->type);

		// Instantiate struct fields.
		//
		scope_open(ctx);
		// Clear the field offset accumulation at struct entry.
		// This shouldn't be done for inner structs (TODO).
		ctx->accum_field_offset = 0;
		n->type->size = 0;
		struct ast_node *f = n->struct_.fields;
		while (f) {
			check_decl(ctx, f);
			assert(f->decl);
			arrput(n->type->members, f);
			// FIXME: respect alignment when calculating byte size
			n->type->size += f->decl->type->size;
			f = f->next;
		}
		scope_close(ctx);

		if (!(n->decl = declare(ctx, n)))
			return;
		assert(n->type);
		break;
	}
	case NFIELD:
		assert(n->field.type_expr);
		if (!(n->type = resolve_type_expr(ctx, &n->field.type_expr->type_expr)))
			return;
		if (!(n->decl = declare(ctx, n)))
			return;
		n->field.offset = ctx->accum_field_offset;
		ctx->accum_field_offset += 4; // FIXME
		break;
	default:
		printf("n->kind=%d\n", n->kind);
		assert(!"TODO");
	}
}

static void
check_stmt(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NEXPRSTMT:
		check_expr(ctx, n->expr_stmt.expr);
		break;
	case NASSIGN: {
		struct ast_assign_expr *assign = &n->assign_expr;
		check_expr(ctx, assign->rhs);
		check_expr(ctx, assign->lhs);
		if (!assign->lhs->type || !assign->rhs->type)
			return;
		if (!check_assign(ctx, assign->lhs, assign->rhs))
			return;

		if (assign->lhs->type->kind == TYPE_SLICE) {
			if (assign->rhs->kind == NCALL) {
				// Array usages:
				//	 arr = alloc()
				// 	 arr = arr2 (tentative)
				// 	 *alloc()
				struct ast_node *memb = makemember(
				    ctx->parser,
				    (struct token){.type = TIDENT, .name = strdup("buf")}
				    /* TODO: make as singleton */
				    ,
				    assign->lhs);
				check_expr(ctx, memb);
				assert(memb->type);
				assert(memb->decl);

				assign->lhs = memb;
			}
		}
		break;
	}
	case NBLOCKSTMT: {
		scope_open(ctx);
		struct ast_node *s = n->block.stmts;
		while (s) {
			check(ctx, s);
			s = s->next;
		}
		scope_close(ctx);
		break;
	}
	case NRETURN:
		check_expr(ctx, n->return_expr.expr);
		if (!n->return_expr.expr->type)
			return;
		break;
	default:
		assert(!"unknown stmt kind");
	}
}

void
check(struct context *ctx, struct ast_node *n) {
	switch (n->kind) {
	case NFILE: {
		struct ast_node *b = n->file.body;
		while (b) {
			check(ctx, b);
			b = b->next;
		}
		break;
	}
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
