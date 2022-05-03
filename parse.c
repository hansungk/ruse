#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct node *parse_expr(struct parser *p);
static struct node *parse_stmt(struct parser *p);
static struct ast_type_expr *parse_type_expr(struct parser *p);

static struct node *makenode(struct parser *p, enum node_kind k,
                             struct src_loc loc) {
	// TODO: store all nodes in a contiguous buffer for better locality?
	// should be careful about node pointers going stale though
	struct node *node = calloc(1, sizeof(struct node));
	if (!node) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	node->kind = k;
	node->loc = loc;
	arrput(p->nodeptrbuf, node);
	return node;
}

static struct node *makefile(struct parser *p, struct node **toplevel) {
	struct node *n = makenode(p, NFILE, p->tok.loc);
	n->file.body = toplevel;
	return n;
}

static struct node *makefunc(struct parser *p, struct token name) {
	struct node *n = makenode(p, NFUNC, name.loc);
	n->tok = name;
	return n;
}

static struct node *makestruct(struct parser *p, struct token name) {
	struct node *n = makenode(p, NSTRUCT, name.loc);
	n->tok = name;
	return n;
}

static struct node *makebinexpr(struct parser *p, struct node *lhs,
                                struct token op, struct node *rhs) {
	struct node *n = makenode(p, NBINEXPR, op.loc);
	n->bin.lhs = lhs;
	n->bin.rhs = rhs;
	n->tok = op;
	return n;
}

static struct node *makederefexpr(struct parser *p, struct node *target,
                                  struct token star) {
	struct node *n = makenode(p, NDEREFEXPR, star.loc);
	n->deref.target = target;
	return n;
}

static struct node *makerefexpr(struct parser *p, struct node *target,
                                struct token amp) {
	struct node *n = makenode(p, NREFEXPR, amp.loc);
	n->ref.target = target;
	return n;
}

static struct node *makecall(struct parser *p, struct node *func,
                             struct node **args) {
	struct node *n = makenode(p, NCALL, func->loc);
	n->call.func = func;
	assert(!n->call.args);
	n->call.args = args;
	return n;
}

// 'lhs' is the 'a.b()' part of 'a.b().c'.
static struct node *makemember(struct parser *p, struct token member,
                               struct node *lhs) {
	struct node *n = makenode(p, NMEMBER, member.loc);
	n->tok = member;
	n->parent = lhs;
	return n;
}

static struct ast_type_expr *maketypeexpr(enum type_kind kind,
                                          struct token tok) {
	// TODO proper arena allocation
	struct ast_type_expr *texpr = calloc(1, sizeof(struct ast_type_expr));
	if (!texpr) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	texpr->loc = tok.loc;
	texpr->tok = tok;
	texpr->typekind = kind;
	return texpr;
}

static struct node *makevardecl(struct parser *p, struct token name,
                                struct node *init_expr,
                                struct ast_type_expr *type_expr) {
	struct node *n = makenode(p, NVARDECL, name.loc);
	n->tok = name;
	n->var_decl.init_expr = init_expr;
	n->type_expr = type_expr;
	return n;
}

static struct node *makereturn(struct parser *p, struct node *rhs) {
	struct node *n = makenode(p, NRETURN, rhs->loc);
	n->return_expr.expr = rhs;
	return n;
}

static struct node *makeexprstmt(struct parser *p, struct node *rhs) {
	struct node *n = makenode(p, NEXPRSTMT, rhs->loc);
	n->expr_stmt.expr = rhs;
	return n;
}

static struct node *makeassign(struct parser *p, struct node *lhs,
                               struct node *rhs) {
	struct node *n = makenode(p, NASSIGN, lhs->loc);
	n->assign_expr.lhs = lhs;
	n->assign_expr.init_expr = rhs;
	return n;
}

// Initialize a parser that parses the given filename.
void parser_from_file(struct parser *p, const char *filename) {
	memset(p, 0, sizeof(struct parser));
	lexer_from_file(&p->l, filename);
	// start by putting something on p->tok
	// can't use next() because p->lexer.tok is initialized to TEOF
	lex(&p->l);
	p->tok = p->l.tok;
}

void parser_from_buf(struct parser *p, const char *buf, size_t len) {
	memset(p, 0, sizeof(struct parser));
	lexer_from_buf(&p->l, buf, len);
	lex(&p->l);
	p->tok = p->l.tok;
}

void parser_cleanup(struct parser *p) {
	for (int i = 0; i < arrlen(p->nodeptrbuf); i++) {
		struct node *n = p->nodeptrbuf[i];
		if (n) {
			// TODO: free arrays in the unions
			free(n->tok.name);
			free(n);
		}
	}

	lexer_cleanup(&p->l);
	arrfree(p->nodeptrbuf);
}

static void next(struct parser *p) {
	if (p->l.tok.type == TEOF)
		return;
	lex(&p->l);
	p->tok = p->l.tok;
}

static void error(struct parser *p, const char *fmt, ...) {
	struct error e;
	va_list args;

	e.loc = p->tok.loc;
	va_start(args, fmt);
	int len = vsnprintf(e.msg, sizeof(e.msg), fmt, args);
	va_end(args);

	if (len < 0 || (size_t)len >= sizeof(e.msg)) {
		fatal("%s(): vsnprintf error", __func__);
	}

	arrput(p->errors, e);
}

static void skip_while(struct parser *p, enum token_type type) {
	while (p->tok.type != TEOF && p->tok.type == type) {
		next(p);
	}
}

static void skip_newlines(struct parser *p) {
	while (p->tok.type == '\n' || p->tok.type == TCOMMENT) {
		skip_while(p, '\n');
		skip_while(p, TCOMMENT);
	}
}

static void skip_to(struct parser *p, enum token_type type) {
	while (p->tok.type != TEOF && p->tok.type != type) {
		next(p);
	}
}

static void skip_to_end_of_line(struct parser *p) {
	while (p->tok.type != TEOF && p->tok.type != TNEWLINE) {
		next(p);
	}
}

static int expect(struct parser *p, enum token_type t) {
	if (p->tok.type != t) {
		char ebuf[TOKLEN], gbuf[TOKLEN];
		tokentypestr(t, ebuf, sizeof(ebuf));
		tokenstr(p->l.src.buf, p->tok, gbuf, sizeof(gbuf));
		if (strcmp(gbuf, "\n") == 0)
			strcpy(gbuf, "\\n");
		error(p, "expected '%s', got '%s'", ebuf, gbuf);
		return 0;
	}
	// make progress
	next(p);
	return 1;
}

static int end_of_line(struct parser *p) {
	return p->tok.type == TNEWLINE || p->tok.type == TCOMMENT;
}

// Expect end of line, skipping over any end-of-line comments.
static int expect_end_of_line(struct parser *p) {
	skip_while(p, TCOMMENT);
	return expect(p, TNEWLINE);
}

// TODO: remove
static void tokentypeprint(enum token_type t) {
	char buf[TOKLEN];
	tokentypestr(t, buf, sizeof(buf));
	printf("i saw %s\n", buf);
}

static struct node *parse_vardecl(struct parser *p) {
	assert(p->tok.type == TVAR || p->tok.type == TCONST);
	next(p);

	struct token name = p->tok;
	next(p);

	struct ast_type_expr *texpr = NULL;
	if (p->tok.type != TEQUAL) {
		texpr = parse_type_expr(p);
	}

	struct node *rhs = NULL;
	if (!end_of_line(p)) {
		expect(p, TEQUAL);
		rhs = parse_expr(p);
	}

	return makevardecl(p, name, rhs, texpr);
}

static struct node *parse_blockstmt(struct parser *p) {
	struct node *n = makenode(p, NBLOCKSTMT, p->tok.loc);

	expect(p, TLBRACE);
	while (p->tok.type != TRBRACE) {
		struct node *s = parse_stmt(p);
		arrput(n->block.stmts, s);
		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return n;
}

static struct node *parse_return(struct parser *p) {
	expect(p, TRETURN);
	struct node *rhs = parse_expr(p);
	return makereturn(p, rhs);
}

// Assumes enclosing '(' is already consumed.
// Does not consume ending ')'.
static struct node **parse_callargs(struct parser *p) {
	struct node **list = NULL;
	while (p->tok.type != TRPAREN) {
		arrput(list, parse_expr(p));
		if (p->tok.type == TCOMMA)
			next(p);
		else
			break;
	}
	return list;
}

static struct node *parse_unaryexpr(struct parser *p) {
	struct node *e = NULL;
	struct node *rhs;
	struct token tok;

	switch (p->tok.type) {
	case TIDENT:
		tok = p->tok;
		next(p);
		e = makenode(p, NIDEXPR, tok.loc);
		e->tok = tok; // FIXME: too bare
		break;
	case TNUM:
		tok = p->tok;
		next(p);
		e = makenode(p, NLITERAL, tok.loc);
		e->tok = tok; // FIXME: too bare
		break;
	case TLPAREN:
		expect(p, TLPAREN);
		e = parse_expr(p);
		expect(p, TRPAREN);
		break;
	case TSTAR:
		tok = p->tok;
		next(p);
		rhs = parse_unaryexpr(p);
		e = makederefexpr(p, rhs, tok);
		break;
		break;
	case TAMPERSAND:
		tok = p->tok;
		next(p);
		rhs = parse_unaryexpr(p);
		e = makerefexpr(p, rhs, tok);
		break;
	default:
		error(p, "expected an expression");
		break;
	}

	// now try to parse any trailing . or ()
	while (p->tok.type == TDOT || p->tok.type == TLPAREN) {
		if (p->tok.type == TDOT) {
			next(p);
			// swap parent with child
			e = makemember(p, p->tok, e);
			expect(p, TIDENT);
		} else {
			expect(p, TLPAREN);
			// swap parent with child
			struct node **args = parse_callargs(p);
			e = makecall(p, e, args);
			expect(p, TRPAREN);
		}
	}

	return e;
}

static int get_precedence(const struct token op) {
	switch (op.type) {
	case TSTAR:
	case TSLASH:
		return 1;
	case TPLUS:
	case TMINUS:
		return 0;
	default:
		return -1; // not an operator
	}
}

// Parse (op binary)* part of the production.
//
// BinaryExpr:
//     UnaryExpr (op BinaryExpr)*
//
// Return the pointer to the node respresenting the reduced binary expression.
// If this is not a binary expression, just return 'lhs' as-is.
static struct node *parse_binexpr_rhs(struct parser *p, struct node *lhs,
                                      int precedence) {
	while (1) {
		int this_prec = get_precedence(p->tok);

		// If the upcoming op has lower precedence, the subexpression of the
		// precedence level that we are currently parsing in is finished. This
		// is equivalent to reducing on a shift/reduce conflict in bottom-up
		// parsing.
		if (this_prec < precedence)
			break;

		struct token op = p->tok;
		next(p);

		// Parse the next term.  We do not know yet if this term should bind to
		// LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know this, we
		// should look ahead for the operator that follows this term.
		struct node *rhs = parse_unaryexpr(p);
		if (!rhs)
			error(p, "expected an expression");
		int next_prec = get_precedence(p->tok);

		// If the next operator is indeed higher-level, evaluate the RHS as a
		// whole subexpression with elevated minimum precedence. Else, just
		// treat it as a unary expression.  This is equivalent to shifting on a
		// shift/reduce conflict in bottom-up parsing.
		//
		// If this_prec == next_prec, don't shift, but reduce it with lhs. This
		// implies left associativity.
		if (this_prec < next_prec)
			rhs = parse_binexpr_rhs(p, rhs, precedence + 1);
		lhs = makebinexpr(p, lhs, op, rhs);
	}
	return lhs;
}

static struct node *parse_expr(struct parser *p) {
	struct node *e = parse_unaryexpr(p);
	e = parse_binexpr_rhs(p, e, 0);
	return e;
}

// Possibly parse the equal and RHS expression of an expression.
// 'expr' is already consumed.
static struct node *parse_assign_or_expr_stmt(struct parser *p,
                                              struct node *expr) {
	struct node *stmt = NULL;

	if (p->tok.type == TEQUAL) {
		next(p);
		struct node *rhs = parse_expr(p);
		stmt = makeassign(p, expr, rhs);
	} else {
		stmt = makeexprstmt(p, expr);
	}
	expect_end_of_line(p);
	return stmt;
}

static struct node *parse_stmt(struct parser *p) {
	struct node *stmt;

	skip_newlines(p);

	switch (p->tok.type) {
	case TEOF:
		return NULL;
	case TCOMMENT:
		expect_end_of_line(p);
		return parse_stmt(p);
	case TVAR:
	case TCONST:
		return parse_vardecl(p);
	case TLBRACE:
		return parse_blockstmt(p);
	case TRETURN:
		return parse_return(p);
	default:
		break;
	}

	// if (is_decl_start(p)) {
	// 	node *decl = parse_decl(p);
	// 	stmt = makeDecl_stmt(p, decl);
	// 	expectEndOfLine(p);
	// 	return stmt;
	// }

	// skip_to_end_of_line(p);
	// expect(p, TNEWLINE);

	// all productions from now on start with an expression
	// TODO: exprstmt?
	stmt = parse_expr(p);
	stmt = parse_assign_or_expr_stmt(p, stmt);
	return stmt;
}

static struct ast_type_expr *parse_type_expr(struct parser *p) {
	struct token tok = p->tok;

	if (p->tok.type == TINT) {
		expect(p, TINT);
		return maketypeexpr(TYPE_VAL, tok);
	} else if (p->tok.type == TIDENT) {
		expect(p, TIDENT);
		return maketypeexpr(TYPE_VAL, tok);
	} else if (p->tok.type == TSTAR) {
		expect(p, TSTAR);
		struct ast_type_expr *texpr = maketypeexpr(TYPE_POINTER, tok);
		texpr->pointee = parse_type_expr(p);
		return texpr;
	} else {
		assert(!"unimplemented");
	}
}

static struct node *parse_func(struct parser *p) {
	expect(p, TFUNC);

	struct node *f = makefunc(p, p->tok);
	next(p);

	// argument list
	// NOTE: similar code to parse_struct()
	expect(p, TLPAREN);
	while (p->tok.type != TRPAREN) {
		struct token tok = p->tok;
		expect(p, TIDENT);
		struct ast_type_expr *texpr = parse_type_expr(p);
		arrput(f->func.params, makevardecl(p, tok, NULL, texpr));
		if (p->tok.type != TRPAREN) {
			if (!expect(p, TCOMMA))
				// recover error
				skip_to(p, TRPAREN);
		}
	}
	expect(p, TRPAREN);

	// return type
	if (p->tok.type != TLBRACE) {
		f->func.ret_type_expr = parse_type_expr(p);
	}

	// body
	expect(p, TLBRACE);
	skip_newlines(p);
	while (p->tok.type != TRBRACE) {
		struct node *stmt = parse_stmt(p);
		if (stmt)
			arrput(f->func.stmts, stmt);

		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return f;
}

static struct node *parse_struct(struct parser *p) {
	expect(p, TSTRUCT);

	struct node *s = makestruct(p, p->tok);
	next(p);

	expect(p, TLBRACE);
	skip_newlines(p);
	// NOTE: similar code to parse_callargs()
	while (p->tok.type != TRBRACE) {
		struct token tok = p->tok;
		expect(p, TIDENT);
		struct ast_type_expr *texpr = parse_type_expr(p);
		struct node *field = makevardecl(p, tok, NULL, texpr);
		arrput(s->struct_.fields, field);
		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return s;
}

static struct node *parse_toplevel(struct parser *p) {
	skip_newlines(p);

	switch (p->tok.type) {
	case TFUNC:
		return parse_func(p);
	case TSTRUCT:
		return parse_struct(p);
	default:
		error(p, "unknown token type %d at toplevel", p->tok.type);
		return NULL;
	}
}

struct node *parse(struct parser *p) {
	struct node **nodes = NULL;

	while (p->tok.type != TEOF) {
		struct node *func = parse_toplevel(p);
		arrput(nodes, func);
		skip_newlines(p);
	}

	return makefile(p, nodes);
}
