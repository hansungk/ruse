#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct node *parse_expr(struct Parser *p);
static struct node *parse_stmt(struct Parser *p);
static struct type *parse_type(struct Parser *p);

static struct node *makenode(struct Parser *p, enum NodeKind k, Token tok) {
	// TODO: store all nodes in a contiguous buffer for better locality?
	// should be careful about node pointers going stale though
	struct node *node = calloc(1, sizeof(struct node));
	if (!node) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	node->kind = k;
	node->tok = tok;
	arrput(p->nodeptrbuf, node);
	return node;
}

static struct node *makefile(struct Parser *p, struct node **toplevel) {
	struct node *n = makenode(p, NFILE, p->tok);
	n->children = toplevel;
	return n;
}

static struct node *makefunc(struct Parser *p, Token name) {
	struct node *n = makenode(p, NFUNC, name);
	return n;
}

static struct node *makestruct(struct Parser *p, Token name,
                               struct type *type) {
	struct node *n = makenode(p, NSTRUCT, name);
	n->type = type;
	return n;
}

static struct node *makebinexpr(struct Parser *p, struct node *lhs, Token op,
                                struct node *rhs) {
	struct node *n = makenode(p, NBINEXPR, op);
	n->lhs = lhs;
	n->rhs = rhs;
	return n;
}

static struct node *makecall(struct Parser *p, struct node *lhs,
                             struct node **args) {
	struct node *n = makenode(p, NCALL, p->tok /*unused*/);
	n->lhs = lhs;
	assert(!n->children);
	n->children = args;
	return n;
}

// 'lhs' is the 'a.b()' part of 'a.b().c'.
static struct node *makemember(struct Parser *p, Token member,
                               struct node *lhs) {
	struct node *n = makenode(p, NMEMBER, member);
	n->parent = lhs;
	return n;
}

static struct node *makedecl(struct Parser *p, Token name,
                             struct node *initexpr, struct type *type) {
	struct node *n = makenode(p, NVAR, name);
	n->rhs = initexpr;
	n->type = type;
	return n;
}

static struct node *makereturn(struct Parser *p, struct node *rhs) {
	struct node *n = makenode(p, NRETURN, p->tok);
	n->rhs = rhs;
	return n;
}

static struct node *makeexprstmt(struct Parser *p, struct node *rhs) {
	struct node *n = makenode(p, NEXPRSTMT, p->tok);
	n->rhs = rhs;
	return n;
}

static struct node *makeassign(struct Parser *p, struct node *lhs,
                               struct node *rhs) {
	struct node *n = makenode(p, NASSIGN, p->tok);
	n->lhs = lhs;
	n->rhs = rhs;
	return n;
}

static struct type *maketype(struct Parser *p, Token tok) {
	struct type *t = calloc(1, sizeof(struct node));
	if (!t) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	t->tok = tok;
	// arrput(p->nodeptrbuf, node); // FIXME
	return t;
}

// Initialize a parser that parses the given filename.
void parser_from_file(struct Parser *p, const char *filename) {
	memset(p, 0, sizeof(Parser));
	lexer_from_file(&p->l, filename);
	// start by putting something on p->tok
	// can't use next() because p->lexer.tok is initialized to TEOF
	lex(&p->l);
	p->tok = p->l.tok;
}

void parser_from_buf(struct Parser *p, const char *buf, size_t len) {
	memset(p, 0, sizeof(Parser));
	lexer_from_buf(&p->l, buf, len);
	lex(&p->l);
	p->tok = p->l.tok;
}

void parser_cleanup(struct Parser *p) {
	for (int i = 0; i < arrlen(p->nodeptrbuf); i++) {
		struct node *n = p->nodeptrbuf[i];
		if (n) {
			if (n->children)
				arrfree(n->children);
			free(n->tok.name);
			free(n);
		}
	}

	lexer_cleanup(&p->l);
	arrfree(p->nodeptrbuf);
}

static void next(struct Parser *p) {
	if (p->l.tok.type == TEOF)
		return;
	lex(&p->l);
	p->tok = p->l.tok;
}

static void error(struct Parser *p, const char *fmt, ...) {
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	fprintf(stderr, "%s:%d:%d: error: %s\n", p->tok.loc.filename, p->tok.loc.line,
	        p->tok.loc.col, msg);
	exit(EXIT_FAILURE);
}

static void skip_while(struct Parser *p, enum TokenType type) {
	while (p->tok.type != TEOF && p->tok.type == type) {
		next(p);
	}
}

static void skip_newlines(struct Parser *p) {
	while (p->tok.type == '\n' || p->tok.type == TCOMMENT) {
		skip_while(p, '\n');
		skip_while(p, TCOMMENT);
	}
}

static void skip_to_end_of_line(struct Parser *p) {
	while (p->tok.type != TEOF && p->tok.type != TNEWLINE) {
		next(p);
	}
}

static int expect(struct Parser *p, enum TokenType t) {
	if (p->tok.type != t) {
		char ebuf[TOKLEN], gbuf[TOKLEN];
		tokentypestr(t, ebuf, sizeof(ebuf));
		tokenstr(p->l.src.buf, p->tok, gbuf, sizeof(gbuf));
		if (strcmp(gbuf, "\n") == 0)
			strcpy(gbuf, "\\n");
		error(p, "expected '%s', got '%s'", ebuf, gbuf);
	}
	// make progress
	next(p);
	return 1;
}

static int end_of_line(struct Parser *p) {
	return p->tok.type == TNEWLINE || p->tok.type == TCOMMENT;
}

// Expect end of line, skipping over any end-of-line comments.
static int expect_end_of_line(struct Parser *p) {
	skip_while(p, TCOMMENT);
	return expect(p, TNEWLINE);
}

// TODO: remove
static void tokentypeprint(enum TokenType t) {
	char buf[TOKLEN];
	tokentypestr(t, buf, sizeof(buf));
	printf("i saw %s\n", buf);
}

static struct node *parse_decl(struct Parser *p) {
	assert(p->tok.type == TVAR || p->tok.type == TCONST);
	next(p);

	Token name = p->tok;
	next(p);

	struct type *ty = NULL;
	if (p->tok.type != TEQUAL) {
		ty = parse_type(p);
	}

	struct node *rhs = NULL;
	if (!end_of_line(p)) {
		expect(p, TEQUAL);
		rhs = parse_expr(p);
	}

	return makedecl(p, name, rhs, ty);
}

static struct node *parse_blockstmt(struct Parser *p) {
	struct node *n = makenode(p, NBLOCKSTMT, p->tok);

	expect(p, TLBRACE);
	while (p->tok.type != TRBRACE) {
		struct node *s = parse_stmt(p);
		arrput(n->children, s);
		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return n;
}

static struct node *parse_return(struct Parser *p) {
	expect(p, TRETURN);
	struct node *rhs = parse_expr(p);
	return makereturn(p, rhs);
}

// Assumes enclosing '(' is already consumed.
// Does not consume ending ')'.
static struct node **parse_callargs(struct Parser *p) {
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

static struct node *parse_unaryexpr(struct Parser *p) {
	struct node *e = NULL;
	Token tok;

	switch (p->tok.type) {
	case TIDENT:
		tok = p->tok;
		next(p);
		e = makenode(p, NIDEXPR, tok);
		break;
	case TNUM:
		tok = p->tok;
		next(p);
		e = makenode(p, NLITERAL, tok);
		break;
	case TLPAREN:
		expect(p, TLPAREN);
		e = parse_expr(p);
		expect(p, TRPAREN);
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
			next(p);
			// swap parent with child
			struct node **args = parse_callargs(p);
			e = makecall(p, e, args);
			expect(p, TRPAREN);
		}
	}

	return e;
}

static int get_precedence(const Token op) {
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
//	 UnaryExpr (op BinaryExpr)*
//
// Return the pointer to the node respresenting the reduced binary expression.
// If this is not a binary expression, just return 'lhs' as-is.
static struct node *parse_binexpr_rhs(struct Parser *p, struct node *lhs,
                                      int precedence) {
	while (1) {
		int this_prec = get_precedence(p->tok);

		// If the upcoming op has lower precedence, the subexpression of
		// the precedence level that we are currently parsing in is
		// finished. This is equivalent to reducing on a shift/reduce
		// conflict in bottom-up parsing.
		if (this_prec < precedence)
			break;

		Token op = p->tok;
		next(p);

		// Parse the next term.  We do not know yet if this term should
		// bind to LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know
		// this, we should look ahead for the operator that follows this
		// term.
		struct node *rhs = parse_unaryexpr(p);
		if (!rhs)
			error(p, "expected an expression");
		int next_prec = get_precedence(p->tok);

		// If the next operator is indeed higher-level, evaluate the RHS
		// as a whole subexpression with elevated minimum precedence.
		// Else, just treat it as a unary expression.  This is
		// equivalent to shifting on a shift/reduce conflict in
		// bottom-up parsing.
		//
		// If this_prec == next_prec, don't shift, but reduce it with
		// lhs. This implies left associativity.
		if (this_prec < next_prec)
			rhs = parse_binexpr_rhs(p, rhs, precedence + 1);
		lhs = makebinexpr(p, lhs, op, rhs);
	}
	return lhs;
}

static struct node *parse_expr(struct Parser *p) {
	struct node *e = parse_unaryexpr(p);
	e = parse_binexpr_rhs(p, e, 0);
	return e;
}

// Possibly parse the equal and RHS expression of an expression.
// 'expr' is already consumed.
static struct node *parse_assign_or_expr_stmt(struct Parser *p, struct node *expr) {
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

static struct node *parse_stmt(struct Parser *p) {
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
		return parse_decl(p);
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

static struct type *parse_type(struct Parser *p) {
	if (p->tok.type == TINT) {
		expect(p, TINT);
	} else if (p->tok.type == TIDENT) {
		expect(p, TIDENT);
	} else {
		error(p, "expected a type (TODO)");
	}
	return maketype(p, p->tok);
}

static struct node *parse_func(struct Parser *p) {
	expect(p, TFUNC);

	struct node *f = makefunc(p, p->tok);
	next(p);

	// argument list
	expect(p, TLPAREN);
	// func->paramdecls = parse_paramdecllist(p);
	expect(p, TRPAREN);

	// return type
	if (p->tok.type != TLBRACE)
		f->rettype = parse_type(p);

	// body
	expect(p, TLBRACE);
	while (p->tok.type != TRBRACE) {
		struct node *stmt = parse_stmt(p);
		if (stmt)
			arrput(f->children, stmt);

		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return f;
}

static struct node *parse_struct(struct Parser *p) {
	expect(p, TSTRUCT);

	struct type *ty = maketype(p, p->tok);
	struct node *s = makestruct(p, p->tok, ty);
	next(p);

	expect(p, TLBRACE);
	skip_newlines(p);
	// NOTE: similar code to parse_callargs()
	while (p->tok.type != TRBRACE) {
		Token tok = p->tok;
		expect(p, TIDENT);
		struct type *ty = parse_type(p);
		arrput(s->children, makedecl(p, tok, NULL, ty));
		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return s;
}

static struct node *parse_toplevel(struct Parser *p) {
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

struct node *parse(struct Parser *p) {
	struct node **nodes = NULL;

	while (p->tok.type != TEOF) {
		struct node *func = parse_toplevel(p);
		arrput(nodes, func);
		skip_newlines(p);
	}

	return makefile(p, nodes);
}
