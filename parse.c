#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Node *parse_expr(struct Parser *p);
static Node *parse_stmt(struct Parser *p);
static Node *parse_typeexpr(struct Parser *p);

static Node *makenode(struct Parser *p, enum NodeKind k, Token tok) {
	// TODO: store all nodes in a contiguous buffer for better locality?
	// should be careful about node pointers going stale though
	Node *node = calloc(1, sizeof(Node));
	if (!node) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	node->kind = k;
	node->tok = tok;
	arrput(p->nodeptrbuf, node);
	return node;
}

static Node *makefile(struct Parser *p, Node **toplevel) {
	Node *n = makenode(p, NFILE, p->tok);
	n->children = toplevel;
	return n;
}

static Node *makefunc(struct Parser *p, Token name) {
	Node *n = makenode(p, NFUNC, name);
	return n;
}

static Node *makestruct(struct Parser *p, Token name) {
	Node *n = makenode(p, NSTRUCT, name);
	return n;
}

static Node *makebinexpr(struct Parser *p, Node *lhs, Token op, Node *rhs) {
	Node *n = makenode(p, NBINEXPR, op);
	n->lhs = lhs;
	n->rhs = rhs;
	return n;
}

static Node *makecall(struct Parser *p, Node *lhs, Node **args) {
	Node *n = makenode(p, NCALL, p->tok /*unused*/);
	n->lhs = lhs;
	assert(!n->children);
	n->children = args;
	return n;
}

// 'lhs' is the 'a.b()' part of 'a.b().c'.
static Node *makemember(struct Parser *p, Token member, Node *lhs) {
	Node *n = makenode(p, NMEMBER, member);
	n->parent = lhs;
	return n;
}

static Node *makedecl(struct Parser *p, Token name, Node *initexpr /*TODO: type*/) {
	Node *n = makenode(p, NDECL, name);
	n->rhs = initexpr;
	return n;
}

static Node *makereturn(struct Parser *p, Node *rhs) {
	Node *n = makenode(p, NRETURN, p->tok);
	n->rhs = rhs;
	return n;
}

static Node *makeexprstmt(struct Parser *p, Node *rhs) {
	Node *n = makenode(p, NEXPRSTMT, p->tok);
	n->rhs = rhs;
	return n;
}

static Node *makeassign(struct Parser *p, Node *lhs, Node *rhs) {
	Node *n = makenode(p, NASSIGN, p->tok);
	n->lhs = lhs;
	n->rhs = rhs;
	return n;
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
		Node *n = p->nodeptrbuf[i];
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

static Node *parse_decl(struct Parser *p) {
	assert(p->tok.type == TVAR || p->tok.type == TCONST);
	next(p);

	Token name = p->tok;
	next(p);

	if (p->tok.type != TEQUAL) {
		parse_typeexpr(p);
	}

	Node *rhs = NULL;
	if (!end_of_line(p)) {
		expect(p, TEQUAL);
		rhs = parse_expr(p);
	}

	return makedecl(p, name, rhs);
}

static Node *parse_blockstmt(struct Parser *p) {
	Node *n = makenode(p, NBLOCKSTMT, p->tok);

	expect(p, TLBRACE);
	while (p->tok.type != TRBRACE) {
		Node *s = parse_stmt(p);
		arrput(n->children, s);
		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return n;
}

static Node *parse_return(struct Parser *p) {
	expect(p, TRETURN);
	Node *rhs = parse_expr(p);
	return makereturn(p, rhs);
}

// Assumes enclosing '(' is already consumed.
// Does not consume ending ')'.
static Node **parse_callargs(struct Parser *p) {
	Node **list = NULL;
	while (p->tok.type != TRPAREN) {
		arrput(list, parse_expr(p));
		if (p->tok.type == TCOMMA)
			next(p);
		else
			break;
	}
	return list;
}

static Node *parse_unaryexpr(struct Parser *p) {
	Node *e = NULL;
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
			Node **args = parse_callargs(p);
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
static Node *parse_binexpr_rhs(struct Parser *p, Node *lhs, int precedence) {
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
		Node *rhs = parse_unaryexpr(p);
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

static Node *parse_expr(struct Parser *p) {
	Node *e = parse_unaryexpr(p);
	e = parse_binexpr_rhs(p, e, 0);
	return e;
}

// Possibly parse the equal and RHS expression of an expression.
// 'expr' is already consumed.
static Node *parse_assign_or_expr_stmt(struct Parser *p, Node *expr) {
	Node *stmt = NULL;

	if (p->tok.type == TEQUAL) {
		next(p);
		Node *rhs = parse_expr(p);
		stmt = makeassign(p, expr, rhs);
	} else {
		stmt = makeexprstmt(p, expr);
	}
	expect_end_of_line(p);
	return stmt;
}

static Node *parse_stmt(struct Parser *p) {
	Node *stmt;

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
	// 	Node *decl = parse_decl(p);
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

static Node *parse_typeexpr(struct Parser *p) {
	if (p->tok.type == TINT) {
		expect(p, TINT);
	} else if (p->tok.type == TIDENT) {
		expect(p, TIDENT);
	} else {
		error(p, "expected a type (TODO)");
	}
	return makenode(p, NTYPEEXPR, p->tok);
}

static Node *parse_func(struct Parser *p) {
	expect(p, TFUNC);

	Node *f = makefunc(p, p->tok);
	next(p);

	// argument list
	expect(p, TLPAREN);
	// func->paramdecls = parse_paramdecllist(p);
	expect(p, TRPAREN);

	// return type
	if (p->tok.type != TLBRACE)
		f->rettypeexpr = parse_typeexpr(p);

	// body
	expect(p, TLBRACE);
	while (p->tok.type != TRBRACE) {
		Node *stmt = parse_stmt(p);
		if (stmt)
			arrput(f->children, stmt);

		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return f;
}

static Node *parse_struct(struct Parser *p) {
	expect(p, TSTRUCT);

	Node *s = makestruct(p, p->tok);
	next(p);

	expect(p, TLBRACE);
	skip_newlines(p);
	// NOTE: similar code to parse_callargs()
	while (p->tok.type != TRBRACE) {
		Token tok = p->tok;
		expect(p, TIDENT);
		parse_typeexpr(p); // TODO: use result
		arrput(s->children, makedecl(p, tok, NULL));
		skip_newlines(p);
	}
	expect(p, TRBRACE);

	return s;
}

static Node *parse_toplevel(struct Parser *p) {
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

Node *parse(struct Parser *p) {
	Node **nodes = NULL;

	while (p->tok.type != TEOF) {
		Node *func = parse_toplevel(p);
		arrput(nodes, func);
		skip_newlines(p);
	}

	return makefile(p, nodes);
}
