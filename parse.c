#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Node *parse_expr(Parser *p);
static Node *parse_typeexpr(Parser *p);

static Node *makenode(Parser *p, enum NodeKind k, Token tok) {
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

static Node *makefile(Parser *p, Node **toplevel) {
	Node *n = makenode(p, ND_FILE, p->tok);
	n->stmts = toplevel;
	return n;
}

static Node *makefunc(Parser *p, Token name) {
	Node *n = makenode(p, ND_FUNC, name);
	return n;
}

static Node *makebinexpr(Parser *p, Node *lhs, Token op, Node *rhs) {
	Node *n = makenode(p, ND_BINEXPR, op);
	n->lhs = lhs;
	n->rhs = rhs;
	return n;
}

static Node *makedecl(Parser *p, Token name, Node *rhs /*TODO: type*/) {
	Node *n = makenode(p, ND_DECL, name);
	n->rhs = rhs;
	return n;
}

static Node *makeret(Parser *p, Node *rhs) {
	Node *n = makenode(p, ND_RETURN, p->tok);
	n->rhs = rhs;
	return n;
}

static Node *makeexprstmt(Parser *p, Node *rhs) {
	Node *n = makenode(p, ND_EXPRSTMT, p->tok);
	n->rhs = rhs;
	return n;
}

static Node *makeassign(Parser *p, Node *lhs,
			       Node *rhs) {
	Node *n = makenode(p, ND_ASSIGN, p->tok);
	n->lhs = lhs;
	n->rhs = rhs;
	return n;
}

// Initialize a parser that parses the given filename.
void parser_from_file(Parser *p, const char *filename) {
	memset(p, 0, sizeof(Parser));
	lexer_from_file(&p->l, filename);
	// start by putting something on p->tok
	// can't use next() because p->lexer.tok is initialized to TOK_EOF
	lex(&p->l);
	p->tok = p->l.tok;
}

void parser_from_buf(Parser *p, const char *buf, size_t len) {
	memset(p, 0, sizeof(Parser));
	lexer_from_buf(&p->l, buf, len);
	lex(&p->l);
	p->tok = p->l.tok;
}

void parser_cleanup(Parser *p) {
	for (int i = 0; i < arrlen(p->nodeptrbuf); i++) {
		Node *n = p->nodeptrbuf[i];
		if (n) {
			if (n->stmts)
				arrfree(n->stmts);
			free(n);
		}
	}

	lexer_cleanup(&p->l);
	arrfree(p->nodeptrbuf);
}

static void next(Parser *p) {
	if (p->l.tok.type == TOK_EOF)
		return;

	lex(&p->l);
	p->tok = p->l.tok;
}

static void error(Parser *p, const char *fmt, ...) {
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	SrcLoc loc = locate(&p->l.src, p->tok.range.start);
	fprintf(stderr, "%s:%d:%d:(%ld): %s\n", loc.filename, loc.line, loc.col,
		p->tok.range.start, msg);
	exit(EXIT_FAILURE);
}

static void skip_while(Parser *p, enum TokenType type) {
	while (p->tok.type != TOK_EOF && p->tok.type == type) {
		next(p);
	}
}

static void skip_newlines(Parser *p) {
	while (p->tok.type == '\n' || p->tok.type == TOK_COMMENT) {
		skip_while(p, '\n');
		skip_while(p, TOK_COMMENT);
	}
}

static void skip_to_end_of_line(Parser *p) {
	while (p->tok.type != TOK_EOF && p->tok.type != TOK_NEWLINE) {
		next(p);
	}
}

static int expect(Parser *p, enum TokenType t) {
	if (p->tok.type != t) {
		char ebuf[MAXTOKLEN], gbuf[MAXTOKLEN];
		tokentypestr(t, ebuf, sizeof(ebuf));
		tokenstr(p->l.src.src, p->tok, gbuf, sizeof(gbuf));
		if (strcmp(gbuf, "\n") == 0) {
			strcpy(gbuf, "\\n");
		}
		error(p, "expected '%s', got '%s'", ebuf, gbuf);
	}
	// make progress
	next(p);
	return 1;
}

// Expect end of line, skipping over any end-of-line comments.
static int expect_end_of_line(Parser *p) {
	skip_while(p, TOK_COMMENT);
	return expect(p, TOK_NEWLINE);
}

// TODO: remove
static void tokentypeprint(enum TokenType t) {
	char buf[MAXTOKLEN];
	tokentypestr(t, buf, sizeof(buf));
	printf("i saw %s\n", buf);
}

static Node *parse_decl(Parser *p) {
	assert(p->tok.type == TOK_VAR || p->tok.type == TOK_CONST);
	next(p);

	Token name = p->tok;
	next(p);

	if (p->tok.type != TOK_EQUAL) {
		parse_typeexpr(p);
	}

	expect(p, TOK_EQUAL);

	Node *rhs = parse_expr(p);
	return makedecl(p, name, rhs);
}

static Node *parse_return(Parser *p) {
	expect(p, TOK_RETURN);
	Node *rhs = parse_expr(p);
	return makeret(p, rhs);
}

static Node *parse_unaryexpr(Parser *p) {
	Node *e = NULL;
	Token tok;

	switch (p->tok.type) {
	case TOK_IDENT:
		tok = p->tok;
		next(p);
		return makenode(p, ND_IDEXPR, tok);
		// TODO: funccall
	case TOK_NUM:
		tok = p->tok;
		next(p);
		return makenode(p, ND_LITERAL, tok);
	case TOK_LPAREN:
		expect(p, TOK_LPAREN);
		e = parse_expr(p);
		expect(p, TOK_RPAREN);
		break;
	default:
		error(p, "expected an expression");
		break;
	}

	return e;
}

static int get_precedence(const Token op) {
	switch (op.type) {
	case TOK_STAR:
	case TOK_SLASH:
		return 1;
	case TOK_PLUS:
	case TOK_MINUS:
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
static Node *parse_binexpr_rhs(Parser *p, Node *lhs, int precedence) {
	while (1) {
		int this_prec = get_precedence(p->tok);

		// If the upcoming op has lower precedence, the subexpression of
		// the precedence level that we are currently parsing in is
		// finished. This is equivalent to reducing on a shift/reduce
		// conflict in bottom-up parsing.
		if (this_prec < precedence) {
			break;
		}

		Token op = p->tok;
		next(p);

		// Parse the next term.  We do not know yet if this term should
		// bind to LHS or RHS; e.g. "a * b + c" or "a + b * c".  To know
		// this, we should look ahead for the operator that follows this
		// term.
		Node *rhs = parse_unaryexpr(p);
		if (!rhs) {
			error(p, "expected expression");
		}
		int next_prec = get_precedence(p->tok);

		// If the next operator is indeed higher-level, evaluate the RHS
		// as a whole subexpression with elevated minimum precedence.
		// Else, just treat it as a unary expression.  This is
		// equivalent to shifting on a shift/reduce conflict in
		// bottom-up parsing.
		//
		// If this_prec == next_prec, don't shift, but reduce it with
		// lhs. This implies left associativity.
		if (this_prec < next_prec) {
			rhs = parse_binexpr_rhs(p, rhs, precedence + 1);
		}
		lhs = makebinexpr(p, lhs, op, rhs);
	}
	return lhs;
}

static Node *parse_expr(Parser *p) {
	Node *e = parse_unaryexpr(p);
	e = parse_binexpr_rhs(p, e, 0);
	return e;
}

// Possibly parse the equal and RHS expression of an expression.
// 'expr' is already consumed.
static Node *parse_assign_or_expr_stmt(Parser *p, Node *expr) {
	Node *stmt = NULL;

	if (p->tok.type == TOK_EQUAL) {
		next(p);
		Node *rhs = parse_expr(p);
		stmt = makeassign(p, expr, rhs);
	} else {
		stmt = makeexprstmt(p, expr);
	}
	expect_end_of_line(p);
	return stmt;
}

static Node *parse_stmt(Parser *p) {
	Node *stmt;

	skip_newlines(p);

	switch (p->tok.type) {
	case TOK_EOF:
		return NULL;
	case TOK_COMMENT:
		expect_end_of_line(p);
		return parse_stmt(p);
	case TOK_VAR:
	case TOK_CONST:
		return parse_decl(p);
	case TOK_RETURN:
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
	// expect(p, TOK_NEWLINE);

	// all productions from now on start with an expression
	// TODO: exprstmt?
	stmt = parse_expr(p);
	stmt = parse_assign_or_expr_stmt(p, stmt);
	return stmt;
}

static Node *parse_typeexpr(Parser *p) {
	if (p->tok.type == TOK_INT) {
		expect(p, TOK_INT);
	}
	return makenode(p, ND_TYPEEXPR, p->tok);
}

static Node *parse_func(Parser *p) {
	expect(p, TOK_FUNC);

	Node *func = makefunc(p, p->tok);
	next(p);

	// argument list
	expect(p, TOK_LPAREN);
	// func->paramdecls = parse_paramdecllist(p);
	expect(p, TOK_RPAREN);

	// return type
	if (p->tok.type != TOK_LBRACE) {
		func->rettypeexpr = parse_typeexpr(p);
	}

	// body
	expect(p, TOK_LBRACE);
	while (p->tok.type != TOK_RBRACE) {
		Node *stmt = parse_stmt(p);
		if (stmt) {
			arrput(func->stmts, stmt);
		}

		skip_newlines(p);
	}
	expect(p, TOK_RBRACE);

	return func;
}

static Node *parse_toplevel(Parser *p) {
	skip_newlines(p);

	switch (p->tok.type) {
	case TOK_FUNC:
		return parse_func(p);
	default:
		error(p, "unknown token type %d at toplevel", p->tok.type);
		return NULL;
	}
}

Node *parse(Parser *p) {
	Node **nodes = NULL;

	while (p->tok.type != TOK_EOF) {
		Node *func = parse_toplevel(p);
		arrput(nodes, func);
		skip_newlines(p);
	}

	return makefile(p, nodes);
}
