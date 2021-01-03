#include "ruse.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct Node *makenode(struct Parser *p, enum NodeKind k, struct Token tok) {
	// TODO: store all nodes in a contiguous buffer for better locality?
	// should be careful about node pointers going stale though
	struct Node *node = calloc(1, sizeof(struct Node));
	if (!node) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	node->kind = k;
	node->tok = tok;
	sb_push(p->nodeptrbuf, node);
	return node;
}

static struct Node *makefile(struct Parser *p, struct Node **toplevel) {
	struct Node *n = makenode(p, ND_FILE, p->tok);
	n->children = toplevel;
	return n;
}

static struct Node *makefunc(struct Parser *p, struct Token name) {
	struct Node *n = makenode(p, ND_FUNC, name);
	return n;
}

// Initialize a Parser that parses the given filename.
void parser_from_file(struct Parser *p, const char *filename) {
	memset(p, 0, sizeof(struct Parser));
	lexer_from_file(&p->l, filename);
	// start by putting something on p->tok
	// can't use next() because p->lexer.tok is initialized to TOK_EOF
	lex_next(&p->l);
	p->tok = p->l.tok;
}

void parser_from_buf(struct Parser *p, const char *buf, size_t len) {
	memset(p, 0, sizeof(struct Parser));
	lexer_from_buf(&p->l, buf, len);
	lex_next(&p->l);
	p->tok = p->l.tok;
}

void parser_cleanup(struct Parser *p) {
	for (int i = 0; i < sb_count(p->nodeptrbuf); i++) {
		struct Node *n = p->nodeptrbuf[i];
		if (n) {
			if (n->children)
				sb_free(n->children);
			free(n);
		}
	}

	lexer_cleanup(&p->l);
	sb_free(p->nodeptrbuf);
}

static void next(struct Parser *p) {
	if (p->l.tok.type == TOK_EOF)
		return;

	lex_next(&p->l);
	p->tok = p->l.tok;
}

static int expect(struct Parser *p, enum TokenType t) {
	if (p->tok.type != t) {
		char ebuf[MAXTOKLEN], gbuf[MAXTOKLEN];
		tokentypestr(t, ebuf, sizeof(ebuf));
		tokenstr(&p->l, p->tok, gbuf, sizeof(gbuf));
		fprintf(stderr, "parse error at %ld: expected '%s', got '%s'\n",
			p->tok.range.start, ebuf, gbuf);
		exit(1);
	}
	// make progress
	next(p);
	return 1;
}

static void skip_while(struct Parser *p, enum TokenType type) {
	while (p->tok.type != TOK_EOF && p->tok.type == type) {
		next(p);
	}
}

static void skip_newlines(struct Parser *p) { skip_while(p, '\n'); }

static void skip_to_end_of_line(struct Parser *p) {
	while (p->tok.type != TOK_EOF && p->tok.type != TOK_NEWLINE) {
		next(p);
	}
}

// TODO: remove
static void tokentypeprint(enum TokenType t) {
	char buf[MAXTOKLEN];
	tokentypestr(t, buf, sizeof(buf));
	printf("i saw %s\n", buf);
}

static struct Node *parse_stmt(struct Parser *p) {
	struct Node *stmt;

	skip_newlines(p);

	// switch (p->tok.type) {
	// case TOK_EOF:
	// 	return NULL;
	// // case TOK_RETURN:
	// // 	stmt = parseReturnStmt(p);
	// // 	return stmt;
	// default:
	// 	break;
	// }

	skip_to_end_of_line(p);
	expect(p, TOK_NEWLINE);

	// if (is_decl_start(p)) {
	// 	Node *decl = parse_decl(p);
	// 	stmt = makeDecl_stmt(p, decl);
	// 	expectEndOfLine(p);
	// 	return stmt;
	// }

	// all productions from now on start with an expression
	// Node *expr = parse_expr(p);
	// if (expr)
	// 	return parseAssignOrExprStmt(p, expr);

	// no production has succeeded
	// TODO: unreachable?
	return NULL;
}

static struct Node *parse_func(struct Parser *p) {
	expect(p, TOK_DEF);

	struct Node *func = makefunc(p, p->tok);
	next(p);

	// argument list
	expect(p, TOK_LPAREN);
	// func->paramdecls = parse_paramdecllist(p);
	expect(p, TOK_RPAREN);

	// return type
	func->rettypeexpr = NULL;
	/* if (p->tok.type == TOK_ARROW) { */
	/*     expect(p, TOK_ARROW); */
	/*     func->rettypeexpr = parse_typeexpr(p); */
	/* } */

	/* if (p->tok.type != TOK_LBRACE) { */
	/*     error_expected(p, "'->' or '{'"); */
	/*     skip_until(p, TOK_LBRACE); */
	/* } */

	while (p->tok.type != TOK_END) {
		struct Node *stmt = parse_stmt(p);
		sb_push(func->children, stmt);
	}
	expect(p, TOK_END);

	return func;
}

static struct Node *parse_toplevel(struct Parser *p) {
	skip_newlines(p);

	switch (p->tok.type) {
	case TOK_DEF:
		return parse_func(p);
	default:
		assert(0 && "unreachable");
		return NULL;
	}
}

struct Node *parse(struct Parser *p) {
	struct Node **nodes = NULL;

	while (p->tok.type != TOK_EOF) {
		struct Node *func = parse_toplevel(p);
		sb_push(nodes, func);
		skip_newlines(p);
	}

	return makefile(p, nodes);
}
