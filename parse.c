#include "ruse.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct Node *makenode(struct Parser *p, enum NodeKind k, Token tok) {
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

static struct Node *makefile(struct Parser *p, struct Node **children) {
	struct Node *n = makenode(p, ND_FILE, p->tok);
	n->children = children;
	return n;
}

static struct Node *makeatom(struct Parser *p, Token tok) {
	struct Node *n = makenode(p, ND_ATOM, tok);
	n->tok = tok;
	return n;
}

static struct Node *makelist(struct Parser *p, struct Node **children) {
	struct Node *n = makenode(p, ND_LIST, p->tok); // TODO: tok is unused
	n->children = children;
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

// Like expect, but do not make progress.
static int expect_peek(struct Parser *p, enum TokenType t) {
	if (p->tok.type != t) {
		char ebuf[MAXTOKLEN], gbuf[MAXTOKLEN];
		tokentypestr(t, ebuf, sizeof(ebuf));
		tokenstr(&p->l, p->tok, gbuf, sizeof(gbuf));
		fprintf(stderr, "parse error at %ld: expected '%s', got '%s'\n",
			p->tok.range.start, ebuf, gbuf);
		exit(1);
	}
	return 1;
}

static int expect(struct Parser *p, enum TokenType t) {
	int r = expect_peek(p, t);
	// make progress
	next(p);
	return r;
}

static void skip_while(struct Parser *p, enum TokenType type) {
	while (p->tok.type != TOK_EOF && p->tok.type == type) {
		next(p);
	}
}

static void skip_newlines(struct Parser *p) { skip_while(p, '\n'); }

// TODO: remove
static void tokentypeprint(enum TokenType t) {
	char buf[MAXTOKLEN];
	tokentypestr(t, buf, sizeof(buf));
	printf("i saw %s\n", buf);
}

// Opening paren is already consumed.
static struct Node *parse_sexp(struct Parser *p) {
	struct Node **children = NULL;

	assert(p->tok.type == '(');
	next(p);

	while (p->tok.type != TOK_EOF && p->tok.type != ')') {
		if (p->tok.type == '(') {
			sb_push(children, parse_sexp(p));
		} else {
			expect(p, TOK_ATOM);
			sb_push(children, makeatom(p, p->tok));
		}
	}

	expect(p, ')');
	return makelist(p, children);
}

struct Node *parse_file(struct Parser *p) {
	struct Node **toplevel = NULL;

	skip_newlines(p);

	for (;;) {
		struct Node *n = NULL;

		switch (p->tok.type) {
		case TOK_EOF:
			return makefile(p, toplevel);
		case '(':
			n = parse_sexp(p);
			if (!n) {
				goto fail;
			}
			sb_push(toplevel, n);
			break;
		case ';':
			fprintf(stderr, "todo: comment\n");
			goto fail;
		default:
			fprintf(stderr, "unknown token: ");
			tokentypeprint(p->tok.type);
			goto fail;
		}

		skip_newlines(p);
	}

fail:
	printf("broke before EOF, p->tok.range.start=%ld\n",
	       p->tok.range.start);
	return makefile(p, toplevel);
}
