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

static struct Node *makeatom(struct Parser *p, struct Token tok) {
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

// TODO: remove
static void tokentypeprint(enum TokenType t) {
	char buf[MAXTOKLEN];
	tokentypestr(t, buf, sizeof(buf));
	printf("i saw %s\n", buf);
}

static struct Node *parse_toplevel(struct Parser *p) {
	skip_newlines(p);

	switch (p->tok.type) {
	case TOK_PROC:
		printf("saw proc\n");
		return NULL;
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
