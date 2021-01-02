#include "ruse.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct Val *makeval(struct Parser *p, enum ValKind k, struct Token tok) {
	// TODO: store all vals in a contiguous buffer for better locality?
	// should be careful about val pointers going stale though
	struct Val *v = calloc(1, sizeof(struct Val));
	if (!v) {
		fprintf(stderr, "alloc error\n");
		exit(1);
	}
	v->kind = k;
	v->tok = tok;
	sb_push(p->valptrbuf, v);
	return v;
}

static struct Val *makeatom(struct Parser *p, struct Token tok) {
	struct Val *v = makeval(p, VAL_ATOM, tok);
	v->tok = tok;
	return v;
}

static struct Val *makelist(struct Parser *p, struct Val **children) {
	struct Val *v = makeval(p, VAL_LIST, p->tok); // TODO: tok is unused
	v->children = children;
	return v;
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
	for (int i = 0; i < sb_count(p->valptrbuf); i++) {
		struct Val *v = p->valptrbuf[i];
		if (v) {
			if (v->children)
				sb_free(v->children);
			free(v);
		}
	}

	lexer_cleanup(&p->l);
	sb_free(p->valptrbuf);
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

// Opening paren is already consumed.
static struct Val *parse_sexp(struct Parser *p) {
	struct Val **children = NULL;

	assert(p->tok.type == '(');
	next(p);

	while (p->tok.type != TOK_EOF && p->tok.type != ')') {
		if (p->tok.type == '(') {
			sb_push(children, parse_sexp(p));
		} else {
			struct Token t = p->tok;
			next(p);
			sb_push(children, makeatom(p, t));
		}
	}

	expect(p, ')');
	return makelist(p, children);
}

struct Val *ruse_read(struct Parser *p) {
	struct Token t;

	skip_newlines(p);

	switch (p->tok.type) {
	case TOK_EOF:
		return NULL;
	case ';':
		while (p->tok.type != TOK_EOF && p->tok.type != TOK_NEWLINE) {
			next(p);
		}
		return ruse_read(p);
	case '(':
		return parse_sexp(p);
	case TOK_IDENT:
	case TOK_NUM:
		t = p->tok;
		next(p);
		return makeatom(p, t);
	default:
		fprintf(stderr, "unknown token: ");
		tokentypeprint(p->tok.type);
		return NULL;
	}
}
