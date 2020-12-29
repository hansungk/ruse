#include "lexer.h"
#include "stretchy_buffer.h"
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *token_names[NUM_TOKENTYPES] = {
    [TOK_IDENT] = "identifier", [TOK_NUM] = "number", [TOK_NEWLINE] = "\\n",
    [TOK_ARROW] = "->",		[TOK_LPAREN] = "(",   [TOK_RPAREN] = ")",
    [TOK_LBRACE] = "{",		[TOK_RBRACE] = "}",   [TOK_DOT] = ".",
    [TOK_COMMA] = ",",		[TOK_COLON] = ":",    [TOK_SEMICOLON] = ";",
    [TOK_EQUALS] = "=",		[TOK_PLUS] = "+",     [TOK_STAR] = "*",
    [TOK_AMPERSAND] = "&",	[TOK_SLASH] = "/",    [TOK_BANG] = "!",
    [TOK_DATE] = "date",	[TOK_END] = "end",    [TOK_ERR] = "unknown",
};

static struct token_map symbols[] = {{"->", TOK_ARROW}, {NULL, 0}};

struct token_map keywords[] = {
    {"date", TOK_DATE},
    {"version", TOK_VERSION},
    {"timescale", TOK_TIMESCALE},
    {"comment", TOK_COMMENT},
    {"scope", TOK_SCOPE},
    {"begin", TOK_BEGIN},
    {"fork", TOK_FORK},
    {"function", TOK_FUNCTION},
    {"module", TOK_MODULE},
    {"task", TOK_TASK},
    {"var", TOK_VAR},
    {"wire", TOK_WIRE},
    {"reg", TOK_REG},
    {"upscope", TOK_UPSCOPE},
    {"enddefinitions", TOK_ENDDEFINITIONS},
    {"dumpvars", TOK_DUMPVARS},
    {"end", TOK_END},
    {NULL, 0},
};

int is_keyword(Token tok) {
	return TOK_KEYWORDS < tok.type && tok.type < TOK_ERR;
}

static char *readfile(const char *filename, long *filesize) {
	FILE *f = fopen(filename, "r");
	if (!f) {
		fprintf(stderr, "error: %s: %s\n", filename, strerror(errno));
		exit(EXIT_FAILURE);
	}
	fseek(f, 0, SEEK_END);
	*filesize = ftell(f);
	rewind(f);

	char *s = malloc(*filesize + 1);
	if (!s) {
		fprintf(stderr, "fatal: out of memory\n");
		exit(EXIT_FAILURE);
	}
	fread(s, *filesize, 1, f);
	s[*filesize] = '\0';
	fclose(f);
	return s;
}

static void step(Lexer *l) {
	if (l->rd_off < l->srclen) {
		l->off = l->rd_off;
		l->ch = l->src[l->off];
		if (l->ch == '\n') {
			sb_push(l->line_offs, l->off);
		}
		l->rd_off++;
	} else {
		l->off = l->srclen;
		l->ch = 0; // EOF
	}
}

int lexer_from_file(Lexer *l, const char *filename) {
	memset(l, 0, sizeof(Lexer));
	memcpy(l->filename, filename, 255);
	l->src = readfile(filename, &l->srclen);
	step(l);
	return 1;
}

int lexer_from_buf(Lexer *l, const char *buf, size_t len) {
	memset(l, 0, sizeof(Lexer));
	strncpy(l->filename, "(null)", 255);
	l->srclen = len;
	l->src = calloc(len + 1, 1);
	strncpy(l->src, buf, len);
	step(l);
	return 1;
}

void lexer_cleanup(Lexer *l) {
	sb_free(l->line_offs);
	free(l->src);
}

// step() n times.
// NOTE: more strict?
static void consume(Lexer *l, long n) {
	for (long i = 0; i < n; i++)
		step(l);
}

static char lookn(Lexer *l, long n) {
	if (l->off + n < l->srclen)
		return l->src[l->off + n];
	return '\0';
}

static void maketoken(Lexer *l, TokenType type) {
	memset(&l->tok, 0, sizeof(Token));
	l->tok.type = type;
	l->tok.range = (SrcRange){l->start, l->off};
}

// maketoken, but specify its end position.
static void maketoken_end(Lexer *l, TokenType type, long end) {
	memset(&l->tok, 0, sizeof(Token));
	l->tok.type = type;
	l->tok.range = (SrcRange){l->start, end};
}

static void lex_ident_or_keyword(Lexer *l) {
	while (isalnum(l->ch) || l->ch == '_')
		step(l);

	for (struct token_map *m = &keywords[0]; m->text != NULL; m++) {
		const char *c = m->text;
		const char *s = l->src + l->start;

		// skip over characters common for the source text and the
		// candidate
		for (; *c != '\0' && s < l->src + l->off && *c == *s; c++, s++)
			;

		if (*c == '\0' && s == l->src + l->off) {
			maketoken(l, m->type);
			return;
		}
	}

	maketoken(l, TOK_IDENT);
}

void lex_identifier_code(Lexer *l) {
	while (!isspace(l->ch))
		step(l);

	maketoken(l, TOK_IDENTCODE);
}

static void skip_numbers(Lexer *l) {
	while (isdigit(l->ch))
		step(l);
}

static void lex_number(Lexer *l) {
	skip_numbers(l);
	if (l->ch == '.') {
		step(l);
		skip_numbers(l);
	}
	maketoken(l, TOK_NUM);
}

static void lex_string(Lexer *l) {
	step(l); // opening "
	while (l->ch != '\0') {
		switch (l->ch) {
		case '\\': // escaped char
			step(l);
			step(l);
			break;
		case '"':
			step(l); // closing "
			goto strclose;
		default:
			step(l);
			break;
		}
	}
strclose:
	maketoken(l, TOK_STRING);
}

// NOTE: taken from ponyc
static void lex_symbol(Lexer *l) {
	for (const struct token_map *m = &symbols[0]; m->text != NULL; m++) {
		for (int i = 0; m->text[i] == '\0' || m->text[i] == lookn(l, i);
		     i++) {
			if (m->text[i] == '\0') {
				consume(l, i);
				maketoken(l, m->type);
				return;
			}
		}
	}

	// assume anything is a single-char symbol
	char c = lookn(l, 0);
	step(l);
	maketoken(l, (TokenType)c);
	return;
}

// Lex until a 'delimiter' string is seen, considering all text between as a
// single token.
static void lex_until(Lexer *l, const char *delimiter) {
	int seen_any = 0;
	long last_nonwhite = 0;

	while (l->ch != '\0') {
		// delimiter match
		for (int i = 0;
		     delimiter[i] == '\0' || delimiter[i] == lookn(l, i); i++) {
			if (delimiter[i] == '\0') { // match reached end
				long end =
				    seen_any ? last_nonwhite + 1 : l->off;
				maketoken_end(l, TOK_VERBATIM, end);
				return;
			}
		}

		seen_any = 1;
		if (!isspace(l->ch)) {
			last_nonwhite = l->off;
		}

		step(l);
	}

	// TODO: strip whitespace here
	maketoken(l, TOK_EOF);
}

// Checks if the character the lexer is currently pointing at is a char.
// Useful for grammars with meaningful whitespaces.
int at_whitespace(const Lexer *l) { return l->ch == ' ' || l->ch == '\t'; }

// Lex the next token and place it at l->tok.
// Return EOF if reached source EOF.
int lex_next(Lexer *l) {
	for (;;) {
		l->start = l->off;

		switch (l->ch) {
		case '\0': {
			maketoken(l, TOK_EOF);
			return EOF;
		}
		case ' ':
		case '\t': {
			// whitespaces
			step(l);
			break;
		}
		case '"': {
			lex_string(l);
			return 0;
		}
		case '/': {
			step(l);
			if (l->ch == '/') {
				while (l->ch != '\n') {
					step(l);
				}
				maketoken(l, TOK_COMMENT);
				return 0;
			} else {
				maketoken(l, TOK_SLASH);
				return 0;
			}
		}
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9': {
			lex_number(l);
			return 0;
		}
		default: {
			if (isalpha(l->ch) || l->ch == '_') {
				lex_ident_or_keyword(l);
			} else {
				lex_symbol(l);
			}
			return 0;
		}
		}
	}

	return 0;
}

// In case lex_next cannot determine the token type automatically, manually
// specify the next token type. The lexer will dispatch the apropriate lexer
// function upon this type.
int lex_next_manual(Lexer *l, enum TokenType t) {
	for (;;) {
		l->start = l->off;

		switch (l->ch) {
		case '\0': {
			maketoken(l, TOK_EOF);
			return EOF;
		}
		case ' ':
		case '\t': {
			// whitespaces
			step(l);
			break;
		}
		default:
			goto manual;
		}
	}

manual:
	l->start = l->off;

	switch (t) {
	case TOK_VERBATIM:
		lex_until(l, "$end");
		break;
	case TOK_IDENTCODE:
		lex_identifier_code(l);
		break;
	default:
		assert(!"unsupported token type");
		break;
	}

	return 0;
}

// FIXME: lifetime of 'filename'?
SrcLoc lexer_locate(Lexer *l, size_t pos) {
	// search linearly for line that contains this position
	// TODO: performance

	if (sb_count(l->line_offs) == 0)
		// First line
		return (SrcLoc){l->filename, 1, pos + 1};

	int line;
	for (line = 0; line < sb_count(l->line_offs); line++)
		if (pos < l->line_offs[line])
			break;

	int col = pos - l->line_offs[line - 1];
	return (SrcLoc){l->filename, line + 1, col};
}

// Print 'tok' as string into buf.
// Needs lexer because it needs the source text.
char *tokenstr(Lexer *lex, Token tok, char *buf, size_t blen) {
	size_t tlen = tok.range.end - tok.range.start;
	size_t strlen = (blen - 1) < tlen ? (blen - 1) : tlen;
	strncpy(buf, lex->src + tok.range.start, strlen);
	buf[strlen] = '\0';
	return buf;
}

// Return the descriptive name for a TokenType enum.
// Different from tokenstr as it does not return the actual text for
// the token, but the description for the type of the token.
char *tokentypestr(enum TokenType t, char *buf, size_t blen) {
	if (t >= TOK_ASCII)
		snprintf(buf, blen, "%s", token_names[t]);
	else if (t == '\n')
		snprintf(buf, blen, "\\n");
	else if (t == TOK_EOF)
		snprintf(buf, blen, "EOF");
	else
		snprintf(buf, blen, "%c", (char)t);
	buf[blen - 1] = '\0';
	return buf;
}

void token_print(Lexer *l, const Token tok) {
	char buf[MAXTOKLEN];

	switch (tok.type) {
	case TOK_IDENT:
	case TOK_IDENTCODE:
	case TOK_COMMENT:
	case TOK_NUM:
	case TOK_STRING:
	case TOK_VERBATIM:
		printf("'%.*s'", (int)(tok.range.end - tok.range.start),
		       l->src + tok.range.start);
		break;
	case TOK_ERR:
		printf("error");
		break;
	default:
		tokentypestr(tok.type, buf, sizeof(buf));
		printf("%s", buf);
		break;
	}
}
