#include "ruse.h"
#include "stb_ds.h"
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *token_names[NUM_TOKENTYPES] = {
	[TIDENT] = "identifier", [TNUM] = "number", [TNEWLINE] = "\\n",
	[TLPAREN] = "(",         [TRPAREN] = ")",   [TCOMMENT] = "comment",
};

struct TokenMap keywords[] = {
	{"var", TVAR},        {"const", TCONST},   {"fn", TFUNC},
	{"struct", TSTRUCT},  {"return", TRETURN}, {"int", TINT},
	{"string", TSTRING_}, {NULL, 0},
};

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
    if (l->rd_off < l->src.buflen) {
        l->off = l->rd_off;
        l->ch = l->src.buf[l->off];
        if (l->ch == '\n') {
            arrput(l->src.line_offs, l->off);
        }
        l->rd_off++;
    } else {
        l->off = l->src.buflen;
        l->ch = 0; // EOF
    }
}

static void consume(Lexer *l, long n) {
    for (long i = 0; i < n; i++) {
        step(l);
    }
}

static char lookn(Lexer *l, long n) {
    if (l->off + n < l->src.buflen) {
        return l->src.buf[l->off + n];
    }
    return '\0';
}

int lexer_from_file(Lexer *l, const char *filename) {
    memset(l, 0, sizeof(Lexer));
    strncpy(l->src.filename, filename, 255);
    l->src.buf = readfile(filename, &l->src.buflen);
    step(l);
    return 1;
}

int lexer_from_buf(Lexer *l, const char *buf, size_t len) {
    memset(l, 0, sizeof(Lexer));
    strncpy(l->src.filename, "(null)", 255);
    l->src.buflen = len;
    l->src.buf = calloc(len + 1, 1);
    strncpy(l->src.buf, buf, len);
    step(l);
    return 1;
}

void lexer_cleanup(Lexer *l) {
    arrfree(l->src.line_offs);
    free(l->src.buf);
}

// Make a new token and place it at 'l->tok'.
static void maketoken(Lexer *l, enum TokenType type) {
	memset(&l->tok, 0, sizeof(Token));
	l->tok.type = type;
	l->tok.range = (SrcRange){l->start, l->off};
	l->tok.loc = locate(&l->src, l->start);
}

static void lex_ident_or_keyword(Lexer *l) {
	// multi-char keywords
	for (const struct TokenMap *m = &keywords[0]; m->text != NULL; m++) {
		int i = 0;
		for (; m->text[i] != '\0' && m->text[i] == lookn(l, i); i++)
			; // nothing
		// check both candidate and token have terminated
		if (m->text[i] == '\0' && !isalnum(lookn(l, i)) &&
		    lookn(l, i) != '_') {
			consume(l, i);
			maketoken(l, m->type);
			// strdup here so that it becomes easy to clean tok.name later by
			// always free()ing it
			l->tok.name = strdup(m->text);
			return;
		}
	}

	// no keyword match, parse as an identifier
	while (isalnum(l->ch) || l->ch == '_')
		step(l);
	maketoken(l, TIDENT);
	long len = l->off - l->start;
	l->tok.name = calloc(len + 1, sizeof(char));
	strncpy(l->tok.name, l->src.buf + l->start, len);
	l->tok.name[len] = '\0';
}

static void lex_symbol(Lexer *l) {
    // multi-char symbol
    for (const struct TokenMap *m = &keywords[0]; m->text != NULL; m++) {
        for (int i = 0; m->text[i] == '\0' || m->text[i] == lookn(l, i); i++) {
            if (m->text[i] == '\0') {
                consume(l, i);
                maketoken(l, m->type);
                return;
            }
        }
    }

    // single-char symbol
    char ch = l->ch;
    step(l);
    maketoken(l, ch);
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
    maketoken(l, TNUM);
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
    maketoken(l, TSTRING);
}

// Lex the next token and place it at l->tok.
// Return EOF if reached source EOF.
int lex(Lexer *l) {
	for (;;) {
		l->start = l->off;

		switch (l->ch) {
		case '\0':
			maketoken(l, TEOF);
			return EOF;
		case ' ':
		case '\t':
		case '\r':
			// whitespaces
			step(l);
			break;
		case '"':
			lex_string(l);
			return 0;
		case '/':
			step(l);
			if (l->ch == '/') {
				while (l->ch != '\n') {
					step(l);
				}
				maketoken(l, TCOMMENT);
				return 0;
			} else {
				maketoken(l, '/');
				return 0;
			}
		case '-':
			step(l);
			if (l->ch == '>') {
				maketoken(l, TARROW);
				return 0;
			} else {
				maketoken(l, '-');
				return 0;
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
		case '9':
			lex_number(l);
			return 0;
		default:
			if (isalpha(l->ch) || l->ch == '_')
				lex_ident_or_keyword(l);
			else
				lex_symbol(l);
			return 0;
		}
	}

	return 0;
}

SrcLoc locate(Source *src, size_t pos) {
    // search linearly for line that contains this position
    // TODO: performance

    // single-line
    if (arrlen(src->line_offs) == 0) {
        return (SrcLoc){src->filename, 1, pos + 1};
    }

    int line;
    for (line = 0; line < arrlen(src->line_offs); line++) {
        if (pos < src->line_offs[line]) {
            break;
        }
    }

    int col = pos - src->line_offs[line - 1];
    return (SrcLoc){src->filename, line + 1, col};
}

// Print 'tok' as string into buf.
// Needs 'src' because it needs the source text.
// FIXME: use tok.name rather than trying to copy from the whole source
char *tokenstr(const char *src, struct Token tok, char *buf, size_t blen) {
	size_t tlen = tok.range.end - tok.range.start;
	size_t strlen = (blen - 1) < tlen ? (blen - 1) : tlen;
	strncpy(buf, src + tok.range.start, strlen);
	buf[strlen] = '\0';
	return buf;
}

// Compare the string content of the two tokens.
int tokeneq(const char *src, Token t1, Token t2) {
    char buf1[TOKLEN], buf2[TOKLEN];
    tokenstr(src, t1, buf1, sizeof(buf1));
    tokenstr(src, t2, buf2, sizeof(buf2));
    return (strcmp(buf1, buf2) == 0);
}

// Return the descriptive name for a TokenType enum.
// Different from tokenstr as it does not return the actual text for
// the token, but the description for the type of the token.
char *tokentypestr(enum TokenType t, char *buf, size_t blen) {
    if (t >= TASCII)
        snprintf(buf, blen, "%s", token_names[t]);
    else if (t == '\n')
        snprintf(buf, blen, "\\n");
    else if (t == TEOF)
        snprintf(buf, blen, "EOF");
    else
        snprintf(buf, blen, "%c", (char)t);
    buf[blen - 1] = '\0';
    return buf;
}

void tokenprint(const char *src, const Token tok) {
    char buf[TOKLEN];

    switch (tok.type) {
    case TIDENT:
    case TCOMMENT:
    case TNUM:
    case TSTRING:
        printf("%.*s", (int)(tok.range.end - tok.range.start),
               src + tok.range.start);
        break;
    case TERR:
        printf("error");
        break;
    default:
        tokentypestr(tok.type, buf, sizeof(buf));
        printf("%s", buf);
        break;
    }
}
