/* vim: set ft=c: */
#ifndef RUSE_H
#define RUSE_H

#include <stddef.h>

#define MAXTOKLEN 64

enum TokenType {
	TOK_EOF = '\0',
	TOK_NEWLINE = '\n',
	TOK_LPAREN = '(',
	TOK_RPAREN = ')',
	TOK_SEMICOLON = ';',
	TOK_QUOTE = '\'',
	TOK_ASCII = 256,

	TOK_NUM,
	TOK_IDENT,
	TOK_STRING,
	TOK_COMMENT,

	TOK_KEYWORDS,

	TOK_ERR,
	NUM_TOKENTYPES
};

extern char *token_names[NUM_TOKENTYPES];

struct token_map {
	const char *text;
	enum TokenType type;
};

struct SrcRange {
	size_t start;
	size_t end;
};

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
struct Token {
	enum TokenType type;
	struct SrcRange range;
};

// Represents a unique location (file:line:col) in the source.
struct SrcLoc {
	const char *filename;
	int line;
	int col;
};

// Lexer state.
struct Lexer {
	struct Token tok;   // currently lexed token
	char ch;	    // lookahead character
	long off;	    // lookahead character offset
	long rd_off;	    // next read character offset
	long line_off;	    // current line offset
	size_t *line_offs;  // byte offsets of '\n's
	long start;	    // start position of `tok`
	char filename[256]; // source filename
	char *src;	    // source text
	long srclen;	    // length of src excluding \0
};

int lexer_from_file(struct Lexer *l, const char *filename);
int lexer_from_buf(struct Lexer *l, const char *buf, size_t len);
void lexer_cleanup(struct Lexer *l);
int lex_next(struct Lexer *l);
int lex_next_manual(struct Lexer *l, enum TokenType t);
char *srclocstr(struct SrcLoc loc, char *buf, size_t len);
struct SrcLoc lexer_locate(struct Lexer *l, size_t pos);
char *tokenstr(struct Lexer *lex, struct Token tok, char *buf, size_t len);
char *tokentypestr(enum TokenType t, char *buf, size_t blen);
void tokenprint(const char *src, const struct Token tok);

enum ValKind {
	VAL_ATOM, // unevaluated
	VAL_NUM,
	VAL_LIST,
};

struct Val {
	enum ValKind kind;
	struct Token tok; // unevaluated read string
	long num;
	// TODO: car/cdr
	struct Val **children;
};

// Source text = ['tok' 'lookahead...' ...unlexed...]
struct Parser {
	struct Lexer l;		 // lexer driven by this parser
	struct Token tok;	 // current token
	struct Token *lookahead; // lookahead tokens
	struct Val **valptrbuf;	 // pointers to the allocated nodes
};

void parser_from_file(struct Parser *p, const char *filename);
void parser_from_buf(struct Parser *p, const char *buf, size_t len);
void parser_cleanup(struct Parser *p);
struct Val *ruse_read(struct Parser *p);

struct Context {
	const char *src;
};

void context_init(struct Context *ctx, const char *src);
void ruse_eval(struct Context *ctx, struct Val *n);
void ruse_print(struct Val *v);

#endif
