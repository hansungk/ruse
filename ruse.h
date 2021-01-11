/* vim: set ft=c: */
#ifndef RUSE_H
#define RUSE_H

#include <stddef.h>

#define MAXTOKLEN 64

struct Source {
	char filename[256]; // source filename
	size_t *line_offs;  // byte offsets of '\n's
	char *src;	    // source text
	long srclen;	    // length of src excluding \0
};

struct SrcRange {
	size_t start; // start position in the source
	size_t end;   // end position in the source
};

// Represents a unique location (file:line:col) in the source.
struct SrcLoc {
	const char *filename;
	int line;
	int col;
};

enum TokenType {
	TOK_EOF = '\0',
	TOK_NEWLINE = '\n',
	TOK_LPAREN = '(',
	TOK_RPAREN = ')',
	TOK_DOT = '.',
	TOK_COLON = ':',
	TOK_EQUAL = '=',
	TOK_PLUS = '+',
	TOK_MINUS = '-',
	TOK_STAR = '*',
	TOK_SLASH = '/',
	TOK_SEMICOLON = ';',
	TOK_QUOTE = '\'',
	TOK_ASCII = 256,

	TOK_NUM,
	TOK_IDENT,
	TOK_STRING,
	TOK_COMMENT,

	TOK_KEYWORDS,
	TOK_VAR,
	TOK_PROC,
	TOK_END,

	TOK_ERR,
	NUM_TOKENTYPES
};

extern char *token_names[NUM_TOKENTYPES];

struct token_map {
	const char *text;
	enum TokenType type;
};

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
struct Token {
	enum TokenType type;
	struct SrcRange range;
};

// Lexer state.
struct Lexer {
	struct Source src;  // program source
	struct Token tok;   // currently lexed token
	char ch;	    // lookahead character
	long off;	    // lookahead character offset
	long rd_off;	    // next read character offset
	long line_off;	    // current line offset
	long start;	    // start position of `tok`
};

int lexer_from_file(struct Lexer *l, const char *filename);
int lexer_from_buf(struct Lexer *l, const char *buf, size_t len);
void lexer_cleanup(struct Lexer *l);
struct SrcLoc locate(struct Source *src, size_t pos);
int lex(struct Lexer *l);
char *srclocstr(struct SrcLoc loc, char *buf, size_t len);
char *tokenstr(const char *src, struct Token tok, char *buf, size_t blen);
int tokeneq(const char *src, struct Token t1, struct Token t2);
char *tokentypestr(enum TokenType t, char *buf, size_t blen);
void tokenprint(const char *src, const struct Token tok);

enum NodeKind {
	ND_FILE,
	ND_FUNC,
	ND_START_EXPR,
	ND_LITERAL,
	ND_IDEXPR,
	ND_BINEXPR,
	ND_END_EXPR,
	ND_DECL,
	ND_ASSIGN,
};

// AST node.
struct Node {
	enum NodeKind kind;
	struct Token tok;
	long num;
	struct Value *val;
	struct Node **children;
	struct Node *lhs;
	struct Node *rhs;
	// functions
	struct Node *rettypeexpr;
	// children for body stmts
};

// Source text = ['tok' 'lookahead...' ...unlexed...]
struct Parser {
	struct Lexer l;		  // lexer driven by this parser
	struct Token tok;	  // current token
	struct Token *lookahead;  // lookahead tokens
	struct Node **nodeptrbuf; // pointers to the allocated nodes
};

void parser_from_file(struct Parser *p, const char *filename);
void parser_from_buf(struct Parser *p, const char *buf, size_t len);
void parser_cleanup(struct Parser *p);
struct Node *parse(struct Parser *p);

enum ValueKind {
	VAL_NUM,
};

struct Value {
	enum ValueKind kind;
	double num;
};

struct Map {
	struct Token name;
	struct Value *val;
};

struct SymbolTable {
	struct Map *tab;
	struct SymbolTable *next; // outer scope
};

struct Context {
	struct Source *src;
	struct SymbolTable *symtab; // symbol table at current scope
};

void context_init(struct Context *ctx, struct Source *src);
void context_free(struct Context *ctx);
void run(struct Context *ctx, struct Node *v);

#endif
