#ifndef RUSE_H
#define RUSE_H

#include <stddef.h>

#define MAXTOKLEN 64

typedef enum TokenType {
	TOK_EOF = '\0',
	TOK_NEWLINE = '\n',
	TOK_LPAREN = '(',
	TOK_RPAREN = ')',
	TOK_LBRACE = '{',
	TOK_RBRACE = '}',
	TOK_LBRACKET = '[',
	TOK_RBRACKET = ']',
	TOK_LT,
	TOK_GT,
	TOK_DOT = '.',
	TOK_COMMA,
	TOK_COLON,
	TOK_SEMICOLON,
	TOK_QUOTE,
	TOK_DOUBLEQUOTE,
	TOK_BACKTICK,
	TOK_EQUALS,
	TOK_PLUS,
	TOK_MINUS,
	TOK_STAR,
	TOK_AMPERSAND,
	TOK_CARET,
	TOK_TILDE,
	TOK_SLASH,
	TOK_BACKSLASH,
	TOK_PIPE,
	TOK_BANG,
	TOK_QUESTION,
	TOK_AT,
	TOK_HASH,
	TOK_DOLLAR = '$',
	TOK_PERCENT = '%',
	TOK_DASH,
	TOK_ASCII = 256,

	TOK_NUM,
	TOK_IDENT,
	TOK_IDENTCODE,
	TOK_STRING,
	TOK_VERBATIM,
	TOK_ARROW,

	TOK_KEYWORDS,
	TOK_DATE,
	TOK_VERSION,
	TOK_TIMESCALE,
	TOK_COMMENT,
	TOK_SCOPE,
	TOK_BEGIN,
	TOK_FORK,
	TOK_FUNCTION,
	TOK_MODULE,
	TOK_TASK,
	TOK_VAR,
	TOK_WIRE,
	TOK_REG,
	TOK_UPSCOPE,
	TOK_ENDDEFINITIONS,
	TOK_DUMPVARS,
	TOK_END,

	TOK_ERR,
	NUM_TOKENTYPES
} TokenType;

#define TOKLEN 100
extern char *token_names[NUM_TOKENTYPES];

struct token_map {
	const char *text;
	TokenType type;
};

typedef struct {
	size_t start; // start position in the source
	size_t end;   // end position in the source
} SrcRange;

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
typedef struct Token {
	TokenType type;
	SrcRange range;
} Token;

// Represents a unique location (file:line:col) in the source.
typedef struct SrcLoc {
	const char *filename;
	int line;
	int col;
} SrcLoc;

// Lexer state.
typedef struct Lexer {
	Token tok;	    // currently lexed token
	char ch;	    // lookahead character
	long off;	    // lookahead character offset
	long rd_off;	    // next read character offset
	long line_off;	    // current line offset
	size_t *line_offs;  // byte offsets of '\n's
	long start;	    // start position of `tok`
	char filename[256]; // source filename
	char *src;	    // source text
	long srclen;	    // length of src excluding \0
} Lexer;

int lexer_from_file(Lexer *l, const char *filename);
int lexer_from_buf(Lexer *l, const char *buf, size_t len);
void lexer_cleanup(Lexer *l);
int lex_next(Lexer *l);
int lex_next_manual(Lexer *l, enum TokenType t);
char *srclocstr(SrcLoc loc, char *buf, size_t len);
SrcLoc lexer_locate(Lexer *l, size_t pos);
char *tokenstr(Lexer *lex, Token tok, char *buf, size_t len);
char *tokentypestr(enum TokenType t, char *buf, size_t blen);
void token_print(Lexer *l, const Token t);

enum NodeKind {
	ND_FILE,
	ND_DATE,
	ND_VERSION,
	ND_TIMESCALE,
	ND_COMMENT,
	ND_SCOPE,
	ND_VAR,
	ND_VARREF,
        ND_UPSCOPE,
        ND_ENDDEFINITIONS,
};

enum VarKind {
        VAR_WIRE,
        VAR_REG,
};

struct Node {
	enum NodeKind kind;
        enum VarKind var_kind;
	Token tok; // ident_code for ND_VAR
        long size;
        long msb;
        long lsb;
	struct Node **children;
};

// Source text = ['tok' 'lookahead...' ...unlexed...]
struct Parser {
	Lexer l;		  // lexer driven by this parser
	Token tok;		  // current token
	Token *lookahead;	  // lookahead tokens
	struct Node **nodeptrbuf; // pointers to the allocated nodes
};

void parser_from_file(struct Parser *p, const char *filename);
void parser_from_buf(struct Parser *p, const char *buf, size_t len);
void parser_cleanup(struct Parser *p);
struct Node *parse_file(struct Parser *p);

#endif
