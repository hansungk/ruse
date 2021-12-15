/* vim: set ft=c: */
#ifndef RUSE_H
#define RUSE_H

#include <stddef.h>

#define MAXTOKLEN 64

typedef struct Source Source;
typedef struct SrcRange SrcRange;
typedef struct SrcLoc SrcLoc;

struct Source {
    char filename[256]; // source filename
    size_t *line_offs;  // byte offsets of '\n's
    char *src;          // source text
    long srclen;        // length of src excluding \0
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
    TOK_LBRACE = '{',
    TOK_RBRACE = '}',
    TOK_ASCII = 256,

    TOK_NUM,
    TOK_IDENT,
    TOK_STRING,
    TOK_COMMENT,
    TOK_ARROW,

    TOK_KEYWORDS,
    TOK_VAR,
    TOK_CONST,
    TOK_FUNC,
    TOK_STRUCT,
    TOK_RETURN,
	TOK_INT,

    TOK_ERR,
    NUM_TOKENTYPES
};

extern char *token_names[NUM_TOKENTYPES];

struct TokenMap {
    const char *text;
    enum TokenType type;
};

typedef struct Token Token;
typedef struct Lexer Lexer;

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
struct Token {
    enum TokenType type;
    struct SrcRange range;
};

// Lexer state.
struct Lexer {
    Source src; // program source
    Token tok;  // currently lexed token
    char ch;           // lookahead character
    long off;          // lookahead character offset
    long rd_off;       // next read character offset
    long line_off;     // current line offset
    long start;        // start position of `tok`
};

int lexer_from_file(Lexer *l, const char *filename);
int lexer_from_buf(Lexer *l, const char *buf, size_t len);
void lexer_cleanup(Lexer *l);
SrcLoc locate(Source *src, size_t pos);
int lex(Lexer *l);
char *srclocstr(SrcLoc loc, char *buf, size_t len);
char *tokenstr(const char *src, Token tok, char *buf, size_t blen);
int tokeneq(const char *src, Token t1, Token t2);
char *tokentypestr(enum TokenType t, char *buf, size_t blen);
void tokenprint(const char *src, const Token tok);

// Keep NEXPR < NTYPEEXPR < NDECL < NSTMT
enum NodeKind {
	NFILE,
	NFUNC,
	NSTRUCT,
	NEXPR,
	NLITERAL = NEXPR,
	NIDEXPR,
	NBINEXPR,
	NTYPEEXPR,
	NDECL,
	NSTMT,
	NEXPRSTMT = NSTMT,
	NASSIGN,
	NRETURN,
};

typedef struct Node Node;
typedef struct Parser Parser;

struct Node {
    enum NodeKind kind;
    Token tok;
    long num;
    struct Decl *decl;
    struct Node **children;
    struct Node *lhs;
    struct Node *rhs;
    // functions
    struct Node *rettypeexpr;
};

// Source text = ['tok' 'lookahead...' ...unlexed...]
struct Parser {
    Lexer l;           // lexer driven by this parser
    Token tok;         // current token
    Token *lookahead;  // lookahead tokens
    struct Node **nodeptrbuf; // pointers to the allocated nodes
};

void parser_from_file(Parser *p, const char *filename);
void parser_from_buf(Parser *p, const char *buf, size_t len);
void parser_cleanup(Parser *p);
struct Node *parse(Parser *p);

enum DeclKind {
    DCL_NUM,
};

typedef struct Decl Decl;
typedef struct Context Context;

struct Decl {
    enum DeclKind kind;
    Token name;
    double num;
};

struct Map {
    Token name;
    struct Decl *decl;
};

struct Scope {
    struct Map *tab;
    struct Scope *outer;
};

struct Context {
    Source *src;
    struct Scope *scope;
    struct Valstack {
        int curr_id; // next id to be pushed to valstack
        int *stack;  // stack of the id of expression results
    } valstack;
};

void context_init(struct Context *ctx, Source *src);
void context_free(struct Context *ctx);
void eval(struct Context *ctx, struct Node *v);

void codegen(struct Context *ctx, struct Node *n);

#endif
