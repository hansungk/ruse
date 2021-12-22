/* vim: set ft=c: */
#ifndef RUSE_H
#define RUSE_H

#include <stddef.h>

#define TOKLEN 64

typedef struct Source Source;
typedef struct SrcRange SrcRange;
typedef struct SrcLoc SrcLoc;

struct Source {
	char filename[256]; // source filename
	size_t *line_offs;  // byte offsets of '\n's
	char *buf;          // source text
	long buflen;        // length of src excluding \0
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
    TEOF = '\0',
    TNEWLINE = '\n',
    TLPAREN = '(',
    TRPAREN = ')',
    TDOT = '.',
    TCOMMA = ',',
    TCOLON = ':',
    TEQUAL = '=',
    TPLUS = '+',
    TMINUS = '-',
    TSTAR = '*',
    TSLASH = '/',
    TSEMICOLON = ';',
    TQUOTE = '\'',
    TLBRACE = '{',
    TRBRACE = '}',
    TASCII = 256,

    TNUM,
    TIDENT,
    TSTRING,
    TCOMMENT,
    TARROW,

    TKEYWORDS,
    TVAR,
    TCONST,
    TFUNC,
    TSTRUCT,
    TRETURN,
	TINT,

    TERR,
    NUM_TOKENTYPES
};

extern char *token_names[NUM_TOKENTYPES];

struct TokenMap {
    char *text;
    enum TokenType type;
};

typedef struct Token Token;
typedef struct Lexer Lexer;

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
struct Token {
    enum TokenType type;
    struct SrcRange range;
	struct SrcLoc loc;
	char *name;
};

// Lexer state.
struct Lexer {
	struct Source src; // program source
	struct Token tok;  // currently lexed token
	char ch;           // next character to start lexing at
	long off;          // byte offset of 'ch'
	long rd_off;       // byte offset of next read position
	long line_off;     // current line offset
	long start;        // start position of 'tok'
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
	NCALL,
	NMEMBER,
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
    struct Node *parent; // for memberexpr
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

typedef struct Map Map;

struct Map {
	size_t bucketlen;
	struct Mapkey *buckets;
};

void makemap(struct Map *m);
void freemap(struct Map *map);
void mapput(struct Map *m, char *str, void *data);
void *mapget(struct Map *m, char *str);

enum DeclKind {
    D_NUM,
};

typedef struct Decl Decl;
typedef struct Context Context;

struct Decl {
    enum DeclKind kind;
    Token name;
    double num;
};

struct Scope {
    struct Map map;
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
void typecheck(struct Context *ctx, struct Node *v);

void codegen(struct Context *ctx, struct Node *n);

#endif
