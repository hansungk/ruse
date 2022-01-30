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
	TAMPERSAND = '&',
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
	TSTRING_,

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

// Keep NEXPR < NDECL < NSTMT
enum NodeKind {
	NFILE,

	NEXPR,
	NLITERAL = NEXPR,
	NIDEXPR,
	NBINEXPR,
	NDEREFEXPR, // *expr
	NREFEXPR, // &expr
	NCALL,
	NMEMBER,

	NDECL,
	NVAR = NDECL,
	NFUNC,
	NSTRUCT,

	NSTMT,
	NEXPRSTMT = NSTMT,
	NASSIGN,
	NBLOCKSTMT,
	NRETURN,
};

typedef struct node Node;
typedef struct Error Error;
typedef struct Parser Parser;

struct node {
	enum NodeKind kind;
	Token tok;         // name of var or func. etc.
	int id;            // scope-unique decl id for codegen
	struct node *decl; // declaration of this value, e.g. original function
	                   // declaration
	struct node *parent;    // for memberexpr
	struct node **args;     // func args
	struct node **children; // func body, struct fields
	struct type *type;      // TODO: should be separate from typeexpr?
	struct node *lhs;
	struct node *rhs; // assign expr
	struct type *rettype;
};

struct Error {
	struct SrcLoc loc;
	char msg[1024];
};

// Source text = ['tok' 'lookahead...' ...unlexed...]
struct Parser {
	Lexer l;                  // lexer driven by this parser
	Token tok;                // current token
	Token *lookahead;         // lookahead tokens
	Error *errors;            // parse errors
	struct node **nodeptrbuf; // pointers to the allocated nodes
};

void parser_from_file(Parser *p, const char *filename);
void parser_from_buf(Parser *p, const char *buf, size_t len);
void parser_cleanup(Parser *p);
struct node *parse(Parser *p);

typedef struct Map Map;

struct Map {
	size_t bucketlen;
	struct Mapkey *buckets;
};

void makemap(struct Map *m);
void freemap(struct Map *map);
int mapput(Map *m, const char *str, void *data);
void *mapget(struct Map *m, const char *str);

enum DeclKind {
	D_NUM,
};

typedef struct type Type;
typedef struct Decl Decl;
typedef struct Scope Scope;
typedef struct Context Context;

enum TypeKind {
	TYVAL,
	TYPTR,
	TYFUNC,
};

struct type {
	enum TypeKind kind;
	struct Token tok;
	struct node **params;
	struct node **members;
	Type *target; // referred type
	Type *rettype;
};

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
	Scope *scope;
	Scope *typescope;
	Error *errors;
	int curr_id; // next scope-unique decl id
	struct Valstack {
		int curr_id; // next id to be pushed to valstack
		int *data;  // stack of the id of expression results
	} valstack;
};

Type *maketype(enum TypeKind kind, Token tok);
void context_init(Context *ctx, Parser *p);
void context_free(Context *ctx);
void check(Context *ctx, struct node *v);
void do_errors(const Error *errors);

void codegen(Context *ctx, struct node *n);

#endif
