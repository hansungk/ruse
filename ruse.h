/* vim: set ft=c: */
#ifndef RUSE_H
#define RUSE_H

#include <stddef.h>

#define TOKLEN 64
#define VALLEN 10

typedef struct source Source;
typedef struct src_range SrcRange;
typedef struct src_loc SrcLoc;

struct source {
	char filename[256]; // source filename
	size_t *line_offs;  // byte offsets of '\n's
	char *buf;          // source text
	long buflen;        // length of src excluding \0
};

struct src_range {
	size_t start; // start position in the source
	size_t end;   // end position in the source
};

// Represents a unique location (file:line:col) in the source.
struct src_loc {
	const char *filename;
	int line;
	int col;
};

enum token_type {
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

struct token_map {
	char *text;
	enum token_type type;
};

typedef struct token Token;
typedef struct lexer Lexer;

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
struct token {
	enum token_type type;
	struct src_range range;
	struct src_loc loc;
	char *name;
};

// Lexer state.
struct lexer {
	struct source src; // program source
	struct token tok;  // currently lexed token
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
char *tokenstr(const char *src, Token tok, char *buf, size_t buflen);
int tokeneq(const char *src, Token t1, Token t2);
char *tokentypestr(enum token_type t, char *buf, size_t buflen);
void tokenprint(const char *src, const Token tok);

// Keep NEXPR < NDECL < NSTMT
enum node_kind {
	NFILE,

	NEXPR,
	NLITERAL = NEXPR,
	NIDEXPR,
	NBINEXPR,
	NDEREFEXPR, // *expr
	NREFEXPR, // &expr
	NCALL,
	NMEMBER,

	NTYPEEXPR,

	NDECL,
	NVARDECL = NDECL,
	NFUNC,
	NSTRUCT,

	NSTMT,
	NEXPRSTMT = NSTMT,
	NASSIGN,
	NBLOCKSTMT,
	NRETURN,
};

typedef struct type Type;
typedef struct node Node;
typedef struct error Error;
typedef struct parser Parser;

enum type_kind {
	TYPE_VAL,
	TYPE_PTR,
	TYPE_FUNC,
};


struct type {
	enum type_kind kind;
	struct token tok; // name of the type (for value types)
	struct node **params;
	struct node **members;
	struct type *target; // referred type
	struct type *rettype;
	size_t size; // memory size in bytes
};

struct node {
	enum node_kind kind;
	Token tok;              // name of var or func. etc.
	int id;                 // scope-unique decl id for codegen
	struct node *decl;      // original declaration of this node
	struct node *parent;    // for memberexpr
	struct node **args;     // func args/params
	struct node **children; // func body, struct fields
	struct node *typeexpr;  // ast node of type specifier
	enum type_kind typekind;
	struct type *type; // type of this node.  This being NULL equals the
	                   // typecheck on this node having failed
	struct node *lhs;
	struct node *rhs;         // ref, assign expr
	struct node *rettypeexpr; // TODO: merge with typeexpr
};

struct error {
	struct src_loc loc;
	char msg[1024];
};

// Source text = ['tok' 'lookahead...' ...unlexed...]
struct parser {
	Lexer l;                  // lexer driven by this parser
	Token tok;                // current token
	Token *lookahead;         // lookahead tokens
	Error *errors;            // parse errors
	struct node **nodeptrbuf; // pointers to the allocated nodes
};

void parser_from_file(struct parser *p, const char *filename);
void parser_from_buf(struct parser *p, const char *buf, size_t len);
void parser_cleanup(struct parser *p);
struct node *parse(struct parser *p);

typedef struct map Map;

struct map {
	size_t bucketlen;
	struct mapkey *buckets;
};

void makemap(struct map *m);
void freemap(struct map *map);
int mapput(Map *m, const char *str, void *data);
void *mapget(struct map *m, const char *str);

enum decl_kind {
	D_NUM,
};

typedef struct decl Decl;
typedef struct scope Scope;
typedef struct context Context;

struct decl {
	enum decl_kind kind;
	Token name;
	double num;
};

struct scope {
	struct map map;
	struct scope *outer;
};

#define VHLEN 10

struct context {
	Source *src;
	Scope *scope;
	Scope *typescope;
	Error *errors;
	int curr_decl_id; // next scope-unique decl id
	struct valstack {
		// value_handle is a handle used for referring to the temporary QBE
		// values that are so far generated.
		struct value_handle {
			enum val_kind {
				VAL_TEMP,
				VAL_ADDR,
			} kind;
			int temp_id;
			int addr_id;
			char name[VHLEN]; // used to hold the un-parsed name of
			                  // the values
			int data_size;
		} *data;
		int curr_temp_id; // next id to be pushed to valstack
	} valstack;
};

Type *maketype(enum type_kind kind, Token tok);
void context_init(struct context *ctx, struct parser *p);
void context_free(struct context *ctx);
void check(struct context *ctx, struct node *v);
int do_errors(const Error *errors);

void codegen(struct context *ctx, struct node *n);

#endif
