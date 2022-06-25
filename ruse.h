/* vim: set ft=c: */
#ifndef _RUSE_H
#define _RUSE_H

#include <stddef.h>
#include <stdio.h>

#define TOKLEN 64
#define VALLEN 10
#define QBELEN 10

void fatal(const char *fmt, ...);

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
	TLBRACKET = '[',
	TRBRACKET = ']',
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
	TANY,
	TSTRING_,

	TERR,
	NUM_TOKENTYPES
};

extern char *token_names[NUM_TOKENTYPES];

struct token_map {
	char *text;
	enum token_type type;
};

// Making Tokens store source ranges instead of string memory blocks makes
// passing them around easy.
struct token {
	enum token_type type;
	struct src_range range;
	struct src_loc loc;
	char *name;
};

// struct lexer state.
struct lexer {
	struct source src; // program source
	struct token tok;  // currently lexed token
	char ch;           // next character to start lexing at
	long off;          // byte offset of 'ch'
	long rd_off;       // byte offset of next read position
	long line_off;     // current line offset
	long start;        // start position of 'tok'
};

int lexer_from_file(struct lexer *l, const char *filename);
int lexer_from_buf(struct lexer *l, const char *buf, size_t len);
void lexer_cleanup(struct lexer *l);
struct src_loc locate(struct source *src, size_t pos);
int lex(struct lexer *l);
char *srclocstr(struct src_loc loc, char *buf, size_t len);
char *tokenstr(const char *src, struct token tok, char *buf, size_t buflen);
int tokeneq(const char *src, struct token t1, struct token t2);
char *tokentypestr(enum token_type t, char *buf, size_t buflen);
void tokenprint(const char *src, const struct token tok);

// Keep NEXPR < NDECL < NSTMT
enum node_kind {
	NFILE,

	NEXPR,
	NLITERAL = NEXPR,
	NIDEXPR,
	NBINEXPR,
	NDEREFEXPR, // *expr
	NREFEXPR,   // &expr
	NSUBSCRIPT, // expr[expr]
	NCALL,
	NMEMBER,
	NTYPEEXPR,

	NDECL,
	NVARDECL = NDECL,
	NFUNC,
	NSTRUCT,
	NFIELD,

	NSTMT,
	NEXPRSTMT = NSTMT,
	NASSIGN,
	NBLOCKSTMT,
	NRETURN,
};

enum type_kind {
	TYPE_ATOM,
	TYPE_ARRAY,
	TYPE_POINTER,
	TYPE_FUNC,
};

struct type {
	enum type_kind kind;
	struct token tok; // name of the type (for value types)
	                  // TODO: swap this out with a char *n
	struct type **params;
	struct ast_node **members;
	struct type *base_type; // referred type
	struct type *return_type;
	size_t size; // memory size in bytes
};

struct ast_node {
	enum node_kind kind;
	struct src_loc loc;    // location in source
	struct token tok;      // name of var or func. etc.
	int local_id;          // scope-unique decl id for codegen
	struct scope *scope;   // scope of this function
	struct ast_node *decl; // original declaration of this node
	// Derived declarations from this decl, e.g. members of a struct ('s.a',
	// 's.b' from 's') or an array subscript expression ('a[1]' from 'a').
	struct ast_node **derived_decls;
	union {
		struct ast_call_expr {
			struct ast_node *func;
			struct ast_node **args;
		} call;
		struct ast_file {
			struct ast_node **body;
		} file;
		struct block_stmt {
			struct ast_node **stmts;
		} block;
		struct ast_function {
			struct ast_node **params;
			struct ast_node **stmts;
			struct ast_node *ret_type_expr;
		} func;
		struct ast_struct {
			struct ast_node **fields;
		} struct_;
		struct ast_bin_expr {
			struct ast_node *lhs;
			struct ast_node *rhs;
		} bin;
		struct ast_ref_expr {
			struct ast_node *target;
		} ref;
		struct ast_deref_expr {
			struct ast_node *target;
		} deref;
		struct ast_subscript_expr { // 'arr[index]'
			struct ast_node *array; // 'arr'
			struct ast_node *index; // 'index'
		} subscript;
		struct ast_assign_expr {
			struct ast_node *lhs;
			struct ast_node *init_expr;
		} assign_expr;
		struct ast_return_expr {
			struct ast_node *expr;
		} return_expr;
		struct ast_member_expr { // 'struct.mem'
			struct ast_node *parent;
			int offset;
		} member;
		struct ast_var_decl {
			struct ast_node *init_expr;
			struct ast_node *type_expr;
		} var_decl;
		struct ast_field_decl {
			int offset;
			struct ast_node *type_expr;
		} field;
		struct ast_expr_stmt {
			struct ast_node *expr;
		} expr_stmt;
		struct ast_type_expr {
			struct src_loc loc;
			enum type_kind typekind;
			struct token tok;
			// base type of a pointer, array, etc.
			struct ast_node *base_type;
		} type_expr;
	};
	// Type of this node.  If this is NULL that means the typecheck on this
	// node have failed.
	struct type *type;
};

struct error {
	struct src_loc loc;
	char msg[1024];
};

// struct source text = ['tok' 'lookahead...' ...unlexed...]
struct parser {
	struct lexer l;          // lexer driven by this parser
	struct token tok;        // current token
	struct token *lookahead; // lookahead tokens
	// TODO: merge with context.errors
	struct error *errors;         // parse errors
	struct ast_node **nodeptrbuf; // pointers to the allocated nodes
};

struct ast_node *makefunc(struct parser *p, struct token name);
struct ast_node *maketempdecl(struct parser *p);
void parser_from_file(struct parser *p, const char *filename);
void parser_from_buf(struct parser *p, const char *buf, size_t len);
void parser_cleanup(struct parser *p);
struct ast_node *parse(struct parser *p);

struct map {
	size_t bucketlen;
	struct mapkey *buckets;
};

void makemap(struct map *m);
void freemap(struct map *map);
int mapput(struct map *m, const char *str, void *data);
void *mapget(struct map *m, const char *str);

enum decl_kind {
	D_NUM,
};

struct decl {
	enum decl_kind kind;
	struct token name;
	double num;
};

struct scope {
	struct map map;
	struct scope *outer;
	// The decl that defines this scope, e.g. function, struct, etc.
	struct ast_node *decl;
};

struct context {
	struct source *src;
	struct parser *parser;
	FILE *outfile;
	struct scope *scope;
	struct scope *typescope;
	struct error *errors;
	int curr_decl_id; // next scope-unique decl id
	// A temporary flag that is set when checking the index part of a subscript
	// expression to signal that the type of the index expression should be
	// promoted to the int64 type.
	int prefer_long;
	// Increments at every parsing of a field decl to compute the offset of the
	// field.
	int accum_field_offset;
	struct qbe_valstack {
		// qbeval is used as a handle for referring to the temporary QBE values
		// that are so far generated.
		struct qbe_val {
			enum val_kind {
				VAL_PARAM,
				VAL_TEMP,
				VAL_ADDR,
			} kind;
			// Used to hold the unparsed name of the values, e.g. %.1.
			// This will be lazily generated at push time.
			char text[QBELEN];
			const char *param_name;
			int temp_id;
			int addr_id;
			int data_size;
		} * data;
		int next_temp_id; // next id to be pushed to valstack
	} valstack;
};

void scope_open(struct context *ctx);
void scope_open_with(struct context *ctx, struct scope *scope);
void scope_close(struct context *ctx);
struct type *maketype(enum type_kind kind, struct token tok);
void context_init(struct context *ctx, struct parser *p);
void context_free(struct context *ctx);
struct ast_node *lookup(struct context *ctx, const struct ast_node *n);
void check(struct context *ctx, struct ast_node *n);
int do_errors(const struct error *errors);

void codegen(struct context *ctx, struct ast_node *n);

#endif
