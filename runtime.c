#include "ruse.h"
#include "stretchy_buffer.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void context_init(struct Context *ctx, struct Source *src) {
	memset(ctx, 0, sizeof(struct Context));
	ctx->src = src;
	ctx->symtab = calloc(sizeof(struct SymbolTable), 1);
}

void context_free(struct Context *ctx) {
	for (int i = 0; i < sb_count(ctx->symtab->tab); i++) {
		if (ctx->symtab->tab[i].decl) {
			free(ctx->symtab->tab[i].decl);
		}
	}
	sb_free(ctx->symtab->tab);
	free(ctx->symtab);
}

// TODO: merge this with the one in parse.c
static void error(struct Context *ctx, long loc, const char *fmt, ...) {
	static char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);

	struct SrcLoc srcloc = locate(ctx->src, loc);
	fprintf(stderr, "error in %s:%d:%d:(%ld): %s\n", srcloc.filename,
		srcloc.line, srcloc.col, loc, msg);
	exit(EXIT_FAILURE);
}

// Push a variable to the current scope.  `n` should be a declaration node.
struct Decl *push_var(struct Context *ctx, struct Node *n) {
	struct Decl *val = calloc(sizeof(struct Decl), 1);
	struct Map map = {.name = n->tok, .decl = val};
	sb_push(ctx->symtab->tab, map);
	return val;
}

struct Decl *lookup_var(struct Context *ctx, struct Node *n) {
	for (int i = 0; i < sb_count(ctx->symtab->tab); i++) {
		if (tokeneq(ctx->src->src, n->tok, ctx->symtab->tab[i].name)) {
			return ctx->symtab->tab[i].decl;
		}
	}
	return NULL;
}

static void eval_expr(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_LITERAL:
		break;
	case ND_IDEXPR:
		n->decl = lookup_var(ctx, n);
		if (!n->decl) {
			char buf[MAXTOKLEN];
			tokenstr(ctx->src->src, n->tok, buf, sizeof(buf));
			error(ctx, n->tok.range.start,
			      "undeclared variable '%s'", buf);
		}
		break;
	case ND_BINEXPR:
		eval_expr(ctx, n->lhs);
		eval_expr(ctx, n->rhs);
		break;
	default:
		break;
	}
}

static void emit(const char *fmt, ...) {
	va_list args;

	va_start(args, fmt);
	vprintf(fmt, args);
	va_end(args);
	putchar('\n');
}

void run(struct Context *ctx, struct Node *n) {
	switch (n->kind) {
	case ND_FILE:
		emit("export function w $main() {");
		emit("@start");
		for (int i = 0; i < sb_count(n->children); i++) {
			run(ctx, n->children[i]);
		}
		emit("	ret 10");
		emit("}");
		break;
	case ND_FUNC:
		// TODO: push/pop scope
		for (int i = 0; i < sb_count(n->children); i++) {
			run(ctx, n->children[i]);
		}
		break;
	case ND_DECL:
		push_var(ctx, n);
		break;
	default:
		if (ND_START_EXPR < n->kind && n->kind < ND_END_EXPR) {
			eval_expr(ctx, n);
		}
		break;
	}
}
