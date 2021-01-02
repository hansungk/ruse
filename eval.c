#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void context_init(struct Context *ctx, const char *src) {
	memset(ctx, 0, sizeof(struct Context));
	ctx->src = src;
}

void ruse_eval(struct Context *ctx, struct Val *v) {
	switch (v->kind) {
	case VAL_ATOM:
		if (v->tok.type == TOK_NUM) {
			char *endptr;
			v->kind = VAL_NUM;
			// TODO: floats
			v->num = strtol(ctx->src + v->tok.range.start, &endptr, 10);
			if (endptr != ctx->src + v->tok.range.end) {
				fprintf(stderr, "failed to parse number\n");
				exit(1);
			}
		}
		break;
	default:
		break;
	}
}
