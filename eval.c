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
			v->kind = VAL_NUM;
			// TODO: floats
			char *endptr;
			v->num = strtol(ctx->src + v->tok.range.start, &endptr, 10);
			if (endptr != ctx->src + v->tok.range.end) {
				fprintf(stderr, "failed to parse number\n");
				exit(1);
			}
		} else if (v->tok.type == TOK_IDENT) {
			printf("todo: ident lookup\n");
		}
		break;
	default:
		break;
	}
}

void ruse_print(struct Val *v) {
	switch (v->kind) {
	case VAL_NUM:
		printf("%ld\n", v->num);
		break;
	case VAL_ATOM:
		printf("atom (todo)\n");
		break;
	case VAL_LIST:
		printf("list (todo)\n");
		break;
	default:
		fprintf(stderr, "don't know how to print\n");
		exit(1);
	}
}
