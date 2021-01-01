#include "ruse.h"
#include <stdio.h>
#include <string.h>

void context_init(struct Context *ctx) {
	memset(ctx, 0, sizeof(struct Context));
}

void ruse_eval(struct Context *ctx, struct Val *n) {
	switch (n->kind) {
	case ND_ATOM:
		if (n->tok.type == TOK_NUM) {
			printf("it's a number\n");
		}
		break;
	default:
		break;
	}
}
