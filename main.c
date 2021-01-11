#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>

int main(void) {
	struct Parser p;
	struct Context ctx;
	parser_from_file(&p, "test.ruse");
	context_init(&ctx, &p.l.src);

	struct Node *n = parse(&p);
	if (p.tok.type != TOK_EOF) {
		fprintf(stderr, "terminated abnormally\n");
		return 1;
	}

	eval(&ctx, n);
	codegen(&ctx, n);

	context_free(&ctx);
	parser_cleanup(&p);
	return 0;
}
