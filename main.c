#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "usage: %s filename\n", argv[0]);
		return 1;
	}

	Parser p;
	parser_from_file(&p, argv[1]);
	struct node *n = parse(&p);

	struct Context ctx;
	context_init(&ctx, &p);
	check(&ctx, n);

	do_errors(ctx.errors);

	codegen(&ctx, n);

	context_free(&ctx);
	parser_cleanup(&p);
	return 0;
}
