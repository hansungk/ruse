#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "usage: %s filename\n", argv[0]);
		return 1;
	}

	Parser p;
	struct Context ctx;
	parser_from_file(&p, argv[1]);
	context_init(&ctx, &p.l.src);

	struct node *n = parse(&p);
	check(&ctx, n);
	do_errors(&ctx);

	// codegen(&ctx, n);

	context_free(&ctx);
	parser_cleanup(&p);
	return 0;
}
