#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>

int main(void) {
	struct Parser p;
	struct Context ctx;
	parser_from_file(&p, "test.ruse");
	context_init(&ctx, p.l.src);

	for (;;) {
		struct Val *v = ruse_read(&p);
		if (!v) {
			break;
		}
		ruse_eval(&ctx, v);
		ruse_print(v);
	}

	if (p.tok.type != TOK_EOF) {
		fprintf(stderr, "terminated abnormally\n");
		return 1;
	}

	parser_cleanup(&p);
	return 0;
}
