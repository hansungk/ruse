#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>

int main(void) {
	struct Parser p;
	parser_from_file(&p, "test.ruse");

	for (;;) {
		struct Node *n = ruse_read(&p);
		if (!n) {
			break;
		}
	}
	if (p.tok.type != TOK_EOF) {
		fprintf(stderr, "terminated abnormally\n");
		return 1;
	}

	parser_cleanup(&p);
	return 0;
}
