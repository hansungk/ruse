#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>

int main(void) {
	struct Parser p;
	parser_from_file(&p, "test.ruse");

	for (;;) {
		struct Node *n = read(&p);
		if (!n) {
			exit(1);
		}
	}
	parser_cleanup(&p);
	return 0;
}
