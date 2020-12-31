#include "ruse.h"
#include <stdio.h>

int main(void) {
	struct Parser p;
	parser_from_file(&p, "test.ruse");
	parse_file(&p);
	parser_cleanup(&p);
	return 0;
}
