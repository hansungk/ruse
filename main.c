#include "ruse.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "usage: %s filename\n", argv[0]);
		return 1;
	}

	struct parser p;
	parser_from_file(&p, argv[1]);
	struct ast_node *n = parse(&p);

	struct context ctx;
	context_init(&ctx, &p);
	check(&ctx, n);

	if (!do_errors(ctx.errors))
		exit(EXIT_FAILURE);

	codegen(&ctx, n);
	fflush(ctx.outfile);

	// FILE *fp_qbe = popen("qbe out.qbe", "r");
	// if (!fp_qbe) {
	// 	fatal("popen() failed");
	// }
	// FILE *fp_assembly = fopen("out.s", "w");
	// if (!fp_assembly) {
	// 	fatal("fopen() failed");
	// }
	// char buf[1024] = {0};
	// while (fgets(buf, sizeof(buf), fp_qbe)) {
	// 	fwrite(buf, 1, strlen(buf), fp_assembly);
	// }
	// fflush(fp_assembly);

	if (system("qbe out.qbe > out.s") != 0) {
		fatal("system() failed");
	}
	if (system("gcc -o out out.s") != 0) {
		fatal("gcc failed");
	}

	context_free(&ctx);
	parser_cleanup(&p);
	return 0;
}
