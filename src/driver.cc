#include "driver.h"
#include "ast.h"
#include "parse.h"
#include "sema.h"

bool Driver::compile() {
    Lexer lexer{source};
    Sema sema{source, errors, beacons};
    Parser parser{lexer, sema};

    auto node = parser.parse();
    if (!no_errors()) {
        return false;
    }

    setupBuiltinTypes(sema);
    if (!typecheck(sema, node)) {
        return false;
    }
    QbeGenerator c{sema, "out.qbe"};
    c.codegen(node);
    fflush(c.file);

    if (system("qbe -o out.s out.qbe") != 0) {
        return false;
    }
    if (system("gcc -o out out.s") != 0) {
        return false;
    }

    return true;
}
