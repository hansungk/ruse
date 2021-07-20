#include "driver.h"
#include "ast.h"
#include "parser.h"
#include "sema.h"

bool Driver::compile() {
    Lexer lexer{source};
    Sema sema{source, errors, beacons};
    Parser parser{lexer, sema};

    auto node = parser.parse();
    if (!no_errors()) {
        return false;
    }

    setup_builtin_types(sema);
    if (!typecheck(sema, node)) {
        return false;
    }
    QbeGenerator c{sema.context, "out.qbe"};
    c.codegen(node);
    fflush(c.file);

    if (system("$HOME/build/qbe/bin/qbe -o out.s out.qbe") != 0) {
        return false;
    }
    if (system("gcc -o out out.s") != 0) {
        return false;
    }

    return true;
}
