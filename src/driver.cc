#include "driver.h"
#include "ast.h"
#include "parse.h"
#include "sema.h"

bool Driver::compile() {
  Lexer lexer{source};
  Sema sema{source, errors, beacons};
  Parser parser{lexer, sema};

  auto file = parser.parse();
  if (!errors.empty()) {
    return false;
  }

  setup_builtin(sema);
  if (!check(sema, file)) {
    return false;
  }
  QbeGen c{sema, "out.qbe"};
  c.codegen(file);
  fflush(c.file);

  if (system("qbe -o out.s out.qbe") != 0) {
    return false;
  }
  if (system("gcc -o out out.s") != 0) {
    return false;
  }

  return true;
}
