#include "ast.h"
#include "sema.h"
#include <cassert>
#include <iostream>
#include <sstream>
#include <string_view>

namespace cmp {

int AstNode::indent = 0;

std::string_view AstNode::text(const Sema &sema) {
  return "text() placeholder";
  assert(!"wontfix: AstNodes no longer use text() to get text data");
  return {sema.source.buf.data() + pos, endpos - pos};
}

} // namespace cmp
