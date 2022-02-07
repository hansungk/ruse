#include "ast.h"
#include "sema.h"
#include <cassert>
#include <iostream>
#include <sstream>
#include <string_view>

namespace cmp {

int AstNode::indent = 0;

std::string_view AstNode::text(const Sema &sema) {
    return {sema.source.buf.data() + pos, endpos - pos};
}

} // namespace cmp
