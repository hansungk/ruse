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

// Return optional of 'type' member of Decl, or None if this Decl kind doesn't
// have any.
std::optional<Type *> Decl::typemaybe() const {
    if (kind == Decl::var) {
        return as<VarDecl>()->type;
    } else if (kind == Decl::struct_) {
        return as<StructDecl>()->type;
    } else if (kind == Decl::enum_) {
        return as<EnumDecl>()->type;
    } else if (kind == Decl::enum_variant) {
        return as<EnumVariantDecl>()->type;
    } else if (kind == Decl::func) {
        return {};
    }
    assert(false && "not all decl kinds handled");
}

} // namespace cmp
