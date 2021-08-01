#include "ast.h"
#include "sema.h"
#include <cassert>
#include <iostream>
#include <sstream>

namespace cmp {

int AstNode::indent = 0;

std::string AstNode::text(const Sema &sema) {
    return {sema.source.buf.data() + pos, endpos - pos};
}

// Return optional of 'type' member of Decl, or None if this Decl kind doesn't
// have any.
std::optional<Type *> Decl::typemaybe() const {
    if (kind == DeclKind::var) {
        return as<VarDecl>()->type;
    } else if (kind == DeclKind::struct_) {
        return as<StructDecl>()->type;
    } else if (kind == DeclKind::enum_) {
        return as<EnumDecl>()->type;
    } else if (kind == DeclKind::enum_variant) {
        return as<EnumVariantDecl>()->type;
    } else if (kind == DeclKind::func) {
        return {};
    }
    assert(false && "not all decl kinds handled");
}

} // namespace cmp
