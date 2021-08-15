#include "sema.h"
#include "fmt/core.h"
#include "parser.h"
#include "source.h"
#include "types.h"
#include <cassert>
#include <cstdarg>

#define BUFSIZE 1024

#define guard(expr)                                                                                \
    if (!(expr)) {                                                                                 \
        return false;                                                                              \
    }

using namespace cmp;

template <typename... Args> static bool error(SourceLoc loc, Args &&...args) {
    auto message = fmt::format(std::forward<Args>(args)...);
    fmt::print(stderr, "{}:{}:{}: error: {}\n", loc.filename, loc.line, loc.col,
               message);
    return false;
    // Not exiting here makes the compiler go as far as it can and report all of
    // the errors it encounters.
    // exit(EXIT_FAILURE);
}

Type *make_builtin_type(Sema &sema, Name *n) {
    Type *t = new Type(n);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_value_type(Sema &sema, Name *n, Decl *decl) {
    Type *t = new Type(TypeKind::value, n, decl);
    sema.type_pool.push_back(t);
    return t;
}

Type *make_ref_type(Sema &sema, Name *name, TypeKind ptr_kind,
                    Type *referee_type) {
    Type *t = new Type(name, ptr_kind, referee_type);
    // assumes pointers are always 8 bytes
    t->size = 8;
    sema.type_pool.push_back(t);
    return t;
}

Type *push_builtin_type_from_name(Sema &s, const std::string &str) {
    Name *name = s.name_table.pushlen(str.data(), str.length());
    auto struct_decl =
        s.make_node<StructDecl>(name, std::vector<FieldDecl *>() /* FIXME */);
    struct_decl->type = make_builtin_type(s, name);
    s.decl_table.insert(name, struct_decl);
    return struct_decl->type;
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void cmp::setup_builtin_types(Sema &s) {
    s.context.void_type = push_builtin_type_from_name(s, "void");
    s.context.int_type = push_builtin_type_from_name(s, "int");
    s.context.int_type->size = 4;
    s.context.char_type = push_builtin_type_from_name(s, "char");
    s.context.char_type->size = 1;
    s.context.string_type = push_builtin_type_from_name(s, "string");
}

Sema::~Sema() {
    for (auto t : type_pool) {
        delete t;
    }
    for (auto lt : lifetime_pool) {
        delete lt;
    }
    for (auto b : basic_block_pool) {
        delete b;
    }
}

void Sema::scope_open() {
    decl_table.scope_open();
    type_table.scope_open();
    lifetime_table.scope_open();
    borrow_table.scope_open();
}

void Sema::scope_close() {
    decl_table.scope_close();
    type_table.scope_close();
    lifetime_table.scope_close();
    borrow_table.scope_close();
}

bool Type::is_struct() const {
    return kind == TypeKind::value && origin_decl && origin_decl->kind == Decl::struct_;
}

bool Type::is_pointer() const {
    return kind == TypeKind::ref || kind == TypeKind::var_ref;
}

bool Type::is_builtin(Sema &sema) const {
    return this == sema.context.int_type || this == sema.context.char_type ||
           this == sema.context.void_type || this == sema.context.string_type;
}

namespace {

bool is_lvalue(const Expr *e) {
    // Determine lvalue-ness by the expression kind.
    switch (e->kind) {
    case Expr::decl_ref:
    case Expr::member:
    case Expr::unary:
        if (e->decl && e->decl->kind == Decl::var) {
            return true;
        }
        break;
    default:
        break;
    }

    return false;
}

// Declare a `decl` that has `name` in the current scope.
// Returns true if success; otherwise (e.g.  redeclaration), return false and
// do error handling.
bool declare(Sema &sema, Name *name, Decl *decl) {
    auto found = sema.decl_table.find(name);
    if (found && found->value->kind == decl->kind &&
        found->scope_level == sema.decl_table.curr_scope_level) {
        return error(decl->loc, "redefinition of '{}'", name->text);
    }

    sema.decl_table.insert(name, decl);
    return true;
}

bool declare_in_struct(StructDecl *struct_decl, Name *name, Decl *decl) {
    auto found = struct_decl->decl_table.find(name);
    if (found && found->value->kind == decl->kind &&
        found->scope_level == struct_decl->decl_table.curr_scope_level) {
        return error(decl->loc, "redefinition of '{}' inside struct {}", name->text,
                     struct_decl->name->text);
    }

    struct_decl->decl_table.insert(name, decl);
    return true;
}

// Get or construct a derived type with kind `kind`, from a given type.
//
// Derived types are only present in the type table if they occur in the source
// code.  Trying to push them every time we see one is sufficient to keep this
// invariant.
Type *get_derived_type(Sema &sema, TypeKind kind, Type *type) {
    Name *name = name_of_derived_type(sema.name_table, kind, type->name);
    if (auto found = sema.type_table.find(name)) {
        return found->value;
    } else {
        Type *derived = make_ref_type(sema, name, kind, type);
        return *sema.type_table.insert(name, derived);
    }
}

bool typecheck_assignable(const Type *to, const Type *from) {
    // TODO: Typecheck assignment rules so far:
    //
    // 1. Pointer <- mutable pointer.
    // 2. Exact same match.

    // Allow promotion from mutable to immutable pointer.
    if (to->kind == TypeKind::ref && from->is_pointer()) {
        // NOTE: this may be related to 'unification'. Ref:
        // http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
        return typecheck_assignable(to->referee_type, from->referee_type);
    }
    return to == from;
}

bool typecheck_expr(Sema &sema, Expr *e);
bool typecheck_stmt(Sema &sema, Stmt *s);
bool typecheck_decl(Sema &sema, Decl *d);

bool typecheck_unary_expr(Sema &sema, UnaryExpr *u) {
    switch (u->kind) {
    case UnaryExpr::paren:
        if (!typecheck_expr(sema, u->operand))
            return false;
        u->type = u->operand->type;
        break;
    case UnaryExpr::deref: {
        if (!typecheck_expr(sema, u->operand))
            return false;

        if (!u->operand->type->is_pointer()) {
            return error(u->loc, "dereferenced a non-pointer type '{}'",
                  u->operand->type->name->text);
        }
        u->type = u->operand->type->referee_type;

        // Bind a temporary VarDecl to this deref expression that respects the
        // mutability of the reference type.  For example,
        //
        //     let v: var &int = ...
        //     *v = 3
        //
        // The '*v' here has to have a valid VarDecl with 'mut' as true.
        bool mut = (u->operand->type->kind == TypeKind::var_ref);
        u->decl = sema.make_node<VarDecl>(nullptr, u->type, mut);

        // We still need to do typecheck on this temporary decl to e.g. bind
        // new decls for all struct children.
        typecheck_decl(sema, u->decl);

        // Temporary VarDecls are not pushed to the scoped decl table, because
        // they are not meant to be accessible from other source locations.
        // Therefore they don't need to have a name.

        break;
    }
    case UnaryExpr::var_ref:
    case UnaryExpr::ref: {
        if (!typecheck_expr(sema, u->operand))
            return false;

        // Prohibit taking address of an rvalue.
        if (!is_lvalue(u->operand)) {
            return error(u->loc, "cannot take address of an rvalue");
        }

        // TODO: Prohibit mutable reference of an immutable variable.

        auto type_kind = (u->kind == UnaryExpr::var_ref) ? TypeKind::var_ref
                                                             : TypeKind::ref;
        u->type = get_derived_type(sema, type_kind, u->operand->type);
        break;
    }
    default:
        assert(!"unknown unary expr kind");
    }

    return true;
}

Name *name_of_member_expr(Sema &sema, MemberExpr *mem) {
    auto text = std::string{mem->parent_expr->decl->name->text} + "." +
                mem->member_name->text;
    return sema.name_table.push(text.c_str());
}

bool typecheck_expr(Sema &sema, Expr *e) {
    switch (e->kind) {
    case Expr::integer_literal: {
        static_cast<IntegerLiteral *>(e)->type = sema.context.int_type;
        break;
    }
    case Expr::string_literal: {
        static_cast<StringLiteral *>(e)->type = sema.context.string_type;
        break;
    }
    case Expr::decl_ref: {
        auto de = static_cast<DeclRefExpr *>(e);
        auto sym = sema.decl_table.find(de->name);
        if (!sym) {
            return error(de->loc, "undeclared identifier '{}'", de->name->text);
        }
        de->decl = sym->value;
        assert(de->decl);
        de->type = de->decl->type;
        break;
    }
    case Expr::call: {
        auto c = static_cast<CallExpr *>(e);
        if (c->kind != CallExpr::func) {
            assert(!"not implemented");
        }

        auto sym = sema.decl_table.find(c->func_name);
        if (!sym) {
            return error(c->loc, "undeclared function '{}'",
                         c->func_name->text);
        }
        c->callee_decl = sym->value;

        // assumes callee_decl is a FuncDecl
        auto func_decl = static_cast<FuncDecl *>(c->callee_decl);
        c->type = func_decl->rettype;

        if (c->args.size() != func_decl->params.size()) {
            return error(c->loc,
                         "function '{}' accepts {} arguments, but {} are given",
                         func_decl->name->text, func_decl->params.size(),
                         c->args.size());
        }

        for (size_t i = 0; i < c->args.size(); i++) {
            if (!typecheck_expr(sema, c->args[i]))
                return false;

            if (!typecheck_assignable(func_decl->params[i]->type,
                                      c->args[i]->type)) {
                auto suffix = (i == 0)   ? "st"
                              : (i == 1) ? "nd"
                              : (i == 2) ? "rd"
                                         : "th";
                return error(c->args[i]->loc,
                             "type mismatch for {}{} argument: cannot assign "
                             "'{}' type to '{}'",
                             i + 1, suffix, c->args[i]->type->name->text,
                             func_decl->params[i]->type->name->text);
            }
        }

        break;
    }
    case Expr::struct_def: {
        auto sde = static_cast<StructDefExpr *>(e);
        if (!typecheck_expr(sema, sde->name_expr))
            return false;

        // @Cleanup: It doesn't make sense that 'name_expr' should have a type,
        // but then we need this kludgy error check below. I think eventually we
        // should rather have it just be a Name and do a lookup on it.
        if (!sde->name_expr->decl || !sde->name_expr->decl->type)
            return false;

        Type *struct_type = sde->name_expr->decl->type;
        if (!struct_type->is_struct()) {
            return error(sde->name_expr->loc, "type '{}' is not a struct",
                         struct_type->name->text);
        }
        for (auto &term : sde->terms) {
            FieldDecl *matched_field = nullptr;
            for (auto field :
                 static_cast<StructDecl *>(sde->name_expr->decl)->fields) {
                if (term.name == field->name) {
                    matched_field = field;
                    term.field_decl = matched_field;
                    break;
                }
            }
            if (!matched_field) {
                return error(sde->loc, "unknown field '{}' in struct '{}'",
                             term.name->text, struct_type->name->text);
            }

            if (!typecheck_expr(sema, term.initexpr))
                return false;

            if (!typecheck_assignable(matched_field->type,
                                      term.initexpr->type)) {
                return error(term.initexpr->loc,
                             "cannot assign '{}' type to '{}'",
                             term.initexpr->type->name->text,
                             matched_field->type->name->text);
            }
        }

        sde->type = struct_type;
        break;
    }
    case Expr::member: {
        auto mem = static_cast<MemberExpr *>(e);
        if (!typecheck_expr(sema, mem->parent_expr))
            return false;

        auto parent_type = mem->parent_expr->type;
        Decl *parent_type_decl = nullptr;
        Name *reported_name = nullptr;
        if (parent_type->is_pointer()) {
            reported_name = parent_type->referee_type->name;
            if (parent_type->referee_type->is_struct()) {
                parent_type_decl = parent_type->referee_type->origin_decl;

                // How do we get the VarDecl of the target of the pointer?
                // Best way to do this would be to rewrite the AST to (*p).mem.
                // Then, a second sema pass would bind a temporary VarDecl to
                // (*p) without any further modification.
                auto new_parent = sema.make_node_range<UnaryExpr>(
                    {mem->parent_expr->pos, mem->parent_expr->endpos},
                    UnaryExpr::deref, mem->parent_expr);
                mem->parent_expr = new_parent;

                // Redo with the rewritten node.
                return typecheck_expr(sema, mem);
            }
        }

        reported_name = parent_type->name;
        if (parent_type->is_struct()) {
            parent_type_decl = parent_type->origin_decl;
        }

        if (!parent_type_decl) {
            assert(reported_name);
            return error(mem->parent_expr->loc, "type '{}' is not a struct",
                         reported_name->text);
        }

        FieldDecl *matched_field = nullptr;
        for (auto field : static_cast<StructDecl *>(parent_type_decl)->fields) {
            if (mem->member_name == field->name) {
                matched_field = field;
                break;
            }
        }
        if (!matched_field) {
            return error(mem->loc, "unknown field '{}' in struct '{}'",
                         mem->member_name->text, parent_type->name->text);
        }

        // For querying offsets in the struct later.
        mem->field_decl = matched_field;

        // If parent is an lvalue, its child is also an lvalue.  So bind a new
        // VarDecl to this MemberExpr as well.

        // At this point, parent_expr is either a struct or a pointer to
        // struct.

        if (mem->parent_expr->decl) {
            assert(mem->parent_expr->decl->kind == Decl::var);
            auto parent_var_decl =
                static_cast<VarDecl *>(mem->parent_expr->decl);
            assert(!parent_var_decl->children.empty());
            for (auto child : parent_var_decl->children) {
                if (child->name == mem->member_name) {
                    mem->decl = child;
                    break;
                }
            }
            assert(mem->decl && "struct member failed to namebind");
        }

        assert(matched_field->type);
        mem->type = matched_field->type;
        break;
    }
    case Expr::unary:
        return typecheck_unary_expr(sema, static_cast<UnaryExpr *>(e));
    case Expr::binary: {
        auto b = static_cast<BinaryExpr *>(e);
        if (!typecheck_expr(sema, b->lhs))
            return false;
        if (!typecheck_expr(sema, b->rhs))
            return false;

        auto lhs_type = b->lhs->type;
        auto rhs_type = b->rhs->type;

        if (lhs_type != rhs_type) {
            return error(b->loc,
                         "incompatible binary op with type '{}' and '{}'",
                         lhs_type->name->text, rhs_type->name->text);
        }

        b->type = lhs_type;

        break;
    }
    case Expr::type_: {
        auto t = static_cast<TypeExpr *>(e);

        // Namebinding for TypeExprs only include linking existing Decls to the
        // type names used in the expression, not declaring new ones.  The
        // declaration would be done when visiting VarDecls and StructDecls,
        // etc.

        if (t->subexpr) {
            if (!typecheck_expr(sema, t->subexpr))
                return false;
        }

        if (t->kind == TypeKind::value) {
            // This is the very first point a new value type is encountered.
            auto sym = sema.decl_table.find(t->name);
            if (!sym)
                return error(t->loc, "undefined type '{}'", t->name->text);
            t->decl = sym->value;
            assert(t->decl);

            // Builtin types, or user types that have showed up before.
            t->type = t->decl->type;
            assert(t->type &&
                   "type not resolved after visiting corresponding *Decl");
        } else if (t->kind == TypeKind::ref || t->kind == TypeKind::var_ref ||
                   t->kind == TypeKind::ptr) {
            t->type = get_derived_type(sema, t->kind, t->subexpr->type);
        } else {
            assert(!"unknown type kind");
        }
        break;
    }
    default:
        assert(!"unknown expr kind");
    }

    assert(e->type);

    // No more work is supposed to be done here.
    return true;
}

bool typecheck_stmt(Sema &sema, Stmt *s) {
    switch (s->kind) {
    case Stmt::expr:
        return typecheck_expr(sema, static_cast<ExprStmt *>(s)->expr);
    case Stmt::decl:
        return typecheck_decl(sema, static_cast<DeclStmt *>(s)->decl);
    case Stmt::assign: {
        auto as = static_cast<AssignStmt *>(s);
        if (!typecheck_expr(sema, as->rhs))
            return false;
        if (!typecheck_expr(sema, as->lhs))
            return false;

        auto lhs_type = as->lhs->type;
        auto rhs_type = as->rhs->type;

        if (!is_lvalue(as->lhs)) {
            return error(as->loc, "cannot assign to an rvalue");
        }

        if (!typecheck_assignable(lhs_type, rhs_type)) {
            return error(as->loc, "cannot assign '{}' type to '{}'",
                         rhs_type->name->text, lhs_type->name->text);
        }

        break;
    }
    case Stmt::if_: {
        auto if_stmt = static_cast<IfStmt *>(s);
        if (!typecheck_expr(sema, if_stmt->cond))
            return false;

        if (!typecheck_stmt(sema, if_stmt->if_body))
            return false;
        if (if_stmt->else_if_stmt) {
            if (!typecheck_stmt(sema, if_stmt->else_if_stmt))
                return false;
        } else if (if_stmt->else_body) {
            if (!typecheck_stmt(sema, if_stmt->else_body))
                return false;
        }
        break;
    }
    case Stmt::return_: {
        auto r = static_cast<ReturnStmt *>(s);
        if (!r->expr)
            break;
        if (!typecheck_expr(sema, r->expr))
            return false;

        assert(!sema.context.func_stack.empty());
        auto current_func = sema.context.func_stack.back();
        if (r->expr->type != current_func->rettype) {
            if (current_func->rettype == sema.context.void_type) {
                return error(
                    r->expr->loc,
                    "tried to return a value from a void function '{}'",
                    current_func->name->text);
            }
            return error(
                r->expr->loc,
                "tried to return '{}' from function '{}', which returns '{}'",
                r->expr->type->name->text, current_func->name->text,
                current_func->rettype->name->text);
        }

        break;
    }
    case Stmt::compound: {
        bool success = true;
        sema.scope_open();
        for (auto stmt : static_cast<CompoundStmt *>(s)->stmts) {
            if (!typecheck_stmt(sema, stmt)) {
                success = false;
            }
        }
        sema.scope_close();
        return success;
    }
    default:
        assert(!"unknown stmt kind");
    }

    return true;
}

VarDecl *instantiate_field(Sema &sema, VarDecl *parent, Name *name,
                                  Type *type) {
    auto field = sema.make_node<VarDecl>(name, type, parent->mut);
    // field->parent = v;
    parent->children.push_back(field);
    return field;
}

bool typecheck_decl(Sema &sema, Decl *d) {
    switch (d->kind) {
    case Decl::var: {
        auto v = static_cast<VarDecl *>(d);
        if (!declare(sema, v->name, v))
            return false;
        if (v->assign_expr) {
            if (!typecheck_expr(sema, v->assign_expr))
                return false;
            v->type = v->assign_expr->type;
        } else if (v->type_expr) {
            if (!typecheck_expr(sema, v->type_expr))
                return false;
            v->type = v->type_expr->type;
        }

        // For struct-typed VarDecls, instantiate all of its fields.
        if (v->type->is_struct()) {
            auto struct_decl = static_cast<StructDecl *>(v->type->origin_decl);
            for (auto field : struct_decl->fields) {
                instantiate_field(sema, v, field->name, field->type);
                // FIXME: should we typecheck_decl() children here?
            }
        }

        break;
    }
    case Decl::func: {
        auto f = static_cast<FuncDecl *>(d);

        sema.decl_table.scope_open();

        // Struct methods.
        if (f->struct_param) {
            // This will declare() the param, so that is why we set up a new
            // scope before to prevent all methods from conflicting each other.
            guard(typecheck_decl(sema, f->struct_param));

            if (!f->struct_param->type->is_struct()) {
                return error(f->struct_param->type_expr->loc,
                             "cannot declare a method for '{}' which is not a struct",
                             f->struct_param->type->name->text);
            }
            // TODO: declare the param as well
            auto target_struct_decl = static_cast<StructDecl *>(f->struct_param->type->origin_decl);
            guard(declare_in_struct(target_struct_decl, f->name, f));
        }
        // Freestanding functions.
        else {
            guard(declare(sema, f->name, f));
        }

        if (f->rettypeexpr) {
            if (!typecheck_expr(sema, f->rettypeexpr))
                return false;
            f->rettype = f->rettypeexpr->type;
        } else {
            f->rettype = sema.context.void_type;
        }

        for (auto param : f->params) {
            // typecheck_decl() will declare the params inside them as well.
            if (!typecheck_decl(sema, param))
                return false;
        }

        sema.context.func_stack.push_back(f);

        bool success = true;
        for (auto stmt : f->body->stmts) {
            if (!typecheck_stmt(sema, stmt)) {
                success = false;
            }
        }

        sema.context.func_stack.pop_back();

        sema.decl_table.scope_close();

        return success;
    }
    case Decl::field: {
        auto f = static_cast<FieldDecl *>(d);
        // field redeclaration check
        if (!declare(sema, f->name, f))
            return false;

        if (!typecheck_expr(sema, f->type_expr))
            return false;
        f->type = f->type_expr->type;
        break;
    }
    case Decl::struct_: {
        auto s = static_cast<StructDecl *>(d);
        if (!declare(sema, s->name, s))
            return false;

        s->type = make_value_type(sema, s->name, s);

        sema.decl_table.scope_open();
        bool success = true;
        for (auto field : s->fields) {
            if (!typecheck_decl(sema, field)) {
                success = false;
            }
        }
        sema.decl_table.scope_close();
        return success;
    }
    default:
        assert(!"unknown decl kind");
    }

    return true;
}

} // namespace

bool cmp::typecheck(Sema &sema, AstNode *n) {
    switch (n->kind) {
    case AstNode::file: {
        bool success = true;
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            if (!typecheck(sema, toplevel)) {
                success = false;
            }
        }
        return success;
    }
    case AstNode::stmt:
        return typecheck_stmt(sema, static_cast<Stmt *>(n));
    case AstNode::decl:
        return typecheck_decl(sema, static_cast<Decl *>(n));
    default:
        assert(!"unknown ast kind");
    }

    return true;
}

std::string abityStr(const Type *type) {
    std::string s;
    if (type->builtin) {
        // TODO: "l", "s", "d", ...
        s = "w";
    } else {
        s = std::string{":"} + type->name->text;
    }
    return s;
}

// 'value' denotes whether the caller that contains the use of this expression
// requires the actual value of it, or just the address (for lvalues).  If
// 'value' is true, a handle for the generated value is placed on the valstack
// top.
void QbeGenerator::codegen_expr_explicit(Expr *e, bool value) {
    switch (e->kind) {
    case Expr::integer_literal:
        emit("%_{} =w add 0, {}", valstack.next_id,
                     static_cast<IntegerLiteral *>(e)->value);
        annotate("{}: integer literal", e->loc.line);
        valstack.push_temp_value();
        break;
    case Expr::string_literal:
        assert(!"not implemented");
        break;
    case Expr::decl_ref: {
        auto dre = static_cast<DeclRefExpr *>(e);

        // The 'a' in this expression generates a memory load from a stack
        // address:
        //   ... = a
        // But these do not:
        //   ... = a.mem
        //   ... = *a
        //   ... = a[3]
        //
        // Whether a load should be generated or not is determined at the use of
        // the expression.  When we reach the statement that actually uses an
        // expression, we could query for the type of the expression, and only
        // generate load for lvalues or something.  All we have to do for each
        // expression is to get the address ready (if it has one) on the
        // valstack.
        //
        // However, this way we have to do the load/no-load check for all
        // possible positions that can use an expression and it could complicate
        // things.  Maybe just paying the price of generating unused loads and
        // pushing the actual value of the expression on the valstack could be
        // much simpler implementation-wise.
        //
        // Outputting values when the expression is converted from an lvalue to
        // an rvalue also sounds good, but this doesn't work considering that
        // expressions may be used without being converted to rvalues in
        // advance.
        if (dre->decl->kind == Decl::var) {
            auto var = static_cast<VarDecl *>(dre->decl);

            valstack.push_address_explicit(var->frame_local_id);

            if (value) {
                if (dre->type->size > 8) {
                    // FIXME: assumes this is a stack-allocated struct
                    assert(!dre->type->builtin);
                    assert(dre->type->origin_decl->kind == Decl::struct_);
                } else if (dre->type->size == 8) {
                    emit("%_{} =l loadl {}", valstack.next_id,
                         valstack.pop().format());
                    annotate("{}: load {}", dre->loc.line, dre->text(sema));
                    valstack.push_temp_value();
                } else if (dre->type->size == 4) {
                    emit("%_{} =w loadw {}", valstack.next_id,
                         valstack.pop().format());
                    annotate("{}: load {}", dre->loc.line, dre->text(sema));
                    valstack.push_temp_value();
                } else {
                    assert(!"unknown alignment");
                }
            }
        } else {
            assert(!"not implemented");
        }
        break;
    }
    case Expr::call: {
        auto c = static_cast<CallExpr *>(e);

        assert(c->callee_decl->kind == Decl::func);
        auto func_decl = static_cast<FuncDecl *>(c->callee_decl);

        // codegen arguments first
        std::vector<Value> generated_args;
        for (auto arg : c->args) {
            codegen_expr_explicit(arg, true);
            generated_args.push_back(valstack.pop());
        }

        if (func_decl->rettypeexpr) {
            emit("%_{} ={} call ${}(", valstack.next_id,
                          abityStr(func_decl->rettype), c->func_name->text);

            for (size_t i = 0; i < c->args.size(); i++) {
                emit_same_line("{} {}, ", abityStr(c->args[i]->type),
                       generated_args[i].format());
            }

            emit_same_line(")");

            valstack.push_temp_value();
        } else {
            emit("call ${}(", c->func_name->text);

            // @Copypaste from above
            for (size_t i = 0; i < c->args.size(); i++) {
                emit_same_line("{} {}, ", abityStr(c->args[i]->type),
                       generated_args[i].format());
            }

            emit_same_line(")");

            // Don't push to valstack here; that the caller doesn't erroneously
            // pop afterwards should have been checked by the semantic phase.
        }
        break;
    }
    case Expr::struct_def: {
        auto sde = static_cast<StructDefExpr *>(e);

        // TODO: document why we alloc here
        auto id = emit_stack_alloc(sde->type, sde->loc.line, sde->text(sema));

        // Don't assume anything and just emit all the value copying of each
        // members here.  This might sound expensive, especially in cases where
        // only part of the struct is actually used (e.g. `S {...}.a`).  But
        // figuring out those cases is really a job to be done at a higher IR,
        // not here.

        for (auto term : sde->terms) {
            // Calculate the right offsetted memory location for each
            // member.
            assert(term.field_decl);
            emit("%a{} =l add %a{}, {}", valstack.next_id, id,
                 term.field_decl->offset);
            annotate("{}: offset of {}.{}", sde->loc.line, sde->text(sema),
                     term.name->text);
            valstack.push_address();
            emit_assignment(term.field_decl, term.initexpr);
        }

        // Leave the address in the valstack; this is what will be used in lieu
        // of the actual value in later nodes.
        valstack.push_address_explicit(id);

        break;
    }
    case Expr::member: {
        auto mem = static_cast<MemberExpr *>(e);

        // We can't do complete code generation at this end without recursing
        // into the child LHS expression because of cases like these:
        //
        //   ... = (*p).memb
        //   ... = f().memb
        //
        // So we have to recurse into things and let the children decide
        // whether they want to emit value or address.

        // emit correct address first
        codegen_expr_explicit(mem->parent_expr, false);

        emit("%a{} =l add {}, {}", valstack.next_id,
                      valstack.pop().format(), mem->field_decl->offset);
        annotate("{}: offset of {}", mem->loc.line, mem->text(sema));
        valstack.push_address();

        if (value) {
            // TODO: for struct values?
            emit("%_{} =w loadw {}", valstack.next_id, valstack.pop().format());
            annotate("{}: load {}", mem->loc.line, mem->text(sema));
            valstack.push_temp_value();
        }

        break;
    }
    case Expr::unary: {
        auto ue = static_cast<UnaryExpr *>(e);

        if (ue->kind == UnaryExpr::ref) {
            codegen_expr_explicit(ue->operand, false);
        } else if (ue->kind == UnaryExpr::deref) {
            // Output its value first, which is an address.
            codegen_expr_explicit(ue->operand, true);
            if (value) {
                // If `value` is true, emit another load to get the
                // dereferenced value.
                // FIXME: size?
                emit("%_{} =w loadw {}", valstack.next_id,
                     valstack.pop().format());
                annotate("{}: dereference {}", ue->loc.line,
                         ue->operand->text(sema));
                valstack.push_temp_value();
            }
            // Otherwise, the address that the pointer contains is on the
            // valstack and we're done.
        } else {
            codegen_expr_explicit(ue->operand, value);
        }
        break;
    }
    case Expr::binary: {
        auto binary = static_cast<BinaryExpr *>(e);
        codegen_expr_explicit(binary->lhs, true);
        codegen_expr_explicit(binary->rhs, true);

        const char *op_str = NULL;
        switch (binary->op.kind) {
        case Tok::plus:
            op_str = "add";
            break;
        case Tok::doubleequals:
            // TODO: not "w"
            op_str = "ceqw";
            break;
        case Tok::notequals:
            op_str = "cnew";
            break;
        default:
            assert(!"unknown binary expr kind");
        }
        emit("%_{} =w {} {}, {}", valstack.next_id, op_str,
             valstack.pop().format(), valstack.pop().format());
        annotate("{}: binary op '{}'", binary->loc.line, binary->op.str());
        valstack.push_temp_value();
        break;
    }
    default:
        assert(!"unknown expr kind");
    }
}

void QbeGenerator::codegen_expr(Expr *e) {
    codegen_expr_explicit(e, true);
}

void QbeGenerator::codegen_expr_address(Expr *e) {
    codegen_expr_explicit(e, false);
}

void QbeGenerator::codegen_stmt(Stmt *s) {
    switch (s->kind) {
    case Stmt::expr:
        codegen_expr(static_cast<ExprStmt *>(s)->expr);
        break;
    case Stmt::decl:
        codegen_decl(static_cast<DeclStmt *>(s)->decl);
        break;
    case Stmt::assign: {
        auto as = static_cast<AssignStmt *>(s);

        codegen_expr(as->rhs);
        auto rhs_val_id = valstack.pop().id;

        codegen_expr_address(as->lhs);
        // This assert doesn't work for pointer dereferences: their value is
        // the target address of this assignment, but they are designated as
        // ValueKind::value.
        // assert(valstack.peek().kind == ValueKind::address);
        auto lhs_address = valstack.pop();

        emit("storew %_{}, {}", rhs_val_id, lhs_address.format());
        annotate("{}: assign to {}", as->loc.line, as->lhs->text(sema));
        break;
    }
    case Stmt::return_:
        codegen_expr(static_cast<ReturnStmt *>(s)->expr);

        // Whether the top of the valstack might contain is a value or an
        // address, either is fine, because returning a struct by value
        // (usually) happens by address.
        // assert(valstack.peek().kind == ValueKind::value);

        emit("ret {}", valstack.pop().format());
        // This is here only to make QBE not complain.  In practice, no
        // instructions after this point should be reachable.
        emit_same_line("\n@L{}", label_id);
        label_id++;
        break;
    case Stmt::if_: {
        auto if_stmt = static_cast<IfStmt *>(s);
        auto id = ifelse_label_id;
        ifelse_label_id++;
        codegen_expr(if_stmt->cond);
        emit("jnz {}, @if_{}, @else_{}", valstack.pop().format(),
                      id, id);
        emit_same_line("\n@if_{}", id);
        codegen_stmt(if_stmt->if_body);
        emit("jmp @fi_{}", id);
        emit_same_line("\n@else_{}", id);
        if (if_stmt->else_if_stmt) {
            codegen_stmt(if_stmt->else_if_stmt);
        } else if (if_stmt->else_body) {
            codegen_stmt(if_stmt->else_body);
        }
        emit_same_line("\n@fi_{}", id);
        break;
    }
    case Stmt::compound:
        for (auto s : static_cast<CompoundStmt *>(s)->stmts) {
            codegen_stmt(s);
        }
        break;
    default:
        assert(!"unknown stmt kind");
    }
}

void QbeGenerator::codegen_decl(Decl *d) {
    switch (d->kind) {
    case Decl::var: {
        auto v = static_cast<VarDecl *>(d);

        if (sema.context.func_stack.empty()) {
            assert(!"vardecl outside function?");
        }

        // frame_local_id for vardecls is set on-the-fly during code
        // generation. This is because there are other cases that allocate on
        // the stack (such as returning a large struct by-value) that are not
        // emitted by a vardecl.
        v->frame_local_id = emit_stack_alloc(v->type, v->loc.line, v->name->text);

        if (v->assign_expr) {
            valstack.push_address_explicit(v->frame_local_id);
            emit_assignment(v, v->assign_expr);
        }

        break;
    }
    case Decl::func: {
        auto f = static_cast<FuncDecl *>(d);

        emit_same_line("\nexport function {} ${}(", abityStr(f->rettype),
                     f->name->text);

        for (auto param : f->params) {
            emit_same_line("{} %{}, ", abityStr(param->type), param->name->text);
        }

        emit_same_line(") {{");
        emit_same_line("\n@start");

        sema.context.func_stack.push_back(f);
        {
            QbeGenerator::IndentBlock ib{*this};

            // emit parameters
            for (auto param : f->params) {
                // FIXME: is there a cleaner, centralized way to allocate
                // frame_local_id?
                param->frame_local_id = valstack.next_id;
                valstack.next_id++;
                emit("%a{} =l add 0, %{}", param->frame_local_id,
                              param->name->text);
            }

            for (auto body_stmt : f->body->stmts) {
                codegen_stmt(body_stmt);
            }
            // Analyses in the earlier passes should make sure that this ret is
            // not reachable.  This is only here to make QBE work meanwhile
            // those analyses are not fully implemented yet.
            emit("ret");
        }
        sema.context.func_stack.pop_back();

        emit_same_line("\n}}");
        emit_same_line("\n");
        break;
    }
    case Decl::struct_: {
        auto s = static_cast<StructDecl *>(d);

        long max_field_size = 0;
        for (const auto f : s->fields) {
            if (f->type->size > max_field_size) {
                max_field_size = f->type->size;
            }
        }
        if (max_field_size <= 4) {
            s->alignment = 4;
        } else if (max_field_size <= 8) {
            s->alignment = 8;
        } else {
            assert(!"unimplemented: too large field size for alignment");
        }

        size_t offset = 0;
        for (size_t i = 0; i < s->fields.size(); i++) {
            s->fields[i]->offset = offset;
            offset += s->alignment;
        }
        s->type->size = offset;

        emit("type :{} = {{", s->name->text);
        for (auto field : s->fields) {
            (void)field;
            emit_same_line("w, ");
        }
        emit_same_line("}}");
        emit_same_line("");
        break;
    }
    default:
        assert(!"unknown decl kind");
    }
}

// Emit a memory-to-memory value copy.
// The memory address of the LHS and the value of the RHS are assumed to be
// already pushed onto the valstack.
// These memory copies may be reduced to register operations by QBE.
//
// TODO: what about a = 3?
//
// a = 3
// a = b
// a = S {.a=...}
// S {.a=...}.a
// f(S {.a=...})
void QbeGenerator::emit_assignment(const Decl *lhs, Expr *rhs) {
    codegen_expr(rhs);

    // NOTE: rhs_value might not actually have ValueKind::value, e.g. for
    // structs allocated on the stack.
    auto rhs_value = valstack.pop();
    auto lhs_address = valstack.pop();
    assert(lhs_address.kind == ValueKind::address);

    // For structs, copy every field one by one.
    // XXX: This assumes that any LHS type that is larger than an eightbyte is
    // a struct.
    auto lhs_type = lhs->type;
    if (lhs_type->size > 8) {
        assert(lhs_type->kind == TypeKind::value);
        assert(!lhs_type->builtin);
        assert(lhs_type->origin_decl->kind == Decl::struct_);
        auto struct_decl = static_cast<StructDecl *>(lhs_type->origin_decl);

        for (auto field : struct_decl->fields) {
            auto rhs_text =
                fmt::format("{}.{}", rhs->text(sema), field->name->text);

            // load address from source; calculate address of the field
            emit("%a{} =l add {}, {}", valstack.next_id, rhs_value.format(),
                 field->offset);
            annotate("{}: address of {}", rhs->loc.line, rhs_text);
            valstack.push_address();

            // load value of the field
            if (struct_decl->alignment == 8) {
                emit("%_{} =l loadl {}", valstack.next_id,
                     valstack.pop().format());
            } else if (struct_decl->alignment == 4) {
                emit("%_{} =w loadw {}", valstack.next_id,
                     valstack.pop().format());
            } else {
                assert(!"unknown alignment");
            }
            annotate("{}: load {}", rhs->loc.line, rhs_text);
            valstack.push_temp_value();

            // calculate the address of the LHS field first
            auto lhs_text =
                fmt::format("{}.{}", lhs->name->text, field->name->text);
            auto value_to_be_copied = valstack.pop();
            emit("%a{} =l add {}, {}", valstack.next_id, lhs_address.format(),
                 field->offset);
            annotate("{}: address of {}", rhs->loc.line, lhs_text);
            valstack.push_address();

            // store value to dest address of LHS
            auto lhs_address = valstack.pop();
            if (struct_decl->alignment == 8) {
                emit("storel {}, {}", value_to_be_copied.format(),
                     lhs_address.format());
            } else if (struct_decl->alignment == 4) {
                emit("storew {}, {}", value_to_be_copied.format(),
                     lhs_address.format());
            } else {
                assert(!"unknown alignment");
            }
            annotate("{}: store to {}", rhs->loc.line, lhs_text);
        }
    }
    // For non-struct types, a simple store is enough.
    else if (lhs_type->size == 8) {
        emit("storel {}, {}", rhs_value.format(), lhs_address.format());
    } else if (lhs_type->size == 4) {
        emit("storew {}, {}", rhs_value.format(), lhs_address.format());
    } else {
        assert(!"unknown type size");
    }
    annotate("{}: store to {}", rhs->loc.line, lhs->name->text);
}

// Emit a value by allocating it on the stack memory.  That value will be
// handled via its address.  `line` and `text` are used for annotations.
long QbeGenerator::emit_stack_alloc(const Type *type, size_t line,
                                  std::string_view text) {
    assert(!sema.context.func_stack.empty());
    // auto current_func = context.func_stack.back();
    // long id = current_func->frame_local_id_counter;
    // current_func->frame_local_id_counter++;
    long id = valstack.next_id;
    valstack.next_id++;

    assert(type->size > 0);

    emit("%a{} =l ", id);
    // FIXME: unify 'ptr' and 'ref'
    if (type->kind == TypeKind::ptr || type->kind == TypeKind::ref) {
        // assumes pointers are always 8 bytes
        emit_same_line("alloc8");
    } else if (type->builtin) {
        assert(type->kind != TypeKind::ptr &&
               "ptr & builtin for Types is possible?");
        emit_same_line("alloc4");
    } else {
        assert(type->kind == TypeKind::value);
        assert(type->origin_decl->kind == Decl::struct_ &&
               "non-struct value type?");
        if (static_cast<StructDecl *>(type->origin_decl)->alignment == 4) {
            emit_same_line("alloc4");
        } else {
            emit_same_line("alloc8");
        }
    }
    emit_same_line(" {}", type->size);
    annotate("{}: alloc {}", line, text);

    return id;
}

void QbeGenerator::codegen(AstNode *n) {
    switch (n->kind) {
    case AstNode::file: {
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            codegen(toplevel);
        }
        break;
    }
    case AstNode::stmt:
        codegen_stmt(static_cast<Stmt *>(n));
        break;
    case AstNode::decl:
        codegen_decl(static_cast<Decl *>(n));
        break;
    default:
        assert(!"unknown ast kind");
    }
}
