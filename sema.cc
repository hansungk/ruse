#include "sema.h"
#include "fmt/core.h"
#include "parser.h"
#include "source.h"
#include "types.h"
#include <cassert>
#include <cstdarg>

#define BUFSIZE 1024

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

Type::Type(Name *n, TypeKind k, Type *rt) : kind(k), name(n), referee_type(rt) {
    copyable = k == TypeKind::ref;
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

namespace {

bool is_pointer_type(const Type *ty) {
    return ty->kind == TypeKind::ref || ty->kind == TypeKind::var_ref;
}

bool is_struct_type(const Type *ty) {
    return ty->kind == TypeKind::value && ty->type_decl &&
           ty->type_decl->kind == DeclKind::struct_;
}

bool is_builtin_type(const Type *ty, Sema &sema) {
    return ty == sema.context.int_type || ty == sema.context.char_type ||
        ty == sema.context.void_type || ty == sema.context.string_type;
}

bool is_lvalue(const Expr *e) {
    // Determine lvalue-ness by the expression kind.
    switch (e->kind) {
    case ExprKind::decl_ref:
    case ExprKind::member:
    case ExprKind::unary:
        if (e->decl && e->decl->kind == DeclKind::var) {
            return true;
        }
        break;
    default:
        break;
    }

    return false;
}

// Returns true if success and otherwise (e.g. redeclaration) do error handling.
bool declare(Sema &sema, Name *name, Decl *decl) {
    auto found = sema.decl_table.find(name);
    if (found && found->value->kind == decl->kind &&
        found->scope_level == sema.decl_table.curr_scope_level) {
        return error(decl->loc, "redefinition of '{}'", name->text);
    }

    sema.decl_table.insert(name, decl);
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
    if (to->kind == TypeKind::ref && is_pointer_type(from)) {
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
    case UnaryExprKind::paren:
        if (!typecheck_expr(sema, u->operand))
            return false;
        u->type = u->operand->type;
        break;
    case UnaryExprKind::deref: {
        if (!typecheck_expr(sema, u->operand))
            return false;

        if (!is_pointer_type(u->operand->type)) {
            return error(u->loc, "dereferenced a non-pointer type '{}'",
                  u->operand->type->name->text);
        }
        u->type = u->operand->type->referee_type;

        // Also bind a temporary VarDecl to this expression that respects
        // the mutability of the reference type (TODO).  This way we know if
        // this lvalue is assignable or not. For example,
        //
        //     let v: var &int = ...
        //     *v = 3
        //
        // The '*v' here has to have a valid VarDecl with 'mut' as true.
        bool mut = (u->operand->type->kind == TypeKind::var_ref);
        u->decl = sema.make_node<VarDecl>(nullptr, u->type, mut);

        // Temporary VarDecls are not pushed to the scoped decl table,
        // because they are not meant to be accessed later from another
        // source location.  And therefore they also don't have a name that
        // can be used to query them.
        break;
    }
    case UnaryExprKind::var_ref:
    case UnaryExprKind::ref: {
        if (!typecheck_expr(sema, u->operand))
            return false;

        // Prohibit taking address of an rvalue.
        if (!is_lvalue(u->operand)) {
            return error(u->loc, "cannot take address of an rvalue");
        }

        // TODO: Prohibit mutable reference of an immutable variable.

        auto type_kind = (u->kind == UnaryExprKind::var_ref) ? TypeKind::var_ref
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
    case ExprKind::integer_literal: {
        static_cast<IntegerLiteral *>(e)->type = sema.context.int_type;
        break;
    }
    case ExprKind::string_literal: {
        static_cast<StringLiteral *>(e)->type = sema.context.string_type;
        break;
    }
    case ExprKind::decl_ref: {
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
    case ExprKind::call: {
        auto c = static_cast<CallExpr *>(e);
        if (c->kind != CallExprKind::func) {
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
    case ExprKind::struct_def: {
        auto sde = static_cast<StructDefExpr *>(e);
        if (!typecheck_expr(sema, sde->name_expr))
            return false;

        // @Cleanup: It doesn't make sense that 'name_expr' should have a type,
        // but then we need this kludgy error check below. I think eventually we
        // should rather have it just be a Name and do a lookup on it.
        if (!sde->name_expr->decl || !sde->name_expr->decl->type)
            return false;

        Type *struct_type = sde->name_expr->decl->type;
        if (!is_struct_type(struct_type)) {
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
    case ExprKind::member: {
        auto mem = static_cast<MemberExpr *>(e);
        if (!typecheck_expr(sema, mem->parent_expr))
            return false;

        auto parent_type = mem->parent_expr->type;
        if (!is_struct_type(parent_type)) {
            return error(mem->parent_expr->loc, "type '{}' is not a struct",
                         parent_type->name->text);
        }

        FieldDecl *matched_field = nullptr;
        for (auto field :
             static_cast<StructDecl *>(parent_type->type_decl)->fields) {
            if (mem->member_name == field->name) {
                matched_field = field;
                break;
            }
        }
        if (!matched_field) {
            return error(mem->loc, "unknown field '{}' in struct '{}'",
                         mem->member_name->text, parent_type->name->text);
        }
        // For querying offsets in struct later.
        mem->field_decl = matched_field;

        // If parent is an lvalue, bind a VarDecl to this MemberExpr as well.
        // @Review: We could also name the field vardecls by constructing its
        // name, e.g. "s.mem", but I suspect this would be cleaner.
        if (mem->parent_expr->decl) {
            assert(mem->parent_expr->decl->kind == DeclKind::var);
            auto parent_var_decl =
                static_cast<VarDecl *>(mem->parent_expr->decl);
            for (auto child : parent_var_decl->children) {
                if (child->name == mem->member_name) {
                    mem->decl = child;
                    break;
                }
            }
            // Field match is already checked above so this shouldn't fail.
            assert(mem->decl && "struct member failed to namebind");
        }

        assert(matched_field->type);
        mem->type = matched_field->type;
        break;
    }
    case ExprKind::unary:
        return typecheck_unary_expr(sema, static_cast<UnaryExpr *>(e));
    case ExprKind::binary: {
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
    case ExprKind::type: {
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
    case StmtKind::expr:
        return typecheck_expr(sema, static_cast<ExprStmt *>(s)->expr);
    case StmtKind::decl:
        return typecheck_decl(sema, static_cast<DeclStmt *>(s)->decl);
    case StmtKind::assign: {
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
    case StmtKind::if_: {
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
    case StmtKind::return_: {
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
    case StmtKind::compound: {
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
    case DeclKind::var: {
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

        // For struct types, instantiate all of its fields.
        if (is_struct_type(v->type)) {
            auto struct_decl = static_cast<StructDecl *>(v->type->type_decl);
            for (auto field : struct_decl->fields) {
                instantiate_field(sema, v, field->name, field->type);
                // FIXME: should we typecheck_decl() children here?
            }
        }

        break;
    }
    case DeclKind::func: {
        auto f = static_cast<FuncDecl *>(d);

        if (!declare(sema, f->name, f))
            return false;

        if (f->rettypeexpr) {
            if (!typecheck_expr(sema, f->rettypeexpr))
                return false;
            f->rettype = f->rettypeexpr->type;
        } else {
            f->rettype = sema.context.void_type;
        }

        for (auto param : f->params) {
            if (!typecheck_decl(sema, param))
                return false;
        }

        sema.context.func_stack.push_back(f);
        sema.decl_table.scope_open();

        bool success = true;
        for (auto stmt : f->body->stmts) {
            if (!typecheck_stmt(sema, stmt)) {
                success = false;
            }
        }

        sema.decl_table.scope_close();
        sema.context.func_stack.pop_back();

        return success;
    }
    case DeclKind::field: {
        auto f = static_cast<FieldDecl *>(d);
        // field redeclaration check
        if (!declare(sema, f->name, f))
            return false;

        if (!typecheck_expr(sema, f->type_expr))
            return false;
        f->type = f->type_expr->type;
        break;
    }
    case DeclKind::struct_: {
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
    case AstKind::file: {
        bool success = true;
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            if (!typecheck(sema, toplevel)) {
                success = false;
            }
        }
        return success;
    }
    case AstKind::stmt:
        return typecheck_stmt(sema, static_cast<Stmt *>(n));
    case AstKind::decl:
        return typecheck_decl(sema, static_cast<Decl *>(n));
    default:
        assert(!"unknown ast kind");
    }

    return true;
}

namespace {

void codegen_decl(QbeGenerator &q, Decl *d);

// 'value' denotes whether the caller that contains the use of this expression
// requires the actual value of it, or just the address (for lvalues).  If
// 'value' is true, a handle for the generated value is placed on the valstack
// top.
void codegen_expr_explicit(QbeGenerator &q, Expr *e, bool value) {
    switch (e->kind) {
    case ExprKind::integer_literal:
        q.emit_indent("%_{} =w add 0, {}\n", q.valstack.next_id,
                     static_cast<IntegerLiteral *>(e)->value);
        q.valstack.push_temp();
        break;
    case ExprKind::string_literal:
        assert(!"not implemented");
        break;
    case ExprKind::decl_ref: {
        auto dre = static_cast<DeclRefExpr *>(e);
        // This generates a load on 'a':
        //   ... = a
        // But this does not:
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
        if (dre->decl->kind == DeclKind::var) {
            auto var = static_cast<VarDecl *>(dre->decl);

            q.valstack.push_address_explicit(var->frame_local_id);

            if (value) {
                q.emit_indent("%_{} =w loadw {}\n", q.valstack.next_id,
                              q.valstack.pop().format());
                q.valstack.push_temp();
            }
        } else {
            assert(!"not implemented");
        }
        break;
    }
    case ExprKind::call: {
        auto c = static_cast<CallExpr *>(e);

        assert(c->callee_decl->kind == DeclKind::func);
        auto func_decl = static_cast<FuncDecl *>(c->callee_decl);
        if (func_decl->rettypeexpr) {
            std::string abity_str;
            if (func_decl->rettype->builtin) {
                // TODO: "l", "s", "d", ...
                abity_str = "w";
            } else {
                abity_str = std::string{":"} + func_decl->rettype->name->text;
            }
            q.emit_indent("%_{} ={} call ${}()\n", q.valstack.next_id,
                          abity_str, c->func_name->text);
            q.valstack.push_temp();
        } else {
            q.emit_indent("call ${}()\n", c->func_name->text);
        }
        break;
    }
    case ExprKind::struct_def: {
        auto sde = static_cast<StructDefExpr *>(e);
        assert(value && "can't take address of temporary struct def");

        auto id = q.emit_stack_alloc(sde->type);

        // Don't assume anything and just emit all the value copying of each
        // members here.  This might sound expensive, especially in cases where
        // only part of the struct is actually used (e.g. `S {...}.a`), but
        // figuring out those cases is really a job to be done at a higher IR
        // level, not here.

        for (auto term : sde->terms) {
            // Calculate the right offsetted memory location for each
            // member.
            assert(term.field_decl);
            q.emit_indent("%a{} =l add %a{}, {}\n", q.valstack.next_id, id,
                          term.field_decl->offset);
            q.valstack.push_address();
            q.emit_assignment(term.field_decl->type, term.initexpr);
        }

        // Leave the address in the valstack; this is what will be used in lieu
        // of the actual value in later nodes.
        q.valstack.push_address_explicit(id);

        break;
    }
    case ExprKind::member: {
        auto mem = static_cast<MemberExpr *>(e);

        // TODO Respect byte alignment of the field.
        //
        // We can't handle all code generation at this end without recursing
        // into the parent expression, because we have cases like these:
        //
        //   ... = (*p).memb
        //   ... = f().memb
        //
        // So we have to recurse into things, at which point the question of
        // whether to generate values or addresses still stands.
        //
        // TODO: what about this case:
        //   ... = S {.a = 1}.memb

        // emit correct address first
        codegen_expr_explicit(q, mem->parent_expr, false);

        q.emit_indent("%a{} =l add {}, {}\n", q.valstack.next_id,
                      q.valstack.pop().format(), mem->field_decl->offset);
        q.valstack.push_address();

        if (value) {
            // TODO: for struct values?
            q.emit_indent("%_{} =w loadw {}\n", q.valstack.next_id,
                          q.valstack.pop().format());
            q.valstack.push_temp();
        }

        break;
    }
    case ExprKind::unary: {
        // TODO
        assert(!"not implemented");
        break;
    }
    case ExprKind::binary: {
        auto binary = static_cast<BinaryExpr *>(e);
        codegen_expr_explicit(q, binary->lhs, true);
        codegen_expr_explicit(q, binary->rhs, true);

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
        q.emit_indent("%_{} =w {} {}, {}\n", q.valstack.next_id, op_str,
                      q.valstack.pop().format(), q.valstack.pop().format());
        q.valstack.push_temp();
        break;
    }
    default:
        assert(!"unknown expr kind");
    }
}

void codegen_expr(QbeGenerator &q, Expr *e) {
    codegen_expr_explicit(q, e, true);
}

void codegen_expr_addr(QbeGenerator &q, Expr *e) {
    codegen_expr_explicit(q, e, false);
}

void codegen_stmt(QbeGenerator &q, Stmt *s) {
    switch (s->kind) {
    case StmtKind::expr:
        codegen_expr(q, static_cast<ExprStmt *>(s)->expr);
        break;
    case StmtKind::decl:
        codegen_decl(q, static_cast<DeclStmt *>(s)->decl);
        break;
    case StmtKind::assign: {
        auto as = static_cast<AssignStmt *>(s);

        codegen_expr(q, as->rhs);
        auto rhs_val_id = q.valstack.pop().id;

        codegen_expr_addr(q, as->lhs);
        assert(q.valstack.peek().kind == ValueKind::address);

        q.emit_indent("storew %_{}, {}\n", rhs_val_id,
                      q.valstack.pop().format());
        break;
    }
    case StmtKind::return_:
        codegen_expr(q, static_cast<ReturnStmt *>(s)->expr);

        // Whether the top of the valstack might contain is a value or an
        // address, either is fine, because returning a struct by value
        // (usually) happens by address.
        // assert(q.valstack.peek().kind == ValueKind::value);

        q.emit_indent("ret {}\n", q.valstack.pop().format());
        // This is here only to make QBE not complain.  In practice, no
        // instructions after this point should be reachable.
        q.emit("@L{}\n", q.label_id);
        q.label_id++;
        break;
    case StmtKind::if_: {
        auto if_stmt = static_cast<IfStmt *>(s);
        auto id = q.ifelse_label_id;
        q.ifelse_label_id++;
        codegen_expr(q, if_stmt->cond);
        q.emit_indent("jnz {}, @if_{}, @else_{}\n", q.valstack.pop().format(),
                      id, id);
        q.emit("@if_{}\n", id);
        codegen_stmt(q, if_stmt->if_body);
        q.emit_indent("jmp @fi_{}\n", id);
        q.emit("@else_{}\n", id);
        if (if_stmt->else_if_stmt) {
            codegen_stmt(q, if_stmt->else_if_stmt);
        } else if (if_stmt->else_body) {
            codegen_stmt(q, if_stmt->else_body);
        }
        q.emit("@fi_{}\n", id);
        break;
    }
    case StmtKind::compound:
        for (auto s : static_cast<CompoundStmt *>(s)->stmts) {
            codegen_stmt(q, s);
        }
        break;
    default:
        assert(!"unknown stmt kind");
    }
}

void codegen_decl(QbeGenerator &q, Decl *d) {
    switch (d->kind) {
    case DeclKind::var: {
        auto v = static_cast<VarDecl *>(d);

        if (q.context.func_stack.empty()) {
            assert(!"vardecl outside function?");
        }

        // frame_local_id for vardecls is set on-the-fly during code
        // generation. This is because there are other cases that allocate on
        // the stack (such as returning a large struct by-value) that are not
        // emitted by a vardecl.
        v->frame_local_id = q.emit_stack_alloc(v->type);

        if (v->assign_expr) {
            q.valstack.push_address_explicit(v->frame_local_id);
            q.emit_assignment(v->type, v->assign_expr);
        }

        break;
    }
    case DeclKind::func: {
        auto f = static_cast<FuncDecl *>(d);
        std::string abity_str;
        if (f->rettype->builtin) {
            // TODO: "l", "s", "d", ...
            abity_str = "w";
        } else {
            abity_str = std::string{":"} + f->rettype->name->text;
        }
        q.emit("export function {} ${}() {{\n", abity_str, f->name->text);
        q.emit("@start\n");

        q.context.func_stack.push_back(f);
        {
            QbeGenerator::IndentBlock ib{q};

            for (auto body_stmt : f->body->stmts) {
                codegen_stmt(q, body_stmt);
            }
            // Analyses in the earlier passes should make sure that this ret is
            // not reachable.  This is only here to make QBE work meanwhile
            // those analyses are not fully implemented yet.
            q.emit_indent("ret\n");
        }
        q.context.func_stack.pop_back();

        q.emit("}}\n");
        break;
    }
    case DeclKind::struct_: {
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

        q.emit_indent("type :{} = {{", s->name->text);
        for (auto field : s->fields) {
            (void)field;
            q.emit("w, ");
        }
        q.emit("}}\n");
        q.emit("\n");
        break;
    }
    default:
        assert(!"unknown decl kind");
    }
}

} // namespace

// Emit a memory-to-memory value copy.
// TODO: what about a = 3?
//
// a = 3
// a = b
// a = S {.a=...}
// S {.a=...}.a
// f(S {.a=...})
void QbeGenerator::emit_assignment(const Type *lhs_type, Expr *rhs) {
    codegen_expr(*this, rhs);
    auto rhs_value = valstack.pop();
    assert(rhs_value.kind == ValueKind::value);
    auto lhs_address = valstack.pop();
    assert(lhs_address.kind == ValueKind::address);

    // XXX: This assumes that any LHS type that is larger than an eightbyte
    // means the top valstack is a pointer.
    if (lhs_type->size > 8) {
        assert(lhs_type->kind == TypeKind::value);
        assert(!lhs_type->builtin);
        assert(lhs_type->type_decl->kind == DeclKind::struct_);
        auto struct_decl = static_cast<StructDecl *>(lhs_type->type_decl);

        for (auto field : struct_decl->fields) {
            // load from source
            emit_indent("%a{} =l add {}, {}\n", valstack.next_id,
                        rhs_value.format(), field->offset);
            valstack.push_address();

            if (struct_decl->alignment == 8) {
                emit_indent("%_{} =l loadl {}\n", valstack.next_id,
                            valstack.pop().format());
            } else if (struct_decl->alignment == 4) {
                emit_indent("%_{} =w loadw {}\n", valstack.next_id,
                            valstack.pop().format());
            } else {
                assert(!"unknown alignment");
            }
            valstack.push_temp();

            // store to dest
            auto value = valstack.pop();
            emit_indent("%a{} =l add {}, {}\n", valstack.next_id,
                        lhs_address.format(), field->offset);
            valstack.push_address();

            if (struct_decl->alignment == 8) {
                emit_indent("storel {}, {}\n", value.format(),
                            valstack.pop().format());
            } else if (struct_decl->alignment == 4) {
                emit_indent("storew {}, {}\n", value.format(),
                            valstack.pop().format());
            } else {
                assert(!"unknown alignment");
            }
        }
    } else if (lhs_type->size == 8) {
        emit_indent("storel {}, {}\n", rhs_value.format(), lhs_address.format());
    } else if (lhs_type->size == 4) {
        emit_indent("storew {}, {}\n", rhs_value.format(), lhs_address.format());
    } else {
        assert(!"unknown type size");
    }
}

// Emit a value by allocating it on a stack.  That value will be handled with
// its address.
long QbeGenerator::emit_stack_alloc(const Type *type) {
    assert(!context.func_stack.empty());
    // auto current_func = context.func_stack.back();
    // long id = current_func->frame_local_id_counter;
    // current_func->frame_local_id_counter++;
    long id = valstack.next_id;
    valstack.next_id++;

    assert(type->kind == TypeKind::value &&
           "stack allocation for non-value types not implemented");
    assert(type->size);

    emit_indent("%a{} =l ", id);
    if (type->builtin) {
        emit("alloc4");
    } else {
        assert(type->type_decl->kind == DeclKind::struct_ &&
               "non-struct value type?");
        if (static_cast<StructDecl *>(type->type_decl)->alignment == 4) {
            emit("alloc4");
        } else {
            emit("alloc8");
        }
    }
    emit(" {}\n", type->size);

    return id;
}

void cmp::codegen(QbeGenerator &q, AstNode *n) {
    switch (n->kind) {
    case AstKind::file: {
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            codegen(q, toplevel);
        }
        break;
    }
    case AstKind::stmt:
        codegen_stmt(q, static_cast<Stmt *>(n));
        break;
    case AstKind::decl:
        codegen_decl(q, static_cast<Decl *>(n));
        break;
    default:
        assert(!"unknown ast kind");
    }
}
