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

void Sema::scopeOpen() {
    decl_table.scopeOpen();
    type_table.scopeOpen();
    lifetime_table.scopeOpen();
    borrow_table.scopeOpen();
}

void Sema::scopeClose() {
    decl_table.scopeClose();
    type_table.scopeClose();
    lifetime_table.scopeClose();
    borrow_table.scopeClose();
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
        sema.scopeOpen();
        for (auto stmt : static_cast<CompoundStmt *>(s)->stmts) {
            if (!typecheck_stmt(sema, stmt)) {
                success = false;
            }
        }
        sema.scopeClose();
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
        sema.decl_table.scopeOpen();

        bool success = true;
        for (auto stmt : f->body->stmts) {
            if (!typecheck_stmt(sema, stmt)) {
                success = false;
            }
        }

        sema.decl_table.scopeClose();
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

        sema.decl_table.scopeOpen();
        bool success = true;
        for (auto field : s->fields) {
            if (!typecheck_decl(sema, field)) {
                success = false;
            }
        }
        sema.decl_table.scopeClose();
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
void QbeGenerator::codegenExprExplicit(Expr *e, bool value) {
    switch (e->kind) {
    case ExprKind::integer_literal:
        emit("%_{} =w add 0, {}", valstack.next_id,
                     static_cast<IntegerLiteral *>(e)->value);
        annotate("{}: integer literal", e->loc.line);
        valstack.push_temp_value();
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

            valstack.push_address_explicit(var->frame_local_id);

            if (value) {
                if (dre->type->size > 8) {
                    // FIXME: assumes this is a stack-allocated struct
                    assert(!dre->type->builtin);
                    assert(dre->type->type_decl->kind == DeclKind::struct_);
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
    case ExprKind::call: {
        auto c = static_cast<CallExpr *>(e);

        assert(c->callee_decl->kind == DeclKind::func);
        auto func_decl = static_cast<FuncDecl *>(c->callee_decl);

        // codegen arguments first
        std::vector<Value> generated_args;
        for (auto arg : c->args) {
            codegenExprExplicit(arg, true);
            generated_args.push_back(valstack.pop());
        }

        if (func_decl->rettypeexpr) {
            emit("%_{} ={} call ${}(", valstack.next_id,
                          abityStr(func_decl->rettype), c->func_name->text);

            for (size_t i = 0; i < c->args.size(); i++) {
                emitSameLine("{} {}, ", abityStr(c->args[i]->type),
                       generated_args[i].format());
            }

            emitSameLine(")");

            valstack.push_temp_value();
        } else {
            emit("call ${}(", c->func_name->text);

            // @Copypaste from above
            for (size_t i = 0; i < c->args.size(); i++) {
                emitSameLine("{} {}, ", abityStr(c->args[i]->type),
                       generated_args[i].format());
            }

            emitSameLine(")");

            // Don't push to valstack here; that the caller doesn't erroneously
            // pop afterwards should have been checked by the semantic phase.
        }
        break;
    }
    case ExprKind::struct_def: {
        auto sde = static_cast<StructDefExpr *>(e);

        // TODO: document why we alloc here
        auto id = emitStackAlloc(sde->type, sde->loc.line, sde->text(sema));

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
            emitAssignment(term.field_decl, term.initexpr);
        }

        // Leave the address in the valstack; this is what will be used in lieu
        // of the actual value in later nodes.
        valstack.push_address_explicit(id);

        break;
    }
    case ExprKind::member: {
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
        codegenExprExplicit(mem->parent_expr, false);

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
    case ExprKind::unary: {
        auto ue = static_cast<UnaryExpr *>(e);

        if (ue->kind == UnaryExprKind::ref) {
            codegenExprExplicit(ue->operand, false);
        } else if (ue->kind == UnaryExprKind::deref) {
            assert(!"TODO: start here");
        } else {
            // FIXME: assumes paren expr
            codegenExprExplicit(ue->operand, value);
        }
        break;
    }
    case ExprKind::binary: {
        auto binary = static_cast<BinaryExpr *>(e);
        codegenExprExplicit(binary->lhs, true);
        codegenExprExplicit(binary->rhs, true);

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

void QbeGenerator::codegenExpr(Expr *e) {
    codegenExprExplicit(e, true);
}

void QbeGenerator::codegenExprAddress(Expr *e) {
    codegenExprExplicit(e, false);
}

void QbeGenerator::codegenStmt(Stmt *s) {
    switch (s->kind) {
    case StmtKind::expr:
        codegenExpr(static_cast<ExprStmt *>(s)->expr);
        break;
    case StmtKind::decl:
        codegenDecl(static_cast<DeclStmt *>(s)->decl);
        break;
    case StmtKind::assign: {
        auto as = static_cast<AssignStmt *>(s);

        codegenExpr(as->rhs);
        auto rhs_val_id = valstack.pop().id;

        codegenExprAddress(as->lhs);
        assert(valstack.peek().kind == ValueKind::address);
        auto lhs_address = valstack.pop();

        emit("storew %_{}, {}", rhs_val_id, lhs_address.format());
        annotate("{}: assign to {}", as->loc.line, as->lhs->text(sema));
        break;
    }
    case StmtKind::return_:
        codegenExpr(static_cast<ReturnStmt *>(s)->expr);

        // Whether the top of the valstack might contain is a value or an
        // address, either is fine, because returning a struct by value
        // (usually) happens by address.
        // assert(valstack.peek().kind == ValueKind::value);

        emit("ret {}", valstack.pop().format());
        // This is here only to make QBE not complain.  In practice, no
        // instructions after this point should be reachable.
        emitSameLine("\n@L{}", label_id);
        label_id++;
        break;
    case StmtKind::if_: {
        auto if_stmt = static_cast<IfStmt *>(s);
        auto id = ifelse_label_id;
        ifelse_label_id++;
        codegenExpr(if_stmt->cond);
        emit("jnz {}, @if_{}, @else_{}", valstack.pop().format(),
                      id, id);
        emitSameLine("\n@if_{}", id);
        codegenStmt(if_stmt->if_body);
        emit("jmp @fi_{}", id);
        emitSameLine("\n@else_{}", id);
        if (if_stmt->else_if_stmt) {
            codegenStmt(if_stmt->else_if_stmt);
        } else if (if_stmt->else_body) {
            codegenStmt(if_stmt->else_body);
        }
        emitSameLine("\n@fi_{}", id);
        break;
    }
    case StmtKind::compound:
        for (auto s : static_cast<CompoundStmt *>(s)->stmts) {
            codegenStmt(s);
        }
        break;
    default:
        assert(!"unknown stmt kind");
    }
}

void QbeGenerator::codegenDecl(Decl *d) {
    switch (d->kind) {
    case DeclKind::var: {
        auto v = static_cast<VarDecl *>(d);

        if (sema.context.func_stack.empty()) {
            assert(!"vardecl outside function?");
        }

        // frame_local_id for vardecls is set on-the-fly during code
        // generation. This is because there are other cases that allocate on
        // the stack (such as returning a large struct by-value) that are not
        // emitted by a vardecl.
        v->frame_local_id = emitStackAlloc(v->type, v->loc.line, v->name->text);

        if (v->assign_expr) {
            valstack.push_address_explicit(v->frame_local_id);
            emitAssignment(v, v->assign_expr);
        }

        break;
    }
    case DeclKind::func: {
        auto f = static_cast<FuncDecl *>(d);

        emitSameLine("\nexport function {} ${}(", abityStr(f->rettype),
                     f->name->text);

        for (auto param : f->params) {
            emitSameLine("{} %{}, ", abityStr(param->type), param->name->text);
        }

        emitSameLine(") {{");
        emitSameLine("\n@start");

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
                codegenStmt(body_stmt);
            }
            // Analyses in the earlier passes should make sure that this ret is
            // not reachable.  This is only here to make QBE work meanwhile
            // those analyses are not fully implemented yet.
            emit("ret");
        }
        sema.context.func_stack.pop_back();

        emitSameLine("\n}}");
        emitSameLine("\n");
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

        emit("type :{} = {{", s->name->text);
        for (auto field : s->fields) {
            (void)field;
            emitSameLine("w, ");
        }
        emitSameLine("}}");
        emitSameLine("");
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
void QbeGenerator::emitAssignment(const Decl *lhs, Expr *rhs) {
    codegenExpr(rhs);

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
        assert(lhs_type->type_decl->kind == DeclKind::struct_);
        auto struct_decl = static_cast<StructDecl *>(lhs_type->type_decl);

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
long QbeGenerator::emitStackAlloc(const Type *type, size_t line,
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
        emitSameLine("alloc8");
    } else if (type->builtin) {
        assert(type->kind != TypeKind::ptr &&
               "ptr & builtin for Types is possible?");
        emitSameLine("alloc4");
    } else {
        assert(type->kind == TypeKind::value);
        assert(type->type_decl->kind == DeclKind::struct_ &&
               "non-struct value type?");
        if (static_cast<StructDecl *>(type->type_decl)->alignment == 4) {
            emitSameLine("alloc4");
        } else {
            emitSameLine("alloc8");
        }
    }
    emitSameLine(" {}", type->size);
    annotate("{}: alloc {}", line, text);

    return id;
}

void QbeGenerator::codegen(AstNode *n) {
    switch (n->kind) {
    case AstKind::file: {
        for (auto toplevel : static_cast<File *>(n)->toplevels) {
            codegen(toplevel);
        }
        break;
    }
    case AstKind::stmt:
        codegenStmt(static_cast<Stmt *>(n));
        break;
    case AstKind::decl:
        codegenDecl(static_cast<Decl *>(n));
        break;
    default:
        assert(!"unknown ast kind");
    }
}
