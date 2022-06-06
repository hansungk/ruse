#include "sema.h"
#include "fmt/core.h"
#include "parse.h"
#include "source.h"
#include "types.h"
#include <cassert>
#include <cstdarg>

namespace cmp {

template <typename... Args> static bool error(SourceLoc loc, Args &&...args) {
  auto message = fmt::format(std::forward<Args>(args)...);
  fmt::print(stderr, "{}:{}:{}: error: {}\n", loc.filename, loc.line, loc.col,
             message);
  return false;
  // Not exiting here makes the compiler go as far as it can and report all of
  // the errors it encounters.
  // exit(EXIT_FAILURE);
}

Type *make_struct_type(Sema &sema, Name *n, Decl *decl) {
  Type *t = new Type(TypeKind::value, n);
  t->origin_decl = decl;
  sema.type_pool.push_back(t);
  return t;
}

Type *make_pointer_type(Sema &sema, Name *name, Type *base_type) {
  Type *t = new Type(TypeKind::pointer, name);
  t->base_type = base_type;
  // assumes pointers are always 8 bytes
  t->size = 8;
  sema.type_pool.push_back(t);
  return t;
}

Type *make_array_type(Sema &sema, Name *name, Type *base_type) {
  Type *t = new Type(TypeKind::array, name);
  t->base_type = base_type;
  // assumes pointers are always 8 bytes
  t->size = 8;
  sema.type_pool.push_back(t);
  return t;
}

Type *make_builtin_type(Sema &sema, Name *n) {
  Type *t = new Type(TypeKind::value, n);
  t->builtin = true;
  sema.type_pool.push_back(t);
  return t;
}

static Type *make_builtin_type_from_name(Sema &s, const std::string &str) {
  Name *name = s.name_table.pushlen(str.data(), str.length());
  auto struct_decl = s.make_node<StructDecl>(name);
  struct_decl->type = make_builtin_type(s, name);
  s.decl_table.insert(name, struct_decl);
  return struct_decl->type;
}

// Push Decls for the builtin types into the global scope of decl_table, so
// that they are visible from any point in the AST.
void setup_builtin_types(Sema &s) {
  s.context.void_type = make_builtin_type_from_name(s, "void");
  s.context.int_type = make_builtin_type_from_name(s, "int");
  s.context.int_type->size = 4;
  s.context.char_type = make_builtin_type_from_name(s, "char");
  s.context.char_type->size = 1;
  s.context.string_type = make_builtin_type_from_name(s, "string");
  // FIXME: size of a string is something like pointer + length
  s.context.string_type->size = 4;
  s.name_table.push("buf");
  s.name_table.push("len");
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
    return kind == TypeKind::value && origin_decl &&
           origin_decl->kind == Decl::struct_;
}

bool Type::is_pointer() const {
  return kind == TypeKind::pointer;
}

bool Type::is_builtin(Sema &sema) const {
    return this == sema.context.int_type || this == sema.context.char_type ||
           this == sema.context.void_type || this == sema.context.string_type;
}

static bool is_lvalue(const Expr *e) {
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

bool declare_in_struct(StructDecl *struct_decl, Name *name, Decl *decl) {
    assert(struct_decl);

    auto found = struct_decl->decl_table.find(name);
    if (found && found->value->kind == decl->kind &&
        found->scope_level == struct_decl->decl_table.curr_scope_level) {
        return error(decl->loc, "redefinition of '{}' inside struct {}",
                     name->text, struct_decl->name->text);
    }

    struct_decl->decl_table.insert(name, decl);
    return true;
}

// Declare a `decl` that has `name` in the current scope.
// Returns true if success; otherwise (e.g.  redeclaration), return false and
// do error handling.
bool declare(Sema &sema, Decl *decl) {
    assert(decl);

    // For struct methods, we need to declare in a special struct-local scope.
    if (decl->kind == Decl::func) {
        auto fd = decl->as<FuncDecl>();
        // If 'fd' is a struct method, if its struct parameter fails check,
        // we can't declare them in its method scope.
        if (fd->struct_param && fd->target_struct) {
            return declare_in_struct(fd->target_struct, fd->name, fd);
        }
    }

    auto found = sema.decl_table.find(decl->name);
    if (found && found->value->kind == decl->kind &&
        found->scope_level == sema.decl_table.curr_scope_level) {
        return error(decl->loc, "redefinition of '{}'", decl->name->text);
    }

    sema.decl_table.insert(decl->name, decl);
    return true;
}

// Get or construct a derived type with kind `kind`, from a given type
// `base_type`.
//
// Derived types are only present in the type table if they occur in the source
// code.  Trying to push them every time we see one is sufficient to keep this
// invariant.
Type *get_derived_type(Sema &sema, TypeKind kind, Type *base_type) {
  Type *derived_type = nullptr;
  Name *name =
      make_name_of_derived_type(sema.name_table, kind, base_type->name);

  if (auto found = sema.type_table.find(name)) {
    derived_type = found->value;
  } else if (kind == TypeKind::pointer) {
    derived_type = make_pointer_type(sema, name, base_type);
    sema.type_table.insert(name, derived_type);
  } else if (kind == TypeKind::array) {
    derived_type = make_array_type(sema, name, base_type);
    sema.type_table.insert(name, derived_type);
  } else {
    assert(!"unknown type kind");
  }

  return derived_type;
}

bool check_assignable(const Type *to, const Type *from) {
  // TODO: Typecheck assignment rules so far:
  //
  // 1. Pointer <- mutable pointer.
  // 2. Exact same match.

  // Allow promotion from mutable to immutable pointer.
  if (to->kind == TypeKind::pointer && from->is_pointer()) {
    // NOTE: this may be related to 'unification'. Ref:
    // http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
    return check_assignable(to->base_type, from->base_type);
  }
  return to == from;
}

bool check_expr(Sema &sema, Expr *e);
bool check_stmt(Sema &sema, Stmt *s);
bool check_decl(Sema &sema, Decl *d);

bool check_unary_expr(Sema &sema, UnaryExpr *u) {
  switch (u->kind) {
  case UnaryExpr::paren:
    if (!check_expr(sema, u->operand))
      return false;
    u->type = u->operand->type;
    break;
  case UnaryExpr::deref: {
    if (!check_expr(sema, u->operand))
      return false;

    if (!u->operand->type->is_pointer()) {
      return error(u->loc, "dereferenced a non-pointer type '{}'",
                   u->operand->type->name->text);
    }
    u->type = u->operand->type->base_type;
    u->decl = sema.make_node<VarDecl>(nullptr, u->type, true);

    // We still need to do typecheck on this temporary decl to e.g. bind
    // new decls for all struct children.
    check_decl(sema, u->decl);

    // Temporary VarDecls are not pushed to the scoped decl table, because
    // they are not meant to be accessible from other source locations.
    // Therefore they don't need to have a name.

    break;
  }
  case UnaryExpr::var_ref:
  case UnaryExpr::ref: {
    if (!check_expr(sema, u->operand))
      return false;

    // Prohibit taking address of an rvalue.
    if (!is_lvalue(u->operand)) {
      return error(u->loc, "cannot take address of an rvalue");
    }

    // TODO: Prohibit mutable reference of an immutable variable.

    auto type_kind = TypeKind::pointer;
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

VarDecl *VarDecl::findMemberDecl(const Name *member_name) const {
  for (auto child : children) {
    if (child->name == member_name) {
      return child;
    }
  }
  return nullptr;
}

bool check_expr(Sema &sema, Expr *e) {
  switch (e->kind) {
  case Expr::integer_literal: {
    e->as<IntegerLiteral>()->type = sema.context.int_type;
    break;
  }
  case Expr::string_literal: {
    e->as<StringLiteral>()->type = sema.context.string_type;
    break;
  }
  case Expr::decl_ref: {
    auto de = e->as<DeclRefExpr>();
    auto sym = sema.decl_table.find(de->name);
    if (!sym) {
      return error(de->loc, "undeclared identifier '{}'", de->name->text);
    }
    de->decl = sym->value;
    assert(de->decl);
    de->type = de->decl->type;
    // DeclRefExprs don't always have a non-null type associated, i.e. when
    // it designates a function.
    if (de->decl->kind != Decl::func) {
      // A DeclRefExpr may succeed name binding but may have failed
      // typechecking, e.g. when its declaration failed typechecking:
      //
      //     let a = invalid()
      //     a + ...
      //
      // In those cases, just give up here so that outer constructs do
      // not bother.
      if (!de->type)
        return false;
    }
    break;
  }
  case Expr::call: {
    auto c = e->as<CallExpr>();
    if (c->kind != CallExpr::func) {
      assert(!"not implemented");
    }

    if (!check_expr(sema, c->callee_expr))
      return false;

    assert(c->callee_expr->decl);

    c->callee_decl = c->callee_expr->decl;
    if (c->callee_decl->kind != Decl::func) {
      return error(c->loc,
                   "'{}' is not a function or a method", // FIXME differentiate
                                                         // between function and
                                                         // method?
                   c->callee_decl->name->text);
    }
    auto func_decl = c->callee_decl->as<FuncDecl>();
    assert(func_decl->ret_type);
    c->type = func_decl->ret_type;

    if (c->args.size() != func_decl->params.size()) {
      return error(
          c->loc, "function '{}' accepts {} arguments, but {} are given",
          func_decl->name->text, func_decl->params.size(), c->args.size());
    }

    for (size_t i = 0; i < c->args.size(); i++) {
      if (!check_expr(sema, c->args[i]))
        return false;

      if (!check_assignable(func_decl->params[i]->type, c->args[i]->type)) {
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
    auto sde = e->as<StructDefExpr>();
    if (!check_expr(sema, sde->name_expr))
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
      for (auto field : sde->name_expr->decl->as<StructDecl>()->fields) {
        if (term.name == field->name) {
          matched_field = field;
          term.field_decl = matched_field;
          break;
        }
      }
      if (!matched_field) {
        return error(sde->loc, "'{}' is not a member of struct '{}'",
                     term.name->text, struct_type->name->text);
      }

      if (!check_expr(sema, term.initexpr))
        return false;

      if (!check_assignable(matched_field->type, term.initexpr->type)) {
        return error(term.initexpr->loc, "cannot assign '{}' type to '{}'",
                     term.initexpr->type->name->text,
                     matched_field->type->name->text);
      }
    }

    sde->type = struct_type;
    break;
  }
  case Expr::member: {
    auto mem = e->as<MemberExpr>();
    if (!check_expr(sema, mem->parent_expr))
      return false;

    auto parent_type = mem->parent_expr->type;
    Decl *parent_type_decl = nullptr;
    Name *reported_name = nullptr;
    if (parent_type->is_pointer()) {
      reported_name = parent_type->base_type->name;
      if (parent_type->base_type->is_struct()) {
        parent_type_decl = parent_type->base_type->origin_decl;

        // How do we get the VarDecl of the target of the pointer?
        // Best way to do this would be to rewrite the AST to (*p).mem.
        // Then, a second sema pass would bind a temporary VarDecl to
        // (*p) without any further modification.
        auto new_parent = sema.make_node_pos<UnaryExpr>(
            mem->parent_expr->pos, UnaryExpr::deref, mem->parent_expr);
        mem->parent_expr = new_parent;

        // Redo with the rewritten node.
        return check_expr(sema, mem);
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

    // At this point, parent_expr is either a struct or a pointer to
    // struct.  Now we have to check the member side.

    auto parent_type_struct_decl = parent_type_decl->as<StructDecl>();
    auto sym = parent_type_struct_decl->decl_table.find(mem->member_name);
    if (!sym) {
      return error(mem->loc, "'{}' is not a member of struct '{}'",
                   mem->member_name->text, reported_name->text);
    }

    // Figure out if this is a field or a method.
    if (sym->value->kind == Decl::field) {
      mem->field_decl = sym->value->as<FieldDecl>();

      // If parent is an lvalue, its child is also an lvalue.  So find
      // the right children VarDecl of the paren tand bind it to this
      // MemberExpr as well.
      if (mem->parent_expr->decl) {
        assert(mem->parent_expr->decl->kind == Decl::var);
        auto parent_var_decl = mem->parent_expr->decl->as<VarDecl>();
        mem->decl = parent_var_decl->findMemberDecl(mem->member_name);
        assert(mem->decl && "struct member failed to namebind");
      }

      mem->type = mem->field_decl->type;
      assert(mem->type);
    } else {
      assert(sym->value->kind == Decl::func);

      // For methods, we need to keep track of the original declaration
      // of the method.
      mem->decl = sym->value->as<FuncDecl>();
    }

    break;
  }
  case Expr::subscript: {
    assert(!"TODO: subscript");
  }
  case Expr::unary: {
    return check_unary_expr(sema, e->as<UnaryExpr>());
  }
  case Expr::binary: {
    auto b = e->as<BinaryExpr>();
    if (!check_expr(sema, b->lhs))
      return false;
    if (!check_expr(sema, b->rhs))
      return false;

    auto lhs_type = b->lhs->type;
    auto rhs_type = b->rhs->type;

    if (lhs_type != rhs_type) {
      return error(b->loc, "incompatible binary op with type '{}' and '{}'",
                   lhs_type->name->text, rhs_type->name->text);
    }

    b->type = lhs_type;

    break;
  }
  case Expr::type_: {
    auto t = e->as<TypeExpr>();

    // Namebinding for TypeExprs only include linking existing Decls to the
    // type names used in the expression, not declaring new ones.  The
    // declaration would be done when visiting VarDecls and StructDecls,
    // etc.

    if (t->subexpr) {
      if (!check_expr(sema, t->subexpr))
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
      assert(t->type && "type not resolved after visiting corresponding *Decl");
    } else if (t->kind == TypeKind::pointer || t->kind == TypeKind::array) {
      t->type = get_derived_type(sema, t->kind, t->subexpr->type);
    } else {
      assert(!"unknown type kind");
    }
    break;
  }
  default:
    assert(!"unknown expr kind");
  }

  // TODO: doc why this is allowed non-null.
  // assert(e->type);

  // No more work is supposed to be done here.
  return true;
}

bool check_stmt(Sema &sema, Stmt *s) {
    switch (s->kind) {
    case Stmt::expr:
        return check_expr(sema, s->as<ExprStmt>()->expr);
    case Stmt::decl: {
        auto decl = s->as<DeclStmt>()->decl;
        if (!check_decl(sema, decl))
            return false;
        if (!declare(sema, decl))
            return false;
        break;
    }
    case Stmt::assign: {
        auto as = s->as<AssignStmt>();
        if (!check_expr(sema, as->rhs))
            return false;
        if (!check_expr(sema, as->lhs))
            return false;

        auto lhs_type = as->lhs->type;
        auto rhs_type = as->rhs->type;

        if (!is_lvalue(as->lhs)) {
            return error(as->loc, "cannot assign to an rvalue");
        }

        if (!check_assignable(lhs_type, rhs_type)) {
            return error(as->loc, "cannot assign '{}' type to '{}'",
                         rhs_type->name->text, lhs_type->name->text);
        }

        break;
    }
    case Stmt::if_: {
        auto if_stmt = s->as<IfStmt>();
        if (!check_expr(sema, if_stmt->cond))
            return false;

        if (!check_stmt(sema, if_stmt->if_body))
            return false;
        if (if_stmt->else_if_stmt) {
            if (!check_stmt(sema, if_stmt->else_if_stmt))
                return false;
        } else if (if_stmt->else_body) {
            if (!check_stmt(sema, if_stmt->else_body))
                return false;
        }
        break;
    }
    case Stmt::return_: {
        auto r = s->as<ReturnStmt>();
        if (!r->expr)
            break;
        if (!check_expr(sema, r->expr))
            return false;

        assert(!sema.context.func_stack.empty());
        auto current_func = sema.context.func_stack.back();
        if (r->expr->type != current_func->ret_type) {
            if (current_func->ret_type == sema.context.void_type) {
                return error(
                    r->expr->loc,
                    "tried to return a value from a void function '{}'",
                    current_func->name->text);
            }
            return error(
                r->expr->loc,
                "tried to return '{}' from function '{}', which returns '{}'",
                r->expr->type->name->text, current_func->name->text,
                current_func->ret_type->name->text);
        }

        break;
    }
    case Stmt::compound: {
        bool success = true;
        sema.scope_open();
        for (auto line : s->as<CompoundStmt>()->stmts) {
            if (!check(sema, line)) {
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

VarDecl *instantiate_member_decl(Sema &sema, VarDecl *parent, Name *name,
                           Type *type) {
    auto field = sema.make_node<VarDecl>(name, type, parent->mut);
    parent->children.push_back(field);
    return field;
}

static bool check_func_decl(Sema &sema, FuncDecl *f) {
  // Struct methods.
  if (f->struct_param) {
    if (!check_decl(sema, f->struct_param))
      return false;

    auto struct_param_type = f->struct_param->type;
    auto struct_param_type_expr = f->struct_param->type_expr->as<TypeExpr>();

    // By-pointer methods
    if (struct_param_type->is_pointer()) {
      if (!struct_param_type->base_type->is_struct()) {
        return error(struct_param_type_expr->subexpr->loc,
                     "cannot declare a method for '{}' which is not a struct",
                     struct_param_type->base_type->name->text);
      }
      f->target_struct =
          struct_param_type->base_type->origin_decl->as<StructDecl>();
    }
    // By-value methods
    else if (!struct_param_type->is_struct()) {
      return error(struct_param_type_expr->loc,
                   "cannot declare a method for '{}' which is not a struct",
                   struct_param_type->name->text);
    } else {
      // all is good
      f->target_struct =
          struct_param_type->origin_decl->as<StructDecl>();
    }
  }

  if (f->ret_type_expr) {
    if (!check_expr(sema, f->ret_type_expr))
      return false;
    f->ret_type = f->ret_type_expr->type;
  } else {
    f->ret_type = sema.context.void_type;
  }

  // Work inside a new function-local scope to handle params and body
  // statements.
  bool success = true;
  {
    // RAII is convenient for when we fail at typecheck() functions below.
    Sema::DeclTableScope dts{sema};
    // @Improve: change this to RAII as well
    sema.context.func_stack.push_back(f);

    if (f->struct_param) {
      if (!declare(sema, f->struct_param)) {
        return false;
      }
    }

    for (auto param : f->params) {
      if (!check_decl(sema, param)) {
        return false;
      }
      if (!declare(sema, param)) {
        return false;
      }
    }

    // Extern function does not have a body.
    if (f->body) {
      for (auto line : f->body->stmts) {
        if (!check(sema, line)) {
          success = false;
        }
      }
    }

    sema.context.func_stack.pop_back();
  }

  return success;
}

// Note that this function will also do declare() for new symbols, so the
// caller would have to set new scopes accordingly.
bool check_decl(Sema &sema, Decl *d) {
  switch (d->kind) {
  case Decl::var: {
    auto v = d->as<VarDecl>();
    // if (!declare(sema, v->name, v))
    //     return false;
    if (v->assign_expr) {
      if (!check_expr(sema, v->assign_expr))
        return false;
      v->type = v->assign_expr->type;
    } else if (v->type_expr) {
      if (!check_expr(sema, v->type_expr))
        return false;
      v->type = v->type_expr->type;
    }

    // For struct-typed VarDecls, instantiate all of its fields.
    if (v->type->is_struct()) {
      auto struct_origin_decl = v->type->origin_decl->as<StructDecl>();
      for (auto field : struct_origin_decl->fields) {
        instantiate_member_decl(sema, v, field->name, field->type);
        // FIXME: should we check_decl() children here?
      }
    } else if (v->type == sema.context.string_type) {
      // String types have hidden children decls of { buf: *uint8, len: int64 }.
      // Instantiate these decls here.
      Name *name_buf = sema.name_table.get("buf");
      Name *name_len = sema.name_table.get("len");
      assert(name_buf && name_len);
      instantiate_member_decl(sema, v, name_buf, sema.context.int_type);
      instantiate_member_decl(sema, v, name_len, sema.context.int_type /*FIXME*/);
      assert(v->findMemberDecl(name_buf));
    }

    break;
  }
  case Decl::func:
    return check_func_decl(sema, d->as<FuncDecl>());
  case Decl::field: {
    auto f = d->as<FieldDecl>();
    // field redeclaration check
    // if (!declare(sema, f->name, f))
    //     return false;

    if (!check_expr(sema, f->type_expr))
      return false;
    f->type = f->type_expr->type;
    break;
  }
  case Decl::struct_: {
    auto s = d->as<StructDecl>();
    s->type = make_struct_type(sema, s->name, s);

    // if (!declare(sema, f->name, f))
    //     return false;

    sema.decl_table.scope_open();
    bool success = true;
    for (auto field : s->fields) {
      if (!check_decl(sema, field)) {
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

bool check(Sema &sema, AstNode *n) {
  bool success;

  switch (n->kind) {
  case AstNode::file:
    success = true;
    for (auto toplevel : n->as<File>()->toplevels) {
      if (!check(sema, toplevel)) {
        success = false;
      }
    }
    return success;
  case AstNode::stmt:
    return check_stmt(sema, n->as<Stmt>());
  case AstNode::decl:
    // For function and struct decls, even if one of its body statements or
    // members fail typechecking, we probably still want to declare them to
    // prevent too many chained errors for the code that use them.
    success = check_decl(sema, n->as<Decl>());
    if (!declare(sema, n->as<Decl>()))
      return false;
    return success;
  default:
    assert(!"unknown ast kind");
  }

  return true;
}

} // namespace cmp
