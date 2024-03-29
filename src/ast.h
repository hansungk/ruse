#ifndef CMP_AST_H
#define CMP_AST_H

#include "lex.h"
#include "scoped_table.h"
#include "types.h"

namespace cmp {

// Forward-declare stuff so that we don't have to include sema.h.
struct AstNode;
struct File;
struct Stmt;
struct Expr;
struct Decl;
struct VarDecl;
struct FuncDecl;
struct FieldDecl;
struct StructDecl;
struct EnumVariantDecl;
struct EnumDecl;
struct BadDecl;

struct AstNode {
  const enum AstKind {
    file,
    stmt,
    decl,
    expr,
  } kind = AstKind::decl; // node kind
  // TODO: deprecate pos/endpos
  size_t pos = 0;    // start pos of this AST in the source text
  size_t endpos = 0; // end pos of this AST in the source text
  SourceLoc loc;
  SourceLoc endloc;

  AstNode() {}
  AstNode(AstKind kind) : kind(kind) {}
  virtual ~AstNode() = default;

  // Casts to the *pointer* of the given type.
  template <typename T> T *as() {
    assert(is<T>());
    return static_cast<T *>(this);
  }
  template <typename T> const T *as() const {
    assert(is<T>());
    return static_cast<const T *>(this);
  }
  template <typename T> bool is() const;

  std::string_view text(const Sema &sema);

  // RAII trick to handle indentation.
  static int indent;
  struct PrintScope {
    PrintScope() { indent += 2; }
    ~PrintScope() { indent -= 2; }
  };
};

// File is simply a group of Toplevels.
struct File : public AstNode {
  File() : AstNode(AstNode::file) {}

  std::vector<AstNode *> toplevels;
};

//
// Statements
// ==========

struct Stmt : public AstNode {
  const enum Kind {
    decl,
    expr,
    assign,
    return_,
    compound,
    if_,
    builtin,
    bad,
  } kind;

  Stmt(Kind s) : AstNode(AstNode::stmt), kind(s) {}
  template <typename T> bool is() const;
};

// Variable declaration statement; doesn't include function declarations.
struct DeclStmt : public Stmt {
  DeclStmt(Decl *d) : Stmt(Stmt::decl), decl(d) {}

  Decl *decl;
};

struct ExprStmt : public Stmt {
  ExprStmt(Expr *e) : Stmt(Stmt::expr), expr(e) {}

  Expr *expr;
};

// Assignment statements, such as `a = b` or `a[0] = func()`.
// Non-single-token expressions can come at the LHS as long as they are lvalues,
// but this is not easily determined at the parsing stage.  The assignability
// will be checked at the semantic stage.
struct AssignStmt : public Stmt {
  AssignStmt(Expr *l, Expr *r, bool m)
      : Stmt(Stmt::assign), lhs(l), rhs(r), move(m) {}

  Expr *lhs;
  Expr *rhs;
  bool move;
};

struct ReturnStmt : public Stmt {
  Expr *expr;

  ReturnStmt(Expr *e) : Stmt(Stmt::return_), expr(e) {}
};

struct CompoundStmt : public Stmt {
  std::vector<AstNode *> stmts;

  CompoundStmt() : Stmt(Stmt::compound) {}
};

struct IfStmt : public Stmt {
  Expr *cond;            // conditional expr
  CompoundStmt *if_body; // body for true cond
  // Views 'else if' clauses as a separate if statement that is embedeed in
  // the 'else' clause.  'else_if' and 'else_body' cannot be non-null at the
  // same time.
  IfStmt *else_if_stmt = nullptr;
  CompoundStmt *else_body = nullptr;

  IfStmt(Expr *e, CompoundStmt *is, IfStmt *ei, CompoundStmt *es)
      : Stmt(Stmt::if_), cond(e), if_body(is), else_if_stmt(ei), else_body(es) {
  }
};

struct BuiltinStmt : public Stmt {
  std::string_view text;

  BuiltinStmt(std::string_view sv) : Stmt(Stmt::builtin), text(sv) {}
};

struct BadStmt : public Stmt {
  BadStmt() : Stmt(Stmt::bad) {}
};

template <> inline bool Stmt::is<DeclStmt>() const { return kind == decl; }
template <> inline bool Stmt::is<ExprStmt>() const { return kind == expr; }
template <> inline bool Stmt::is<AssignStmt>() const { return kind == assign; }
template <> inline bool Stmt::is<ReturnStmt>() const { return kind == return_; }
template <> inline bool Stmt::is<CompoundStmt>() const {
  return kind == compound;
}
template <> inline bool Stmt::is<IfStmt>() const { return kind == if_; }
template <> inline bool Stmt::is<BuiltinStmt>() const {
  return kind == builtin;
}
template <> inline bool Stmt::is<BadStmt>() const { return kind == bad; }
template <typename T> inline bool Stmt::is() const {
  assert(!"unimplemented is()");
}

// Expressions
// ===========

struct Type;

struct Expr : public AstNode {
  enum Kind {
    integer_literal,
    string_literal,
    decl_ref,
    call,
    struct_def,
    cast,
    member,
    subscript,
    unary,
    binary,
    type_,
    bad,
  } kind;

  // type of the expression.
  //
  // For expressions that have a Decl, e.g. DeclRefExpr and MemberExpr, their
  // types are stored in decl->type.  For these cases, the value of this
  // pointer should be maintained the same as decl->type, so that expr->type
  // becomes the unified way to retrieve the type of an expression.
  Type *type = nullptr;
  // TODO: document
  Decl *decl = nullptr;

  Expr(Kind e) : AstNode(AstNode::expr), kind(e), type(nullptr) {}
  template <typename T> bool is() const;
  bool is_lvalue() const { return decl != nullptr; }
};

struct IntegerLiteral : public Expr {
  int64_t value;

  IntegerLiteral(int64_t v) : Expr(Expr::integer_literal), value(v) {}
};

struct StringLiteral : public Expr {
  std::string_view value;

  StringLiteral(std::string_view sv) : Expr(Expr::string_literal), value(sv) {}
};

// A unary expression that references a declaration object, e.g. a variable or
// a function.
struct DeclRefExpr : public Expr {
  Name *name = nullptr;

  DeclRefExpr(Name *n) : Expr(Expr::decl_ref), name(n) {}
};

// Also includes typecasts.
struct CallExpr : public Expr {
  enum Kind {
    func,
  } kind;

  // This could be either DeclRefExpr ("f()") or MemberExpr ("s.m()").
  Expr *func_expr = nullptr;
  // Decl of the called function, or the destination type for typecasts.
  Decl *func_decl = nullptr; // FIXME: just use callee_expr->decl
  std::vector<Expr *> args;

  CallExpr(Kind kind, Expr *ce, const std::vector<Expr *> &args)
      : Expr(Expr::call), kind(kind), func_expr(ce), args(args) {}
};

// '.memb = expr' part in Struct { ... }.
struct StructDefTerm {
  Name *name = nullptr;
  Expr *initexpr = nullptr;
  FieldDecl *field_decl = nullptr;
};

// 'Struct { .m1 = .e1, .m2 = e2, ... }'
struct StructDefExpr : public Expr {
  // Either a DeclRefExpr or a MemberExpr.
  // @Improve: Technically, we might be able to just have this as a Name. It
  // doesn't make sense all the intermediate Exprs in a MemberExpr has to have
  // an associated Type.
  DeclRefExpr *name_expr;
  std::vector<StructDefTerm> terms;

  StructDefExpr(DeclRefExpr *dre, const std::vector<StructDefTerm> &t)
      : Expr(Expr::struct_def), name_expr(dre), terms(t) {}
};

// 'struct.mem'
struct MemberExpr : public Expr {
  // 'struct' part; this is a general Expr because things like func().mem
  // should be possible.
  Expr *parent_expr = nullptr;
  Name *member_name = nullptr; // 'mem' part

  // Back-reference to the FieldDecl, used for querying the byte offset of
  // the field.  Only valid for struct fields.
  FieldDecl *field_decl = nullptr;

  // MemberExprs may or may not have an associated VarDecl, depending on
  // 'struct_expr' being l-value or r-value.

  MemberExpr(Expr *parent, Name *member)
      : Expr(Expr::member), parent_expr(parent), member_name(member) {}
};

// 'array[index]'
struct SubscriptExpr : public Expr {
  Expr *array_expr = nullptr;
  Expr *index_expr = nullptr;

  SubscriptExpr(Expr *array, Expr *index)
      : Expr(Expr::subscript), array_expr(array), index_expr(index) {}
};

// '[type](expr)'
struct CastExpr : public Expr {
  Expr *type_expr = nullptr;
  Expr *target_expr = nullptr;

  CastExpr(Expr *type, Expr *target)
      : Expr(Expr::cast), type_expr(type), target_expr(target) {}
};

struct UnaryExpr : public Expr {
  const enum Kind {
    paren,
    ref,
    var_ref,
    deref,
    plus,  // TODO
    minus, // TODO
  } kind;

  Expr *operand;

  UnaryExpr(Kind k, Expr *oper) : Expr(Expr::unary), kind(k), operand(oper) {}
};

struct BinaryExpr : public Expr {
  Expr *lhs;
  Token op;
  Expr *rhs;

  BinaryExpr(Expr *lhs_, Token op_, Expr *rhs_)
      : Expr(Expr::binary), lhs(lhs_), op(op_), rhs(rhs_) {
    loc = lhs->loc;
  }
};

struct TypeExpr : public Expr {
  TypeKind kind = TypeKind::atom;
  // Name of the type. TODO: should this contain '&' and '[]'?
  Name *name = nullptr;
  // Expr's 'decl' is the Decl object that represents this type.

  // Is this type mutable?
  bool mut = false;
  // E.g., 'T' part of '*T'.  It is Expr rather than TypeExpr mainly so that
  // it can store BadExpr.  XXX dirty.
  Expr *subexpr = nullptr;

  // TODO: incomplete.
  TypeExpr(TypeKind k, Name *n, bool m, Name *lt, Expr *se)
      : Expr(Expr::type_), kind(k), name(n), mut(m), subexpr(se) {}
};

struct BadExpr : public Expr {
  BadExpr() : Expr(Expr::bad) {}
};

template <> inline bool Expr::is<IntegerLiteral>() const {
  return kind == integer_literal;
}
template <> inline bool Expr::is<StringLiteral>() const {
  return kind == string_literal;
}
template <> inline bool Expr::is<DeclRefExpr>() const {
  return kind == decl_ref;
}
template <> inline bool Expr::is<CallExpr>() const { return kind == call; }
template <> inline bool Expr::is<StructDefExpr>() const {
  return kind == struct_def;
}
template <> inline bool Expr::is<CastExpr>() const { return kind == cast; }
template <> inline bool Expr::is<MemberExpr>() const { return kind == member; }
template <> inline bool Expr::is<SubscriptExpr>() const {
  return kind == subscript;
}
template <> inline bool Expr::is<UnaryExpr>() const { return kind == unary; }
template <> inline bool Expr::is<BinaryExpr>() const { return kind == binary; }
template <> inline bool Expr::is<TypeExpr>() const { return kind == type_; }
template <> inline bool Expr::is<BadExpr>() const { return kind == bad; }
template <typename T> inline bool Expr::is() const {
  assert(!"unimplemented is()");
}

// Declarations
// ============

// A declaration node.
//
// Decl is different from Type in that it stores metadatas that are unique to
// each instances of that Type, e.g. initializing expressions for VarDecls, etc.
//
// All types that derive from Decl has a pointer field called 'name'. The value
// of this pointer serves as a unique integer ID used as the key the symbol
// table.
struct Decl : public AstNode {
  const enum Kind {
    var,
    func,
    field,
    struct_,
    enum_variant,
    enum_,
    bad,
  } kind;
  Name *name = nullptr;
  // Might be null for symbols that do not have an associated Type like
  // functions.
  Type *type = nullptr;

  Decl(Kind d) : Decl(d, nullptr, nullptr) {}
  Decl(Kind d, Name *n) : Decl(d, n, nullptr) {}
  Decl(Kind d, Name *n, Type *t)
      : AstNode(AstNode::decl), kind(d), name(n), type(t) {}
  template <typename T> bool is() const;
};

// Variable declaration.
struct VarDecl : public Decl {
  // Whether this VarDecl has been declared as a local variable, as a field
  // inside a struct, or as a parameter for a function.
  // TODO: necessary?
  const enum Kind {
    local_,
    struct_,
    param,
  } kind = local_;

  // TypeExpr of the variable.  Declared as Expr to accommodate for BadExpr.
  // TODO: Ugly.
  Expr *type_expr = nullptr;

  // Assignment expression specified at the point of declaration, if any.
  Expr *assign_expr = nullptr;

  // Mutability of the variable.
  bool mut = false;

  // Whether this variable is a function-local variable.
  bool local = false;

  // ID of the VarDecl that is local inside the current stack frame (~current
  // function).  Used for codegen to easily designate the address to which we
  // have to store the temporary value of an expression.
  long frame_local_id = 0;

  // Decls for each of the values that are associated to this value.
  // For example, if this value is a struct type, these would be VarDecls for
  // each of its field.
  //
  // Note that these are different from the 'fields' field of StructDecl:
  // while they are simply the definitions of the struct fields, these
  // represent the instantiated entities that consumes actual space in the
  // memory.
  std::vector<VarDecl *> children;

  VarDecl(Name *n, Kind k, Expr *texpr, Expr *assign)
      : Decl(Decl::var, n), kind(k), type_expr(texpr), assign_expr(assign) {}
  VarDecl(Name *n, Type *t, bool mut) : Decl(Decl::var, n, t), mut(mut) {}

  VarDecl *findMemberDecl(const Name *member_name) const;
};

// Function declaration.  There is no separate function definition: functions
// should always be defined whenever they are declared.
// Note that FuncDecl doesn't have a related type; only its ret_type will be
// set here.
struct FuncDecl : public Decl {
  VarDecl *struct_param = nullptr; // struct parameter for methods
  StructDecl *target_struct =
      nullptr; // target struct that this method is implemented in. Should
               // be null for freestanding functions.
               // @Improve: don't really want to keep this as a member.
  bool extern_ = false; // whether this function has an external linkage
  std::vector<VarDecl *> params; // list of parameters
  CompoundStmt *body = nullptr;  // body statements
  Expr *ret_type_expr = nullptr; // return type expression
  Type *ret_type = nullptr;      // return type of the function

  FuncDecl(Name *n) : Decl(Decl::func, n) {}
  size_t args_count() const { return params.size(); }
};

struct FieldDecl : public Decl {
  Expr *type_expr = nullptr;

  // Byte offset of this field in the struct.
  long offset = 0;

  FieldDecl(Name *n, Expr *texpr) : Decl(Decl::field, n), type_expr(texpr) {}
};

// Struct declaration.
struct StructDecl : public Decl {
  std::vector<FieldDecl *> fields; // FIXME: unneeded with decl_table.
  // Decl table that stores methods, fields(TODO), etc.
  ScopedTable<Name *, Decl *> decl_table;

  // By a multiple of how much the start address for this struct should be
  // aligned to.
  uint64_t alignment;

  StructDecl(Name *n) : Decl(Decl::struct_, n) {}
};

// A variant type in an enum.
struct EnumVariantDecl : public Decl {
  std::vector<Expr *> fields; // type of the fields

  EnumVariantDecl(Name *n, std::vector<Expr *> f)
      : Decl(Decl::enum_variant, n), fields(f) {}
};

// Enum declaration.
struct EnumDecl : public Decl {
  std::vector<EnumVariantDecl *> variants; // variants

  EnumDecl(Name *n, std::vector<EnumVariantDecl *> m)
      : Decl(Decl::enum_, n), variants(m) {}
};

struct BadDecl : public Decl {
  BadDecl() : Decl(Decl::bad) {}
};

template <> inline bool Decl::is<VarDecl>() const { return kind == var; }
template <> inline bool Decl::is<FuncDecl>() const { return kind == func; }
template <> inline bool Decl::is<FieldDecl>() const { return kind == field; }
template <> inline bool Decl::is<StructDecl>() const { return kind == struct_; }
template <> inline bool Decl::is<EnumVariantDecl>() const {
  return kind == enum_variant;
}
template <> inline bool Decl::is<EnumDecl>() const { return kind == enum_; }
template <> inline bool Decl::is<BadDecl>() const { return kind == bad; }
template <typename T> inline bool Decl::is() const {
  assert(!"unimplemented is()");
}

template <> inline bool AstNode::is<File>() const { return kind == file; }
template <> inline bool AstNode::is<Stmt>() const { return kind == stmt; }
template <> inline bool AstNode::is<Decl>() const { return kind == decl; }
template <> inline bool AstNode::is<Expr>() const { return kind == expr; }
template <typename T> inline bool AstNode::is() const {
  if (is<Stmt>()) {
    return static_cast<const Stmt *>(this)->is<T>();
  } else if (is<Decl>()) {
    return static_cast<const Decl *>(this)->is<T>();
  } else if (is<Expr>()) {
    return static_cast<const Expr *>(this)->is<T>();
  } else {
    return false;
  }
}

} // namespace cmp

#endif
