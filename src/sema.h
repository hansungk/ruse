#ifndef CMP_SEMA_H
#define CMP_SEMA_H

#include "ast.h"
#include "error.h"
#include "fmt/core.h"
#include "scoped_table.h"
#include <memory>

namespace cmp {

inline void unreachable() { assert(false && "unreachable"); }

struct Context {
  // Current enclosing decls.
  std::vector<FuncDecl *> func_stack;
  std::vector<EnumDecl *> enum_decl_stack;
  // Builtin types.
  // The void type exists to differentiate the type of FuncCallExprs whose
  // function no return values, from expressions that failed to typecheck and
  // have their type field left as nullptr.
  Type *ty_void = nullptr;
  Type *ty_int = nullptr;
  Type *ty_int64 = nullptr;
  Type *ty_char = nullptr;
  Type *ty_string = nullptr;
  // Used for type checking built-in functions, e.g. "arr = alloc(42)".
  Type *ty_incomplete = nullptr;

  FuncDecl *fn_alloc = nullptr;
};

// Maps a VarDecl to its borrow count in the current scope.
// To be stored inside a ScopedTable.
struct BorrowMap {
  // FIXME: unused.
  const VarDecl *decl = nullptr;

  // Number of occasions that this variable was borrowed.
  int immutable_borrow_count = 0;
  int mutable_borrow_count = 0;
};

struct BasicBlock {
  std::vector<Stmt *> stmts;
  std::vector<BasicBlock *> pred;
  std::vector<BasicBlock *> succ;
  bool walked = false;

  // Indicates whether it is guaranteed that a return statement is seen on
  // every possible control flow that leads to this basic block.
  bool returned_so_far = false;

  // True if this basic block contains a return statement.
  bool returns() const;

  // Walk and enumerate all children nodes and itself in post-order.
  // Used to implement the the reverse post-order traversal.
  void enumerate_postorder(std::vector<BasicBlock *> &walkList);
};

class Parser;

class Lifetime {
public:
  // There are two kinds of lifetimes:
  // 1. Exact lifetimes.
  // 2. Annotated lifetimes.
  // (TODO doc)
  enum { exact, annotated } kind;

  // Declaration that first introduced this Exact lifetime.
  Decl *decl = nullptr;

  // Annotation of the Annotated lifetimes.
  Name *lifetime_annot = nullptr;

  Lifetime(Decl *d) : kind(exact), decl(d) {}
  Lifetime(Name *a) : kind(annotated), lifetime_annot(a) {}
};

// Stores all of the semantic information necessary for semantic analysis
// phase.
struct Sema {
  const Source &source; // source text
  NameTable name_table; // name table

  // Memory pools.  Currently maintains simply a list of malloc()ed pointers
  // for batch freeing.
  std::vector<std::unique_ptr<AstNode>> node_pool;
  std::vector<Type *> type_pool;

  // Declarations visible at the current scope, keyed by their Names.
  ScopedTable<Name *, Decl *> decl_table;

  struct DeclTableScope {
    Sema &sema;

    DeclTableScope(Sema &s) : sema(s) { sema.decl_table.scope_open(); }
    ~DeclTableScope() { sema.decl_table.scope_close(); }
  };

  // XXX: needed?
  ScopedTable<Name *, Type *> type_table;

  // Stores lifetimes that are alive at the current position.
  // Note that this variable is not meant to be used directly; use
  // start_lifetime*() functions to create lifetimes instead.
  ScopedTable<Lifetime *, Lifetime *> lifetime_table;

  // TODO: doc
  ScopedTable<const VarDecl *, BorrowMap> borrow_table;

  // TODO: organize.
  Context context;

  // List of generated errors.
  std::vector<Error> &errors;
  // List of error beacons found in the source text.
  std::vector<Error> &beacons;

  Sema(const Source &s, std::vector<Error> &e, std::vector<Error> &b)
      : source(s), errors(e), beacons(b) {}
  Sema(const Sema &) = delete;
  Sema(Sema &&) = delete;
  ~Sema();

  void scope_open();
  void scope_close();

  template <typename T, typename... Args> T *make_node(Args &&...args) {
    node_pool.emplace_back(new T{std::forward<Args>(args)...});
    return static_cast<T *>(node_pool.back().get());
  }
  template <typename T, typename... Args>
  T *make_node_pos(size_t pos, Args &&...args) {
    auto node = make_node<T>(std::forward<Args>(args)...);
    auto ast_node = static_cast<AstNode *>(node);
    ast_node->pos = pos;
    ast_node->loc = source.locate(pos);
    return node;
  }
};

void setup_builtin(Sema &s);

bool declare_in_struct(StructDecl *struct_decl, Name *name, Decl *decl);
bool declare(Sema &sema, Decl *decl);
Type *get_derived_type(Sema &sema, TypeKind kind, Type *base_type);
bool check(Sema &sema, AstNode *n);

// Intended to be passed by value.
struct QbeValue {
  enum Kind {
    temp,
    address,
    global,
  } kind;
  int id = 0;
  std::string name = "";

  std::string format() const {
    switch (kind) {
    case Kind::temp:
      return fmt::format("%_{}", id);
    case Kind::address:
      return fmt::format("%a{}", id);
    case Kind::global:
      return fmt::format("${}", name);
    default:
      assert(false);
    }
  }
};

// Valstack is a means for AST nodes to communicate information in the codegen
// stage.  It maintains the handling ID of a value that is produced by a node
// to a FIFO stack, so that later nodes can use that value by generating loads
// or arithmetic instructions with it.  This ID could otherwise be kept in the
// nodes themselves, but that will make the access expensive.
struct Valstack {
  std::vector<QbeValue> buf;
  int next_id = 0;

  QbeValue make_temp() {
    auto id = next_id;
    next_id++;
    return QbeValue{.kind = QbeValue::temp, .id = id};
  }
  QbeValue make_addr() {
    auto id = next_id;
    next_id++;
    return QbeValue{.kind = QbeValue::address, .id = id};
  }

  // Push a value as a temporary variable in QBE.  This will show up as "%_0"
  // in the IL. This function does not take any argument because the actual
  // value is emitted to the code.
  void push_temp() {
    buf.push_back(QbeValue{.kind = QbeValue::temp, .id = next_id});
    next_id++;
  }
  void push_temp(QbeValue val) {
    buf.push_back(val);
  }

  // Push a value in the form of its memory address.  Larger sized types such
  // as structs cannot be emitted as a QBE temporary, and this is the only
  // way to emit its value.  This will show up as "%a0" in the IL. This does
  // not take any argument because the actual address is emitted to the code.
  void push_address() {
    buf.push_back(QbeValue{.kind = QbeValue::address, .id = next_id});
    next_id++;
  }
  void push_address(QbeValue addr) {
    buf.push_back(addr);
  }

  // Push a global value that is defined in the data {} section.
  void push_global(const std::string &name) {
    buf.push_back(
        QbeValue{.kind = QbeValue::global, .id = next_id, .name = name});
    next_id++;
  }

  // Push a value in the form of its memory address.  Explicitly give the tag
  // id of the address.
  void push_address_explicit(int id) {
    buf.push_back(QbeValue{.kind = QbeValue::address, .id = id});
  }

  QbeValue peek() const { return buf.back(); }

  QbeValue pop() {
    assert(!buf.empty());
    auto v = peek();
    buf.pop_back();
    return v;
  }
};

struct QbeGen {
  Sema &sema;
  Valstack stack;
  int label_id = 0;
  int ifelse_label_id = 0;
  int indent = 0;
  FILE *file;

  // Code, Annot: use them like Code{<args to fmt::print>} and pass them to
  // emitAnnotated.
  struct Code {
    std::string str;
    template <typename... Args> Code(Args &&...args) {
      str = fmt::format(std::forward<Args>(args)...);
    }
  };

  struct Annot {
    std::string str;
    template <typename... Args> Annot(Args &&...args) {
      str = fmt::format(std::forward<Args>(args)...);
    }
  };

  QbeGen(Sema &s, const char *filename) : sema{s} {
    file = fopen(filename, "w");
  }
  ~QbeGen() { fclose(file); }
  // Emit on a new line, starting with indentation.
  template <typename... Args> void emitnl(Args &&...args) {
    fmt::print(file, "\n{:{}}", "", indent);
    fmt::print(file, std::forward<Args>(args)...);
  }
  // Emit continually at the end of the current line.
  template <typename... Args> void emit(Args &&...args) {
    fmt::print(file, std::forward<Args>(args)...);
  }
  // Annotate the last emitted QBE line.
  template <typename... Args> void annotate(Args &&...args) {
    fmt::print(file, "			# ");
    fmt::print(file, std::forward<Args>(args)...);
  }
  struct IndentBlock {
    QbeGen &c;
    IndentBlock(QbeGen &c) : c{c} { c.indent += 4; }
    ~IndentBlock() { c.indent -= 4; }
  };

  void emit_assignment(const Decl *lhs, Expr *rhs);
  QbeValue emit_stack_alloc(const Type *type, size_t line, std::string_view text);

  void codegen(AstNode *n);
  void codegen_decl(Decl *d);
  void codegen_expr_value(Expr *e);
  void codegen_expr_address(Expr *e);
  void codegen_expr_explicit(Expr *e, bool value);
  void codegen_stmt(Stmt *s);
  void codegen_data_section();
};

} // namespace cmp

#endif
