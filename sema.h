#ifndef CMP_SEMA_H
#define CMP_SEMA_H

#include "ast.h"
#include "error.h"
#include "fmt/core.h"
#include "scoped_table.h"
#include <memory>
#include <utility>

namespace cmp {

inline void unreachable() { assert(false && "unreachable"); }

struct Context {
    // Current enclosing decls.
    std::vector<FuncDecl *> func_stack;
    std::vector<EnumDecl *> enum_decl_stack;
    // Builtin types.
    // voidType exists to differentiate the type of FuncCallExprs whose
    // function no return values, from expressions that failed to typecheck.
    Type *void_type = nullptr;
    Type *int_type = nullptr;
    Type *char_type = nullptr;
    Type *string_type = nullptr;
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
    std::vector<Lifetime *> lifetime_pool;
    std::vector<BasicBlock *> basic_block_pool;

    // Declarations visible at the current scope, keyed by their Names.
    ScopedTable<Name *, Decl *> decl_table;

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

    void scopeOpen();
    void scopeClose();

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
    template <typename T, typename... Args>
    T *make_node_range(std::pair<size_t, size_t> range, Args &&...args) {
        auto node = make_node<T>(std::forward<Args>(args)...);
        auto ast_node = static_cast<AstNode *>(node);
        ast_node->pos = range.first;
        ast_node->endpos = range.second;
        ast_node->loc = source.locate(range.first);
        ast_node->endloc = source.locate(range.second);
        return node;
    }
    template <typename... Args> Lifetime *make_lifetime(Args &&...args) {
        lifetime_pool.emplace_back(new Lifetime{std::forward<Args>(args)...});
        return lifetime_pool.back();
    }
    BasicBlock *makeBasicBlock() {
        BasicBlock *bb = new BasicBlock;
        basic_block_pool.push_back(bb);
        return bb;
    }
};

void setup_builtin_types(Sema &s);

bool typecheck(Sema &sema, AstNode *n);

enum class ValueKind {
    value,
    address,
};

struct Value {
    ValueKind kind;
    int id;

    std::string format() const {
        switch (kind) {
        case ValueKind::value:
            return fmt::format("%_{}", id);
        case ValueKind::address:
            return fmt::format("%a{}", id);
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
    std::vector<Value> buf;
    int next_id{0};

    // Push a value as a temporary variable in QBE.  This will be designated as
    // "%_0" in the IL.
    void pushTemp() {
        buf.push_back(Value{ValueKind::value, next_id});
        next_id++;
    }

    // Push a value in the form of its memory address.  Larger sized types such
    // as structs cannot be emitted as a QBE temporary, and this is the only
    // way to emit its value.  This will be designated as "%a0" in the IL.
    void pushAddress() {
        buf.push_back(Value{ValueKind::address, next_id});
        next_id++;
    }

    // Explicitly give the id of the value which will be reused.
    void pushAddressExplicit(int id) {
        buf.push_back(Value{ValueKind::address, id});
    }

    Value peek() const { return buf.back(); }

    Value pop() {
        assert(!buf.empty());
        auto v = peek();
        buf.pop_back();
        return v;
    }
};

struct QbeGenerator {
    // Needed to access declarations in the current scope.
    Context &context;
    Valstack valstack;
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

    QbeGenerator(Context &c, const char *filename) : context{c} {
        file = fopen(filename, "w");
    }
    ~QbeGenerator() { fclose(file); }
    template <typename... Args> void emit(Args &&...args) {
        fmt::print(file, "{:{}}", "", indent);
        fmt::print(file, std::forward<Args>(args)...);
    }
    void emitAnnotated(Code code, Annot annotation) {
        emit(code.str);
        emitSameLine("   # {}\n", annotation.str);
    }
    template <typename... Args> void emitSameLine(Args &&...args) {
        fmt::print(file, std::forward<Args>(args)...);
    }
    struct IndentBlock {
        QbeGenerator &c;
        IndentBlock(QbeGenerator &c) : c{c} { c.indent += 4; }
        ~IndentBlock() { c.indent -= 4; }
    };

    void emitAssignment(const Type *lhs_type, Expr *rhs);
    long emitStackAlloc(const Type *type);

    void codegen(AstNode *n);
    void codegenDecl(Decl *d);
    void codegenExpr(Expr *e);
    void codegenExprAddress(Expr *e);
    void codegenExprExplicit(Expr *e, bool value);
    void codegenStmt(Stmt *s);
};

} // namespace cmp

#endif
