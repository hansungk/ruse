#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include <variant>

namespace cmp {

class ParseError : public std::runtime_error {
public:
    ParseError(const std::string &msg) : std::runtime_error(msg) {}
};

class ParserError {
public:
    ParserError() {}
    ParserError(SourceLoc loc, const std::string &msg): location(loc), message(msg) {}
    // Report this error to stderr.
    void report() const;

    SourceLoc location;
    std::string message;
};

// ParserResult wraps the result of a single parse operation, i.e. the resulting
// AST node in the successful case, or an error object in the failing case. It
// also marks the position where the parse operation started.  With these
// information, it enables several helpful features:
//
//   1. it allows the caller to easily recover from the parse failure, using the
//   marked position info;
//
//   2. it enables the parser to proceed and try alternative productions
//   without being interrupted by an error generated in the failed production;
//
//   3. it allows the caller to overwrite the error with a more descriptive,
//   context-aware one.
template <typename T> class ParserResult {
public:
    // Uninitialized
    ParserResult() {}

    // Successful result
    template <typename U>
    ParserResult(P<U> ptr) : result(std::move(ptr)) {}

    // Erroneous result
    ParserResult(const ParserError &error) : result(error) {}

    // ParserResult owns the AST node, so it cannot be copied.
    ParserResult(const ParserResult<T> &res) = delete;

    // Upcasting
    template <typename U>
    ParserResult(ParserResult<U> &&res) {
        if (res.success()) {
            result = res.get_ptr();
        } else {
            result = res.get_error();
        }
    }

    ParserResult &operator=(ParserResult &&res) = default;

    /// Returns 'res', provided there were no errors; if there were, report them
    /// and cause the compiler to exit.
    P<T> unwrap();

    // Get the stored node pointer, handing over the ownership.
    P<T> get_ptr() { return std::move(std::get<P<T>>(result)); }

    // Get the stored ParserError object.
    ParserError &get_error() { return std::get<ParserError>(result); }

    // Is this result successful?
    bool success() { return std::holds_alternative<P<T>>(result); }

private:
    std::variant<P<T>, ParserError> result;
};

using StmtResult = ParserResult<Stmt>;
using DeclResult = ParserResult<Decl>;
using ExprResult = ParserResult<Expr>;

class Parser {
public:
    Parser(Lexer &lexer);

    Ast parse();

private:
    // Parse a whole file.
    ParserResult<File> parse_file();

    // Parse a toplevel statement.
    ParserResult<AstNode> parse_toplevel();

    // Statement parsers.
    P<Stmt> parse_stmt();
    P<Stmt> parse_expr_or_assign_stmt();
    P<ReturnStmt> parse_return_stmt();
    P<DeclStmt> parse_decl_stmt();
    P<CompoundStmt> parse_compound_stmt();
    bool is_end_of_stmt() const;

    // Declaration parsers.
    P<Decl> parse_decl();
    DeclResult parse_param_decl();
    std::vector<P<ParamDecl>> parse_param_decl_list();
    P<VarDecl> parse_var_decl();
    DeclResult parse_func_decl();
    bool is_start_of_decl() const;

    // Expression parsers.
    P<Expr> parse_expr();
    P<UnaryExpr> parse_literal_expr();
    P<DeclRefExpr> parse_declref_expr();
    P<TypeExpr> parse_type_expr();
    P<Expr> parse_unary_expr();
    P<Expr> parse_binary_expr_rhs(ExprPtr lhs, int precedence = 0);

    // Get the next token from the lexer.
    void next();

    // Get the current lookahead token.
    const Token look() const;

    // Get the current parsing position.
    auto get_position() const { return lookahead_pos; }
    // Restore the parsing position back to the given one.
    void restore_position(size_t pos) {
        lookahead_pos = pos;
    }

    // RAII object that when deleted restores the parser position back to the
    // point of its creation.
    class ParserPositionRAII {
    public:
        ParserPositionRAII(Parser &p) : p(p) {
            pos = p.get_position();
        }
        ~ParserPositionRAII() {
            p.restore_position(pos);
        }
        Parser &p;
        size_t pos;
    };

    // Get the precedence of an operator.
    int get_precedence(const Token &op) const;

    void expect(TokenKind kind, const std::string &msg);
    void expect_end_of_stmt();

    // Report an error and terminate.
    void error(const std::string &msg);

    // Construct a successful ParserResult, annotating it with the start
    // position.
    template <typename T> auto make_result(P<T> ptr) {
        return ParserResult<T>{std::move(ptr)};
    }

    // Construct a failed ParserResult, annotating it with an error message and
    // the start position.
    ParserError make_error(const std::string &msg) {
        return ParserError{locate(), msg};
    }

    void skip_newlines();

    // Figure out the current location (line, col) in the source.
    SourceLoc locate() const {
        return lexer.get_source().locate(look().pos);
    }

    Lexer &lexer;                           // associated lexer
    Token tok;                              // lookahead token
    NameTable name_table; // name table (TODO: document)
    std::vector<Token> tokens;              // lexed tokens
    std::vector<Token> lookahead_cache;     // lookahead tokens cache
    size_t lookahead_pos = 0;               // lookahead position in the cache
    std::vector<std::string> errors;
};

} // namespace cmp

#endif
