#include "parser.h"
#include "fmt/core.h"
#include <cassert>
#include <regex>

namespace cmp {

template <typename T> using Res = ParserResult<T>;

// Report this error to stderr.
void ParseError::print() const {
    fmt::print(stderr, "{}:{}:{}: parse error: {}\n", loc.filename, loc.line,
               loc.col, message);
}

template <typename T>
T *ParserResult<T>::unwrap() {
    if (!success()) {
        error().report();
        exit(EXIT_FAILURE);
    }
    return ptr();
}

Parser::Parser(Lexer &lexer) : lexer{lexer} {
    tok = lexer.lex();
    // insert keywords in name table
    for (auto m : keyword_map)
        names.get_or_add(std::string{m.first});
}

void Parser::error_expected(const std::string &msg) {
    std::string s = fmt::format("expected {}, found '{}'", msg, tok);
    add_error(s);
}

// In the course of this, if an error beacon is found in the comment, add the
// error to the parser error list so that it can be compared to the actual
// errors later in the verifying phase.
void Parser::next() {
    if (tok.kind != TokenKind::eos) {
        tok = lexer.lex();
        if (tok.kind == TokenKind::comment) {
            std::string_view marker{"[error:"};
            auto found = tok.text.find(marker);
            if (found != std::string_view::npos) {
                auto bracket = tok.text.substr(found);
                Source s{std::string{bracket}};
                Lexer l{s};
                Parser p{l};
                auto v = p.parse_error_beacon();
                // override location
                for (auto &e : v) {
                    e.loc = locate();
                    beacons.push_back(e);
                }
            }
        }
    }
}

bool Parser::expect(TokenKind kind, const std::string &msg = "") {
    if (tok.kind != kind) {
        std::string s = msg;
        if (msg.empty())
            s = fmt::format("expected '{}', found '{}'",
                            tokentype_to_string(kind), tok);
        add_error(s);
        return false;
    }
    next();
    return true;
}

bool Parser::is_end_of_stmt() const {
    return tok.kind == TokenKind::newline || tok.kind == TokenKind::comment;
}

bool Parser::is_eos() {
    skip_newlines();
    return tok.kind == TokenKind::eos;
}

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
Stmt *Parser::parse_stmt() {
    Stmt *stmt = nullptr;

    if (tok.kind == TokenKind::kw_return) {
        stmt = parse_return_stmt();
    } else if (is_start_of_decl()) {
        stmt = parse_decl_stmt();
    } else {
        stmt = parse_expr_or_assign_stmt();
    }
    skip_newlines();

    return stmt;
}

Stmt *Parser::parse_return_stmt() {
    auto start_pos = tok.pos;

    expect(TokenKind::kw_return);

    // optional
    Expr *expr = nullptr;
    if (!is_end_of_stmt())
        expr = parse_expr();
    if (!is_end_of_stmt()) {
        skip_until_end_of_line();
        return make_node_with_pos<BadStmt>(start_pos);
    }
    return make_node_with_pos<ReturnStmt>(start_pos, expr);
}

// let a = ...
DeclStmt *Parser::parse_decl_stmt() {
    auto decl = parse_decl();
    if (!is_end_of_stmt()) {
        if (decl->kind != AstKind::bad_decl)
            expect(TokenKind::newline);
        // try to recover
        skip_until_end_of_line();
    }
    return make_node<DeclStmt>(decl);
}

Stmt *Parser::parse_expr_or_assign_stmt() {
    auto start_pos = tok.pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        skip_until_end_of_line();
        return make_node<ExprStmt>(lhs);
    }

    // AssignStmt: expression is followed by equals
    // (anything else is treated as an error)
    if (!expect(TokenKind::equals)) {
        skip_until_end_of_line();
        return make_node_with_pos<BadStmt>(start_pos);
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return make_node_with_pos<AssignStmt>(start_pos, lhs,
                                          rhs);
}

// Compound statement is a scoped block that consists of multiple statements.
// There is no restriction in order such as variable declarations should come
// first, etc.
//
// CompoundStmt:
//     { Stmt* }
CompoundStmt *Parser::parse_compound_stmt() {
    expect(TokenKind::lbrace);
    auto compound = make_node<CompoundStmt>();

    while (true) {
        skip_newlines();
        if (tok.kind == TokenKind::rbrace)
            break;
        auto stmt = parse_stmt();
        compound->stmts.push_back(stmt);
    }

    expect(TokenKind::rbrace);
    return compound;
}

Decl *Parser::parse_var_decl() {
    auto start_pos = tok.pos;

    Name *name = names.get_or_add(std::string{tok.text});
    next();

    if (tok.kind == TokenKind::equals) {
        // a = expr
        expect(TokenKind::equals);
        auto assignexpr = parse_expr();
        return make_node_with_pos<VarDecl>(start_pos, name,
                                           nullptr, assignexpr);
    } else if (tok.kind == TokenKind::colon) {
        // a: type
        expect(TokenKind::colon);
        auto typeexpr = parse_typeexpr();
        return make_node_with_pos<VarDecl>(start_pos, name,
                                           typeexpr, nullptr);
    } else {
        error_expected("'=' or ':' after var name");
        return make_node_with_pos<BadDecl>(start_pos);
    }
}

// This doesn't include enclosing parentheses or braces.
std::vector<Decl *> Parser::parse_var_decl_list() {
    std::vector<Decl *> decls;

    while (true) {
        Decl *decl = nullptr;
        skip_newlines();
        if (tok.kind != TokenKind::ident)
            break;

        decl = parse_var_decl();
        decls.push_back(decl);

        if (decl->kind == AstKind::bad_decl)
            // Determining where each decl ends is a little tricky.
            // We could test for every tokens that are either (1) separator
            // tokens, i.e. comma, newline, or (2) used to enclose a decl list,
            // i.e. parentheses and braces.
            skip_until({TokenKind::comma, TokenKind::newline, TokenKind::rparen,
                        TokenKind::rbrace});
        if (tok.kind == TokenKind::comma)
            next();
    }
    skip_newlines();

    return decls;
}

StructDecl *Parser::parse_struct_decl() {
    expect(TokenKind::kw_struct);

    if (tok.kind != TokenKind::ident)
        error_expected("an identifier");
    Name *name = names.get_or_add(std::string{tok.text});
    next();

    if (!expect(TokenKind::lbrace))
        skip_until_end_of_line();
    auto fields = parse_var_decl_list();
    expect(TokenKind::rbrace, "unterminated struct declaration");
    // TODO: recover

    return make_node<StructDecl>(name, fields);
}

FuncDecl *Parser::parse_func_decl() {
    auto start_pos = tok.pos;

    expect(TokenKind::kw_fn);

    Name *name = names.get_or_add(std::string{tok.text});
    auto func = make_node<FuncDecl>(name);
    func->start_pos = tok.pos;
    next();

    // argument list
    expect(TokenKind::lparen);
    func->params = parse_var_decl_list();
    expect(TokenKind::rparen);

    // return type (-> ...)
    if (tok.kind == TokenKind::arrow) {
        next();
        func->retTypeExpr = parse_typeexpr();
    }
    if (tok.kind != TokenKind::lbrace) {
        error_expected("'->' or '{'");
        skip_until(TokenKind::lbrace);
    }
    // function body
    func->body = parse_compound_stmt();
    func->start_pos = start_pos;

    return func;
}

bool Parser::is_start_of_decl() const {
    switch (tok.kind) {
    case TokenKind::kw_let:
    case TokenKind::kw_var:
        return true;
    default:
        return false;
    }
}

// 'let a = ...'
Decl *Parser::parse_decl() {
    switch (tok.kind) {
    case TokenKind::kw_let:
        next();
        return parse_var_decl();
    default:
        assert(false && "not a start of a declaration");
    }
    // unreachable
    return nullptr;
}

UnaryExpr *Parser::parse_literal_expr() {
    UnaryExpr *expr = nullptr;
    // TODO Literals other than integers?
    switch (tok.kind) {
    case TokenKind::number: {
        std::string s{tok.text};
        int value = std::stoi(s);
        expr = make_node<IntegerLiteral>(value);
        break;
    }
    default:
        assert(false && "non-integer literals not implemented");
    }
    expr->start_pos = tok.pos;

    next();

    return expr;
}

// TODO: Add struct declaration here, e.g. Car {}
// maybe name it parse_ident_start_exprs?
Expr *Parser::parse_funccall_or_declref_expr() {
    auto start_pos = tok.pos;
    assert(tok.kind == TokenKind::ident);
    auto name = names.get_or_add(std::string{tok.text});
    next();

    if (tok.kind == TokenKind::lparen) {
        expect(TokenKind::lparen);
        std::vector<Expr *> args;
        while (tok.kind != TokenKind::rparen) {
            args.push_back(parse_expr());
            if (tok.kind == TokenKind::comma)
                next();
        }
        expect(TokenKind::rparen);
        return make_node_with_pos<FuncCallExpr>(start_pos, name, args);
    } else {
        return make_node_with_pos<DeclRefExpr>(start_pos, name);
    }
}

bool Parser::is_start_of_typeexpr() const {
    return tok.kind == TokenKind::ampersand || is_identifier_or_keyword(tok);
}

// Parse a type expression.
// A type expression is simply every stream of tokens in the source that can
// represent a type.
//
// TypeExpr:
//     '&' TypeExpr
//     'mut'? ident
Expr *Parser::parse_typeexpr() {
    auto typeexpr = make_node<TypeExpr>();

    typeexpr->start_pos = tok.pos;

    // Encode each type into a unique Name, so that they are easy to find in
    // the type table in the semantic analysis phase.
    std::string text;
    if (tok.kind == TokenKind::ampersand) {
        next();
        typeexpr->ref = true;
        typeexpr->subexpr = parse_typeexpr();
        if (typeexpr->subexpr->kind == AstKind::type_expr)
            text = "&" + static_cast<TypeExpr *>(typeexpr->subexpr)->name->text;
    } else if (is_identifier_or_keyword(tok)) {
        if (tok.kind == TokenKind::kw_mut) {
            expect(TokenKind::kw_mut);
            typeexpr->mut = true;
        }
        if (!is_identifier_or_keyword(tok)) {
            // FIXME: type name? expression?
            error_expected("type name");
            return make_node_with_pos<BadExpr>(typeexpr->start_pos);
        }
        typeexpr->ref = false;
        typeexpr->subexpr = nullptr;
        text = tok.text;
        next();
    } else {
        error_expected("type expression");
        return make_node_with_pos<BadExpr>(typeexpr->start_pos);
    }

    typeexpr->name = names.get_or_add(text);

    return typeexpr;
}

Expr *Parser::parse_unary_expr() {
    auto start_pos = tok.pos;

    switch (tok.kind) {
    case TokenKind::number:
    case TokenKind::string:
        return parse_literal_expr();
    case TokenKind::ident:
        return parse_funccall_or_declref_expr();
    case TokenKind::star: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, UnaryExpr::Deref, expr);
    }
    case TokenKind::ampersand: {
        next();
        auto expr = parse_unary_expr();
        return make_node_with_pos<UnaryExpr>(start_pos, UnaryExpr::Address, expr);
    }
    case TokenKind::lparen: {
        expect(TokenKind::lparen);
        auto expr = parse_expr();
        expect(TokenKind::rparen);
        return make_node_with_pos<UnaryExpr>(start_pos, UnaryExpr::Paren, expr);
    }
    default:
        // Because all expressions start with a unary expression, failing here
        // means no other expression could be matched either, so just do a
        // really generic report.
        error_expected("an expression");
        return make_node_with_pos<BadExpr>(start_pos);
    }
}

static int op_precedence(const Token &op) {
    switch (op.kind) {
    case TokenKind::star:
    case TokenKind::slash:
        return 1;
    case TokenKind::plus:
    case TokenKind::minus:
        return 0;
    default:
        // Not an operator
        return -1;
    }
}

// Extend a unary expression into binary if possible, by parsing any attached
// RHS.  Returns result that owns the node of the newly constructed binary
// expression.
Expr *Parser::parse_binary_expr_rhs(Expr *lhs, int precedence) {
    Expr *root = lhs;

    while (true) {
        int this_prec = op_precedence(tok);

        // If the upcoming op has lower precedence, finish this subexpression.
        // It will be treated as a single term when this function is re-called
        // with lower precedence.
        if (this_prec < precedence) {
            return root;
        }

        Token op = tok;
        next();

        // Parse the second term.
        Expr *rhs = parse_unary_expr();

        // We do not know if this term should associate to left or right;
        // e.g. "(a * b) + c" or "a + (b * c)".  We should look ahead for the
        // next operator that follows this term.
        int next_prec = op_precedence(tok);

        // If the next operator has higher precedence ("a + b * c"), evaluate
        // the RHS as a single subexpression with elevated minimum precedence.
        // Else ("a * b + c"), just treat it as a unary expression.
        if (this_prec < next_prec) {
            rhs = parse_binary_expr_rhs(rhs, precedence + 1);
        }

        // Create a new root with the old root as its LHS, and the recursion
        // result as RHS.  This implements left associativity.
        root = make_node<BinaryExpr>(root, op, rhs);
    }

    return root;
}

Expr *Parser::parse_expr() {
    auto unary = parse_unary_expr();
    if (!unary)
        return unary;
    return parse_binary_expr_rhs(unary);
}

std::vector<ParseError> Parser::parse_error_beacon() {
    expect(TokenKind::lbracket);
    expect(TokenKind::kw_error);
    expect(TokenKind::colon);

    std::vector<ParseError> v;
    v.push_back({locate(), std::string{tok.text}});
    next();

    expect(TokenKind::rbracket);
    return v;
}

// Verify errors against the error beacons embedded in the source text.
bool Parser::verify() const {
  bool success = true;
  fmt::print("\033[0;32mTEST\033[0m {}:\n", lexer.source().filename);

  size_t i = 0, j = 0;
  while (i < errors.size() && j < beacons.size()) {
    auto error = errors[i];
    auto beacon = beacons[j];
    if (error.loc.line == beacon.loc.line) {
      std::string stripped{std::cbegin(beacon.message) + 1,
                           std::cend(beacon.message) - 1};
      std::regex regex{stripped};
      if (!std::regex_search(error.message, regex)) {
        success = false;
        fmt::print("< {}\n> {}\n", error, beacon);
      }
      i++;
      j++;
    } else if (error.loc.line < beacon.loc.line) {
      success = false;
      fmt::print("< {}\n", error);
      i++;
    } else {
      success = false;
      fmt::print("> {}\n", beacon);
      j++;
    }
  }
  for (; i < errors.size(); i++) {
    success = false;
    fmt::print("< {}\n", errors[i]);
  }
  for (; j < beacons.size(); j++) {
    success = false;
    fmt::print("> {}\n", beacons[j]);
  }

  fmt::print("{} {}\n", success ? "\033[0;32mSUCCESS\033[0m" : "\033[0;31mFAIL\033[0m", lexer.source().filename);
  return success;
}

void Parser::skip_until(TokenKind kind) {
  while (tok.kind != kind)
    next();
}

void Parser::skip_until(const std::vector<TokenKind> &kinds) {
  while (true) {
    for (auto kind : kinds) {
      if (tok.kind == kind)
        return;
    }
    next();
  }
}

void Parser::skip_until_end_of_line() {
    while (!is_end_of_stmt())
        next();
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
void Parser::skip_newlines() {
    while (tok.kind == TokenKind::newline || tok.kind == TokenKind::comment)
        next();
}

AstNode *Parser::parse_toplevel() {
    skip_newlines();

    switch (tok.kind) {
    case TokenKind::kw_fn:
        return parse_func_decl();
    case TokenKind::kw_struct:
        return parse_struct_decl();
    default:
        assert(false && "unreachable");
    }
}

File *Parser::parse_file() {
    auto file = make_node<File>();
    // FIXME
    while (!is_eos()) {
        auto toplevel = parse_toplevel();
        file->toplevels.push_back(toplevel);
    }
    return file;
}

Ast Parser::parse() {
    ast = parse_file();
    return Ast{ast, names};
}

// Report errors to stdout.
void Parser::report() const {
    for (auto e : errors) {
        e.print();
    }
}

} // namespace cmp