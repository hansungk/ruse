#include "parse.h"
#include "ast.h"
#include "fmt/core.h"
#include <cassert>

namespace cmp {

Parser::Parser(Lexer &l, Sema &sema) : lexer{l}, sema(sema) {
    // insert keywords in name table
    for (auto m : keyword_map)
        sema.name_table.push(m.first);

    // set up lookahead and cache
    next();
}

void Parser::error(const std::string &msg) {
    auto srcloc = locate();
    sema.errors.push_back({srcloc, msg});
    fmt::print(stderr, "{}:{}:{}: {}\n", srcloc.filename, srcloc.line,
               srcloc.col, msg);
    exit(EXIT_FAILURE);
}

void Parser::error_expected(const std::string &msg) {
    std::string s = fmt::format("expected {}, found '{}'", msg, tok.str());
    error(s);
}

void Parser::next() {
    if (tok.kind == Token::eos)
        return;

    // update cache if necessary
    if (next_read_pos == token_cache.size()) {
        auto t = lexer.lex();
        token_cache.push_back(t);
    }

    last_tok_endpos = tok.endPos();
    tok = token_cache[next_read_pos];
    next_read_pos++;
}

Parser::State Parser::save_state() {
    return State{tok, last_tok_endpos, next_read_pos, sema.errors.size()};
}

void Parser::restore_state(State state) {
    tok = state.tok;
    last_tok_endpos = state.last_tok_endpos;
    next_read_pos = state.next_read_pos;
    sema.errors.resize(state.error_count);
}

// Returns true if match succeeded, false otherwise.
bool Parser::expect(Token::Kind kind, const std::string &msg = "") {
    if (tok.kind != kind) {
        std::string s = msg;
        if (msg.empty()) {
            s = fmt::format(
                "expected '{}', found '{}'", tokenTypeToString(kind),
                std::string{tok.start,
                            static_cast<size_t>(tok.end - tok.start)});
        }
        error(s);
        // Don't make progress if the match failed.
        // Note: the Go compiler does otherwise. Is that necessary?
        return false;
    }
    next();
    return true;
}

// Assumes that comments can only come at the end of a line, i.e. it considers
// only the line '//' comments.
bool Parser::is_end_of_stmt() const {
    return tok.kind == Token::newline || tok.kind == Token::comment;
}

bool Parser::is_eos() const { return tok.kind == Token::eos; }

// Parse a statement.
//
// Stmt:
//     Decl
//     Expr
Stmt *Parser::parse_stmt() {
    Stmt *stmt = nullptr;

    if (tok.kind == Token::lbrace) {
        stmt = parse_compound_stmt();
    } else if (tok.kind == Token::kw_return) {
        stmt = parse_return_stmt();
    } else if (tok.kind == Token::kw_if) {
        stmt = parse_if_stmt();
    } else if (tok.kind == Token::hash) {
        stmt = parse_builtin_stmt();
    } else if (is_start_of_decl()) {
        stmt = parse_decl_stmt();
    } else {
        stmt = parse_expr_or_assign_stmt();
    }
    skip_newlines();

    return stmt;
}

Stmt *Parser::parse_return_stmt() {
    auto pos = tok.pos;

    expect(Token::kw_return);

    // optional
    Expr *expr = nullptr;
    if (!is_end_of_stmt()) {
        expr = parse_expr();
    }
    if (!is_end_of_stmt()) {
        skip_until_end_of_line();
        expect(Token::newline);
        return sema.make_node_pos<BadStmt>(pos);
    }
    skip_until_end_of_line();
    expect(Token::newline);
    return sema.make_node_pos<ReturnStmt>(pos, expr);
}

// Simplest way to represent the if-elseif-else chain is to view the else-if
// clause as simply a separate if statement that is embedded under the else
// statement.
IfStmt *Parser::parse_if_stmt() {
    auto pos = tok.pos;

    expect(Token::kw_if);

    Expr *cond = parse_expr();
    CompoundStmt *cstmt = parse_compound_stmt();

    IfStmt *elseif = nullptr;
    CompoundStmt *cstmt_false = nullptr;

    if (tok.kind == Token::kw_else) {
        next();

        if (tok.kind == Token::kw_if) {
            elseif = parse_if_stmt();
        } else if (tok.kind == Token::lbrace) {
            cstmt_false = parse_compound_stmt();
        } else {
            expect(Token::lbrace);

            // do our best to recover
            parse_expr();
            if (tok.kind == Token::lbrace) {
                cstmt_false = parse_compound_stmt();
            } else {
                skip_to_next_line();
            }
        }
    }

    return sema.make_node_pos<IfStmt>(pos, cond, cstmt, elseif, cstmt_false);
}

// This could be 'let a = ...' or a 'struct { ... }'; look into parseDecl()
// (FIXME doc).
DeclStmt *Parser::parse_decl_stmt() {
    VarDecl *var_decl = nullptr;

    switch (tok.kind) {
    case Token::kw_let:
        next();
        var_decl = parse_var_decl(VarDecl::local_);
        break;
    case Token::kw_var:
        next();
        var_decl = parse_var_decl(VarDecl::local_);
        var_decl->mut = true;
        break;
    default:
        assert(!"not implemented");
    }

    if (!is_end_of_stmt()) {
        // XXX: remove bad check
        if (var_decl && static_cast<Decl *>(var_decl)->kind != Decl::bad) {
            expect(Token::newline);
        }
        // try to recover
        skip_until_end_of_line();
    }
    return sema.make_node<DeclStmt>(var_decl);
}

// Upon seeing an expression, we don't know yet if it is a simple expression
// statement or an assignment statement until we see the '=' token and the RHS.
// This function handles both cases in one go.
Stmt *Parser::parse_expr_or_assign_stmt() {
    auto pos = tok.pos;

    auto lhs = parse_expr();
    // ExprStmt: expression ends with a newline
    if (is_end_of_stmt()) {
        skip_until_end_of_line();
        expect(Token::newline);
        return sema.make_node<ExprStmt>(lhs);
    }

    bool move = false;
    // AssignStmt: expression is followed by '=' or '<-'
    // (anything else is treated as an error)
    if (tok.kind == Token::reversearrow) {
        move = true;
        next();
    } else if (!expect(Token::equals, "expected '=' or '\\n' after expression")) {
        skip_until_end_of_line();
        expect(Token::newline);
        return sema.make_node_pos<BadStmt>(pos);
    }

    // At this point, it becomes certain that this is an assignment statement,
    // and so we can safely unwrap for RHS.
    auto rhs = parse_expr();
    return sema.make_node_pos<AssignStmt>(pos, lhs, rhs, move);
}

// Compound statement is a scoped block that consists of multiple statements.
//
// CompoundStmt:
//     { Stmt* }
CompoundStmt *Parser::parse_compound_stmt() {
    expect(Token::lbrace);
    auto compound = sema.make_node<CompoundStmt>();

    while (!is_eos()) {
        skip_newlines();
        if (tok.kind == Token::rbrace)
            break;
        auto line = parse_toplevel();
        compound->stmts.push_back(line);
    }

    expect(Token::rbrace);
    return compound;
}

BuiltinStmt *Parser::parse_builtin_stmt() {
    auto start = tok.pos;
    skip_until_end_of_line();
    auto end = tok.pos;
    std::string_view text{lexer.source().buf.data() + start, end - start};
    return sema.make_node_pos<BuiltinStmt>(start, text);
}

static Name *push_token_to_name_table(Sema &sema, const Token tok) {
    return sema.name_table.pushlen(tok.start, tok.end - tok.start);
}

// Doesn't include 'let' or 'var'.
VarDecl *Parser::parse_var_decl(VarDecl::Kind kind) {
    auto pos = tok.pos;

    if (tok.kind != Token::ident) {
        error_expected("an identifier");
    }

    Name *name = push_token_to_name_table(sema, tok);
    next();

    VarDecl *v = nullptr;
    // '=' comes either first, or after the ': type' part.
    if (tok.kind == Token::colon) {
        next();
        auto type_expr = parse_type_expr();
        v = sema.make_node_pos<VarDecl>(pos, name, kind, type_expr, nullptr);
    }
    if (tok.kind == Token::equals) {
        next();
        auto assign_expr = parse_expr();
        if (v)
            static_cast<VarDecl *>(v)->assign_expr = assign_expr;
        else
            v = sema.make_node_pos<VarDecl>(pos, name, kind, nullptr,
                                            assign_expr);
    }
    if (!v) {
        error_expected("'=' or ':' after var name");
        v = nullptr;
    }
    return v;
}

// Parses a comma separated list of AST nodes whose type is T*.  Parser
// function for the node type should be provided as `parse_fn` so that this
// function knows how to parse the elements.
// `do_on_parse` specifies the action done on each successful parse of an
// element.  This might include declare()ing to a decl table, or pushing back
// to a vector that stores struct members.
// Doesn't account for the enclosing parentheses or braces.
template <typename T, typename F1, typename F2>
void Parser::parse_comma_separated_list(F1 &&parse_fn, F2 &&do_on_parse) {
    // With both ) and } checked, this works for both function argument lists
    // and struct fields.
    auto finishers = {Token::rparen, Token::rbrace};
    auto delimiters = {Token::comma, Token::newline, Token::rparen, Token::rbrace};

    for (;;) {
        skip_newlines();
        if (tok.is_any(finishers) || is_eos())
            break;

        T elem;
        bool success = parse_fn(elem);
        if (success) {
            do_on_parse(elem);
        }

        // Determining where a decl ends in a list is a little tricky.  Here,
        // we stop for any token that is either (1) separator tokens like comma
        // or newline, or (2) closing tokens like parentheses and braces.
        if (!success) {
            skip_until_any(delimiters);
        } else if (!tok.is_any(delimiters)) {
            // For cases where a VarDecl succeeds parsing but there is a
            // leftover token, e.g. 'a: int###', we need to directly check the
            // next token is the delimiting token, and do an appropriate error
            // report.
            error(fmt::format("trailing token '{}' after declaration",
                              tok.str()));
            skip_until_any(delimiters);
        }

        // Skip comma if any. This allows trailing comma at the end, such as
        // '(a,)'.
        if (tok.kind == Token::comma)
            next();
    }
}

FuncDecl *Parser::parseFuncDecl() {
  auto pos = tok.pos;

  bool extern_ = false;
  if (tok.kind == Token::kw_extern) {
    expect(Token::kw_extern);
    extern_ = true;
  }

  expect(Token::kw_func);

  // struct for methods
  VarDecl *method_struct = nullptr;
  if (tok.kind == Token::lparen) {
    expect(Token::lparen);
    method_struct = parse_var_decl(VarDecl::param);
    expect(Token::rparen);
  }

  // name
  Name *name = push_token_to_name_table(sema, tok);
  auto func = sema.make_node_pos<FuncDecl>(pos, name);
  func->extern_ = extern_;
  func->loc = sema.source.locate(tok.pos);
  func->struct_param = method_struct;
  next();

  // parameter list
  expect(Token::lparen);
  while (tok.kind != Token::rparen) {
    auto param = parse_var_decl(VarDecl::param);
    func->params.push_back(param);
    if (tok.kind != Token::rparen) {
      if (!expect(Token::comma))
        // recover error
        skip_until(Token::rparen);
    }
  }
  expect(Token::rparen);

  // return type
  if (extern_ && !is_end_of_stmt()) {
    func->ret_type_expr = parse_type_expr();
  } else if (!extern_ && tok.kind != Token::lbrace) {
    func->ret_type_expr = parse_type_expr();
  }

  // function body
  // extern function decls do not have a body defined.
  if (!extern_) {
    func->body = parse_compound_stmt();
  }

  return func;
}

StructDecl *Parser::parse_struct_decl() {
    auto pos = tok.pos;
    Name *name = nullptr;

    expect(Token::kw_struct);

    if (tok.kind != Token::ident) {
        error_expected("an identifier");
        skip_until(Token::lbrace);
    } else {
        name = push_token_to_name_table(sema, tok);
        next();
    }

    auto sd = sema.make_node_pos<StructDecl>(pos, name);

    expect(Token::lbrace);
    skip_newlines();
    while (tok.kind != Token::rbrace) {
        // FIXME: Creates a throwaway VarDecl.
        auto var_decl = parse_var_decl(VarDecl::struct_);
        auto field_decl = sema.make_node_pos<FieldDecl>(
            var_decl->pos, var_decl->name, var_decl->type_expr);
        sd->fields.push_back(field_decl);
        declare_in_struct(sd, field_decl->name, field_decl);
        skip_newlines();
    }
    expect(Token::rbrace, "unterminated struct declaration");

    return sd;
}

bool Parser::is_start_of_decl() {
  switch (tok.kind) {
  case Token::kw_let:
  case Token::kw_struct:
  case Token::kw_func:
  case Token::kw_extern:
    return true;
  case Token::kw_var: {
    // For var, there can be exceptions such as 'var &a'. We need to do some
    // lookahead here.
    auto s = save_state();
    next();
    if (tok.kind == Token::star) {
      restore_state(s);
      return false;
    }
    restore_state(s);
    return true;
  }
  default:
    return false;
  }
}

// Parse a declaration.
// Remember to modify is_start_of_decl() accordingly.
Decl *Parser::parse_decl() {
  assert(is_start_of_decl());

  switch (tok.kind) {
  case Token::kw_let: {
    next();
    auto v = parse_var_decl(VarDecl::local_);
    return v;
  }
  case Token::kw_var: {
    next();
    auto v = parse_var_decl(VarDecl::local_);
    v->mut = true;
    return v;
  }
  case Token::kw_struct:
    return parse_struct_decl();
  case Token::kw_func:
  case Token::kw_extern:
    return parseFuncDecl();
  default:
    assert(!"not exhaustive list of declaration start");
  }
}

Expr *Parser::parse_literal_expr() {
    Expr *expr = nullptr;
    // TODO Literals other than integers?
    switch (tok.kind) {
    case Token::number: {
        std::string s{tok.start, static_cast<size_t>(tok.end - tok.start)};
        int value = std::stoi(s);
        expr = sema.make_node_pos<IntegerLiteral>(tok.pos, value);
        break;
    }
    case Token::string:
        expr = sema.make_node_pos<StringLiteral>(
            tok.pos, std::string_view{
                         tok.start, static_cast<size_t>(tok.end - tok.start)});
        break;
    default:
        assert(false && "non-integer literals not implemented");
    }

    next();

    return expr;
}

// Upon seeing an expression that starts with an identifier, we don't know
// whether it is just a variable, a function call, or a struct name ('a' vs.
// 'a()' vs. 'a.m').  Rather than using lookahead, parse the both kinds in one
// go in this function.
Expr *Parser::parse_postfix_maybe(Expr *expr) {
  auto pos = tok.pos;

  // Then loop to see if there's any trailing . or (), expanding the LHS because
  // they are left-associative.
  while (!is_eos()) {
    if (tok.kind == Token::dot) {
      expect(Token::dot);
      assert(tok.kind == Token::ident);
      Name *member_name = push_token_to_name_table(sema, tok);
      next();
      // Collapse the expr parsed so far into the LHS of a new MemberExpr.
      expr = sema.make_node_pos<MemberExpr>(pos, expr, member_name);
    } else if (tok.kind == Token::lparen) {
      expect(Token::lparen);
      std::vector<Expr *> args;
      while (tok.kind != Token::rparen) {
        args.push_back(parse_expr());
        if (tok.kind == Token::comma)
          next();
      }
      expect(Token::rparen);
      expr = sema.make_node_pos<CallExpr>(pos, CallExpr::func, expr, args);
    } else if (tok.kind == Token::lbracket) {
      expect(Token::lbracket);
      auto index_expr = parse_expr();
      expect(Token::rbracket);
      expr = sema.make_node_pos<SubscriptExpr>(pos, expr, index_expr);
    } else {
      // Otherwise, this could be anything between a variable, a struct or a
      // function, which can only be decided in the type checking stage.
      break;
    }
  }
  return expr;
}

Expr *Parser::parse_cast_expr() {
    auto pos = tok.pos;

    expect(Token::lbracket);
    auto type_expr = parse_type_expr();
    expect(Token::rbracket);

    expect(Token::lparen);
    auto target_expr = parse_expr();
    expect(Token::rparen);

    return sema.make_node_pos<CastExpr>(pos, type_expr, target_expr);
}

// Get the Name handle that designates a reference type of a given referee type
// name.  This function is used in the typechecking phase to query
// already-declared reference types from the type table. TODO: looks kinda like
// an unnecessary step.
Name *make_name_of_derived_type(NameTable &names, TypeKind kind,
                           Name *referee_name) {
    std::string s;
    switch (kind) {
    case TypeKind::pointer:
        s = "*";
        break;
    case TypeKind::array:
        s = "[]";
        break;
    default:
        assert(!"unknown type kind");
    }
    s += referee_name->text;
    return names.push(s.c_str());
}

// Parse a type expression.  A type expression is simply every stream of tokens
// in the source that can represent a type.
//
// type-expression:
//     ('*' | '&')? '[]'? ident
Expr *Parser::parse_type_expr() {
  auto pos = tok.pos;

  if (tok.kind == Token::kw_var) {
    next();
  }

  TypeKind type_kind = TypeKind::value;
  Name *lt_name = nullptr;
  Expr *subexpr = nullptr;
  // String to push to the name table.  Push full names of the derived types
  // (e.g. *int, []int) as we come across them here on the spot.
  std::string text;
  if (tok.kind == Token::star) {
    next();
    type_kind = TypeKind::pointer;
    // Base type name
    subexpr = parse_type_expr();
    text = make_name_of_derived_type(sema.name_table, TypeKind::pointer,
                                     subexpr->as<TypeExpr>()->name)
               ->text;
  } else if (tok.kind == Token::lbracket) {
    expect(Token::lbracket);
    expect(Token::rbracket);
    type_kind = TypeKind::array;
    subexpr = parse_type_expr();
    text = make_name_of_derived_type(sema.name_table, TypeKind::array,
                                     subexpr->as<TypeExpr>()->name)
               ->text;
  } else if (is_ident_or_keyword(tok)) {
    type_kind = TypeKind::value;

    text = std::string{tok.start, static_cast<size_t>(tok.end - tok.start)};
    next();

    subexpr = nullptr;
  } else {
    error_expected("type name");
    return sema.make_node_pos<BadExpr>(pos);
  }

  Name *name = sema.name_table.push(text.c_str());

  return sema.make_node_pos<TypeExpr>(pos, type_kind, name, true, lt_name,
                                      subexpr);
}

Expr *Parser::parse_unary_expr() {
  auto pos = tok.pos;

  Expr *expr = nullptr;
  switch (tok.kind) {
  case Token::number:
  case Token::string: {
    expr = parse_literal_expr();
    break;
  }
  case Token::ident: {
    // First, just parse a single declref.
    Name *name = push_token_to_name_table(sema, tok);
    next();
    expr = sema.make_node_pos<DeclRefExpr>(pos, name)->as<Expr>();
    expr = parse_postfix_maybe(expr);
    // 'S {}'.  Comes after parsing all postfix operators of the 'S' part.
    expr = parse_structdef_maybe(expr);
    break;
  }
  case Token::star: {
    next();
    expr = parse_unary_expr();
    expr = sema.make_node_pos<UnaryExpr>(pos, UnaryExpr::deref, expr);
    break;
  }
  case Token::kw_var:
  case Token::ampersand: {
    auto kind = UnaryExpr::ref;
    if (tok.kind == Token::kw_var) {
      expect(Token::kw_var);
      kind = UnaryExpr::var_ref;
    }
    expect(Token::ampersand);
    expr = parse_unary_expr();
    expr = sema.make_node_pos<UnaryExpr>(pos, kind, expr);
    break;
  }
  case Token::lparen: {
    expect(Token::lparen);
    auto inside_expr = parse_expr();
    expect(Token::rparen);
    expr = sema.make_node_pos<UnaryExpr>(pos, UnaryExpr::paren, inside_expr);
    break;
  }
  // TODO: prefix (++), postfix, sign (+/-)
  default: {
    // Because all expressions start with a unary expression, falling here
    // means this is not an expression.
    error_expected("an expression");
    // XXX: remove use of BadExpr
    return sema.make_node_pos<BadExpr>(pos);
  }
  }

  expr = parse_postfix_maybe(expr);
  assert(expr);

  return expr;
}

namespace {

int binary_op_precedence(const Token &op) {
    switch (op.kind) {
    case Token::star:
    case Token::slash:
        return 2;
    case Token::plus:
    case Token::minus:
        return 1;
    case Token::doubleequals:
    case Token::notequals:
    case Token::greaterthan:
    case Token::lesserthan:
        return 0;
    default:
        // not an operator
        return -1;
    }
}

} // namespace

// Extend a unary expression into binary if possible, by parsing any succeeding
// RHS.  The parsing goes on as long as operators with higher than or equal to
// `precedence` are seen.  Setting it to 0 will have the function parse the
// entire binary expression, as non-operator tokens have negative precedence
// values.
Expr *Parser::parse_binary_expr_rhs(Expr *lhs, int precedence = 0) {
  Expr *root = lhs;

  while (!is_eos()) {
    int this_prec = binary_op_precedence(tok);

    // If the upcoming op has lower precedence, finish this subexpression.
    // It will be treated as a single term when this function is re-called
    // with lower precedence.
    if (this_prec < precedence)
      return root;

    Token op = tok;
    next();

    // Parse the second term.
    Expr *rhs = parse_unary_expr();

    // We do not know if this term should associate to left or right; e.g.
    // "(a * b) + c" or "a + (b * c)".  We should look ahead for the next
    // operator that follows this term.
    int next_prec = binary_op_precedence(tok);

    // If the next operator has higher precedence ("a + b * c"), parse the RHS
    // as a single subexpression with elevated minimum precedence. Else ("a * b
    // + c"), just treat it as a unary expression.
    if (this_prec < next_prec)
      rhs = parse_binary_expr_rhs(rhs, precedence + 1);

    // Create a new root with the old root as its LHS, and the recursion result
    // as RHS.  This implements left associativity.
    root = sema.make_node_pos<BinaryExpr>(root->pos, root, op, rhs);
  }

  return root;
}

bool Parser::lookahead_struct_def() {
    auto s = save_state();

    if (tok.kind != Token::lbrace) {
        goto fail;
    }

    // empty ({})
    if (tok.kind == Token::rbrace) {
        goto success;
    }
    next();

    if (tok.kind != Token::dot) {
        goto fail;
    }

success:
    restore_state(s);
    return true;

fail:
    restore_state(s);
    return false;
}

// Parse '.memb = expr' part in Struct { .m1 = e1, .m2 = e2, ... }.
// Returns false if there is no more valid term to parse.
bool Parser::parse_structdef_field(StructDefTerm &result) {
    if (!expect(Token::dot))
        return false;
    Name *name = push_token_to_name_table(sema, tok);
    next();

    if (!expect(Token::equals))
        return false;

    auto expr = parse_expr();
    if (!expr)
        return false;

    result = StructDefTerm{name, expr};
    return true;
}

// If this expression has a trailing {...}, parse as a struct definition
// expression.  If not, just pass along the original expression. This function
// is called after the struct name part is fully parsed, i.e. when looking at
// '{'.
Expr *Parser::parse_structdef_maybe(Expr *expr) {
  if (!lookahead_struct_def()) {
    return expr;
  }

  auto pos = tok.pos;

  if (expr->kind != Expr::decl_ref)
    error("qualified struct names are not yet supported");

  auto decl_ref_expr = static_cast<DeclRefExpr *>(expr);

  expect(Token::lbrace);

  std::vector<StructDefTerm> desigs;
  parse_comma_separated_list<StructDefTerm>(
      [this](StructDefTerm &result) { return parse_structdef_field(result); },
      [&](const StructDefTerm &result) { desigs.push_back(result); });

  expect(Token::rbrace);

  return sema.make_node_pos<StructDefExpr>(pos, decl_ref_expr, desigs);
}

Expr *Parser::parse_expr() {
  auto expr = parse_unary_expr();
  if (!expr) {
    return nullptr;
  }
  return parse_binary_expr_rhs(expr);
}

void Parser::skip_until(Token::Kind kind) {
    while (!is_eos() && tok.kind != kind)
        next();
}

void Parser::skip_until_any(std::initializer_list<Token::Kind> &kinds) {
    while (!is_eos() && !tok.is_any(kinds))
        next();
}

void Parser::skip_until_end_of_line() {
    while (!is_eos() && tok.kind != Token::newline)
        next();
}

// Used when the current statement turns out to be broken and we just want to
// skip this whole line.
void Parser::skip_to_next_line() {
    skip_until_end_of_line();
    expect(Token::newline);
}

// The language is newline-aware, but newlines are mostly meaningless unless
// they are at the end of a statement or a declaration.  In those cases we use
// this to skip over them.
void Parser::skip_newlines() {
    while (tok.kind == Token::newline || tok.kind == Token::comment)
        next();
}

AstNode *Parser::parse_toplevel() {
    if (is_start_of_decl()) {
        return parse_decl();
    }

    auto stmt = parse_stmt();
    if (!stmt) {
        error(fmt::format(
            "unexpected '{}' at toplevel",
            std::string{tok.start, static_cast<size_t>(tok.end - tok.start)}));
        skip_to_next_line();
        return nullptr;
    }
    return stmt;
}

File *Parser::parse_file() {
    auto file = sema.make_node<File>();

    skip_newlines();

    while (!is_eos()) {
        auto toplevel = parse_toplevel();
        if (!toplevel) {
            continue;
        }
        file->toplevels.push_back(toplevel);
        skip_newlines();
    }

    return file;
}

AstNode *Parser::parse() {
    return parse_file();
}

} // namespace cmp
