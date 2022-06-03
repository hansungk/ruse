#ifndef CMP_PARSER_H
#define CMP_PARSER_H

#include "ast.h"
#include "lex.h"
#include "sema.h"

namespace cmp {

Name *make_name_of_derived_type(NameTable &names, TypeKind kind, Name *referee_name);

class Parser {
public:
  Lexer &lexer;
  Sema &sema;

  Token tok;                                   // lookahead token
  std::vector<std::unique_ptr<AstNode>> nodes; // node pointer pool
  AstNode *ast = nullptr;                      // resulting AST

  // End position of the last consumed token.
  // Used for tracking the range of the current token.
  size_t last_tok_endpos = 0;

  // Token cache.
  // These are data structures that enable flexible roll-back of the parsing
  // state.
  // Cache of the lookahead tokens.
  std::vector<Token> token_cache;
  // Index of the token to be read in the next next() call.
  size_t next_read_pos = 0;

  struct State {
    Token tok;
    size_t last_tok_endpos;
    size_t next_read_pos;
    size_t error_count;
  };
  State save_state();
  void restore_state(State state);

  Parser(Lexer &lexer, Sema &sema);
  AstNode *parse();

private:
  // Parse the whole file.
  File *parse_file();

  // Parse a toplevel statement.
  AstNode *parse_toplevel();

  // Statement parsers.
  Stmt *parse_stmt();
  Stmt *parse_expr_or_assign_stmt();
  Stmt *parse_return_stmt();
  IfStmt *parse_if_stmt();
  DeclStmt *parse_decl_stmt();
  CompoundStmt *parse_compound_stmt();
  BuiltinStmt *parse_builtin_stmt();
  bool is_end_of_stmt() const;
  bool is_eos() const;

  // Declaration parsers
  Decl *parse_decl();
  VarDecl *parse_var_decl(VarDecl::Kind kind);
  template <typename T, typename F1, typename F2>
  void parse_comma_separated_list(F1 &&parse_fn, F2 &&push_back_fn);
  FuncDecl *parseFuncDecl();
  StructDecl *parse_struct_decl();
  bool isStartOfDecl();

  // Expression parsers
  Expr *parse_expr();
  Expr *parse_unary_expr();
  Expr *parse_literal_expr();
  Expr *parse_postfix_maybe(Expr *expr);
  Expr *parse_cast_expr();
  Expr *parse_type_expr();
  Expr *parse_binary_expr_rhs(Expr *lhs, int precedence);
  bool parse_structdef_field(StructDefTerm &result);
  bool lookahead_struct_def();
  Expr *parse_structdef_maybe(Expr *expr);

  // Error handling
  void error(const std::string &msg);
  void error_expected(const std::string &msg);

  // Advance the lookahead token.
  void next();

  // Expect and consume functions.
  bool expect(Token::Kind kind, const std::string &msg);

  // Skip until a specific token(s) show up.
  void skip_until(Token::Kind kind);
  void skip_until_any(std::initializer_list<Token::Kind> &kinds);
  void skip_until_end_of_line();
  void skip_to_next_line();
  void skip_newlines();

  // Figure out the current location (line, col) in the source.
  SourceLoc locate() const { return lexer.source().locate(tok.pos); }
};

} // namespace cmp

#endif
