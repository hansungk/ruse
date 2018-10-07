#include "parser.h"
#include <utility>
#include <sstream>

namespace comp {

void Parser::next() {
    tok = lexer.lex();
}

void Parser::expect(TokenType type) {
    if (tok.type != type) {
        std::stringstream ss;
        ss << "expected " << static_cast<int>(type)
           << ", got " << static_cast<int>(tok.type);
        error(ss.str());
    }
    next();
}

void Parser::expect_semi() {
    expect(TokenType::semicolon);
}

ExprPtr Parser::parse_ident() {
    if (tok.type != TokenType::ident)
        expect(TokenType::ident);
    // AstNodePtr ast = make_ast(NodeType::atom, tok);
    auto expr = std::make_unique<Expr>(Expr::literal);
    next();
    return expr;
}

ExprPtr Parser::parse_literal() {
    auto expr = std::make_unique<LiteralExpr>(tok);
    next();
    return expr;
}

ExprPtr Parser::parse_unary_expr() {
    switch (tok.type) {
    case TokenType::number:
    case TokenType::ident:
        return parse_literal();
    case TokenType::lparen: {
        expect(TokenType::lparen);
        auto expr = parse_binary_or_unary_expr();
        expect(TokenType::rparen);
        return expr;
        // error("ParenExpr not yet implemented");
    }
    default:
        error("expected a unary expression");
        return nullptr;
    }
}

ExprPtr Parser::parse_binary_or_unary_expr() {
    ExprPtr lhs = parse_unary_expr();

    // Check op
    if (!(tok.type == TokenType::star ||
          tok.type == TokenType::plus ||
          tok.type == TokenType::minus)) {
        return lhs;
    }

    Token op = tok;
    next();

    ExprPtr rhs = parse_expr();
    return std::make_unique<BinaryExpr>(lhs, op, rhs);
}

ExprPtr Parser::parse_expr() {
    return parse_binary_or_unary_expr();
}

void Parser::error(const std::string &msg) {
    auto loc = lexer.src.locate(tok.pos);
    std::cerr << lexer.src.path << ":" << loc.first << ":" << loc.second << ": ";
    std::cerr << "parse error: " << msg << std::endl;
    exit(1);
}

AstNodePtr Parser::parse() {
    ExprPtr ast = nullptr;

    while (true) {
        switch (tok.type) {
        // case TokenType::kw_assign:
        //     return parse_assign();
        // case TokenType::kw_wire:
        // case TokenType::kw_reg:
        //     return parse_netdecl();
        case TokenType::comment:
        case TokenType::semicolon:
            next();
            continue;
        default:
            return parse_expr();
        }
    }
    return ast;
}

} // namespace cp
