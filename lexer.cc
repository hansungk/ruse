#include "lexer.h"
#include "fmt/core.h"
#include <cctype>

namespace cmp {

std::string tokentype_to_string(TokenKind kind) {
    if (kind == TokenKind::newline)
        return "\\n";
    for (auto &p : symbol_map) {
        if (p.second == kind)
            return std::string{p.first};
    }
    for (auto &p : keyword_map) {
        if (p.second == kind)
            return std::string{p.first};
    }
    return "";
}

bool is_identifier_or_keyword(const Token tok) {
    return tok.kind == TokenKind::ident ||
           (tok.kind > TokenKind::KWSTART && tok.kind < TokenKind::KWEND);
}

void Lexer::step() {
    if (look < eos()) {
        // Register newline first
        if (*look == '\n') {
            line_off.push_back(pos());
        }
        look++;
    } else {
        look = eos();
    }
}

Token Lexer::lex_ident_or_keyword() {
    skip_while([](char c) { return isalnum(c) || c == '_'; });

    // Keyword lookup
    for (auto &p : keyword_map) {
        auto text = p.first;
        auto kind = p.second;

        // If the leftover source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < text.length()) {
            continue;
        }

        std::string_view sv{curr, text.length()};
        if (sv == text) {
            return make_token_with_literal(kind);
        }
    }
    // No keyword match; it's an identifier
    num_ident++;
    return make_token_with_literal(TokenKind::ident);
}

Token Lexer::lex_number() {
    skip_while(isdigit);
    return make_token_with_literal(TokenKind::number);
}

Token Lexer::lex_string() {
    step(); // skip opening "
    while (look < eos()) {
        skip_while([](char c) { return !(c == '\\' || c == '"'); });
        if (*look == '"') {
            step(); // skip closing "
            break;
        } else {
            // skip the escaped character '\x'
            step();
            step();
        }
    }
    return make_token_with_literal(TokenKind::string);
}

Token Lexer::lex_comment() {
    skip_while([](char c) { return c != '\n'; });
    auto tok = make_token_with_literal(TokenKind::comment);
    return tok;
}

Token Lexer::lex_symbol() {
    for (auto &p : symbol_map) {
        auto text = p.first;
        auto kind = p.second;

        // If the leftover source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < text.length())
            continue;

        std::string_view sv{curr, text.length()};
        if (sv == text) {
            look = curr + text.length();
            return make_token_with_literal(kind);
        }
    }
    // Match fail
    error("unrecognized token");
    return make_token(TokenKind::none);
}

const char *Lexer::lookn(long n) const {
    if (look + n < eos()) {
        return look + n;
    }
    return eos();
}

Token Lexer::make_token(TokenKind kind) {
    return Token{kind, pos()};
}

Token Lexer::make_token_with_literal(TokenKind kind) {
    std::string_view text{curr, static_cast<size_t>(look - curr)};
    return Token{kind, pos(), text};
}

Token Lexer::lex() {
    skip_whitespace();

    if (curr == eos())
        return Token{TokenKind::eos, pos()};

    Token tok;
    switch (*curr) {
    case 0:
        // TODO emit a warning
        fmt::print(stderr, "unexpected null in source\n");
        break;
    case '"':
        tok = lex_string();
        break;
    case '/':
        if (*lookn(1) == '/') {
            tok = lex_comment();
        } else {
            tok = lex_symbol(); // divide
        }
        break;
    default:
        if (std::isalpha(*curr) || *curr == '_') {
            tok = lex_ident_or_keyword();
        } else if (std::isdigit(*curr)) {
            tok = lex_number();
        } else {
            tok = lex_symbol();
        }
        break;
    }

    // Advance token start position
    curr = look;

    return tok;
}

std::vector<Token> Lexer::lex_all() {
    std::vector<Token> v;
    Token tok;
    while ((tok = lex()).kind != TokenKind::eos) {
        v.push_back(tok);
    }
    v.push_back(tok); // terminate with eos
    return v;
}

Token Lexer::peek() {
    auto save = curr;
    auto token = lex();
    curr = save;
    return token;
}

template <typename F>
void Lexer::skip_while(F &&lambda) {
    while (look < eos() && lambda(*look)) {
        step();
    }
}

void Lexer::skip_whitespace() {
    // Newline is significant because the language doesn't have semicolons.
    skip_while([](char c) { return isspace(c) && c != '\n'; });
    curr = look;
}

void Lexer::error(const std::string &msg) {
    auto loc = src.locate(pos());
    std::cout << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cout << "lex error: " << msg << std::endl;
    exit(1);
}

} // namespace cmp