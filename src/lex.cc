#include "lex.h"
#include "fmt/core.h"
#include <cctype>

namespace cmp {

std::string tokenTypeToString(Token::Kind kind) {
    if (kind == Token::newline)
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

bool is_ident_or_keyword(const Token tok) {
    return tok.kind == Token::ident ||
           (tok.kind > Token::KWSTART && tok.kind < Token::KWEND);
}

bool Token::is_any(std::initializer_list<Token::Kind> &kinds) const {
    for (auto cand : kinds)
        if (kind == cand)
            return true;
    return false;
}

const char *tokstr(const Token tok, char *buf, size_t len) {
    if (tok.end - tok.start >= (long)len) {
        fprintf(stderr, "toksrc buffer too small");
        exit(1);
    }
    // memcpy(buf, tok.pos, len);
    buf[len - 1] = '\0';
    return buf;
}

std::string Token::str() const {
    if (kind == Token::newline)
        return {"\\n"};
    else if (kind == Token::eos)
        return {"EOF"};
    return std::string{start, static_cast<size_t>(end - start)};
}

// Advances 'look', not 'curr'. 'curr' is used as a manual marking position for
// each token start.
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
        const char *text = p.first;
        Token::Kind kind = p.second;

        // If the remaining source text is shorter than the keyword, skip it.
        if (static_cast<size_t>(eos() - curr) < strlen(text)) {
            continue;
        }

        std::string_view sv{curr, static_cast<size_t>(look - curr)};
        if (sv == text) {
            return make_token_with_literal(kind);
        }
    }
    // No keyword match; it's an identifier
    num_ident++;
    return make_token_with_literal(Token::ident);
}

Token Lexer::lex_number() {
    skip_while(isdigit);
    return make_token_with_literal(Token::number);
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
    return make_token_with_literal(Token::string);
}

Token Lexer::lex_comment() {
    skip_while([](char c) { return c != '\n'; });
    auto tok = make_token_with_literal(Token::comment);
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
    return make_token(Token::none);
}

const char *Lexer::lookn(long n) const {
    if (look + n < eos()) {
        return look + n;
    }
    return eos();
}

Token Lexer::make_token(Token::Kind kind) { return Token{kind, pos()}; }

Token Lexer::make_token_with_literal(Token::Kind kind) {
    return Token{kind, pos(), curr, look};
}

Token Lexer::lex() {
    skip_whitespace();

    if (curr == eos()) {
        return Token{Token::eos, pos()};
    }

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
            tok = lex_symbol();
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
    while ((tok = lex()).kind != Token::eos) {
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

template <typename F> void Lexer::skip_while(F &&lambda) {
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
    printf("%s:%d:%d: ", loc.filename.c_str(), loc.line, loc.col);
    printf("lex error: %s\n", msg.c_str());
    exit(1);
}

} // namespace cmp
