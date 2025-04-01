#include "lexer.hpp"
#include <cassert>

namespace lexer {

Tokenizer::Tokenizer() {
    define_operator(Operator{ Token::Type::PLUS_EQUALS, "+=" });
    define_operator(Operator{ Token::Type::INC, "++" });
    define_operator(Operator{ Token::Type::DEC, "--" });
    define_operator(Operator{ Token::Type::LOGICAL_AND, "&&" });
    define_operator(Operator{ Token::Type::AND, "&" });
    define_operator(Operator{ Token::Type::EQUALS, "=" });
    define_operator(Operator{ Token::Type::MIN, "-" });
    define_operator(Operator{ Token::Type::PLUS, "+" });
    define_operator(Operator{ Token::Type::MUL, "*" });
    define_operator(Operator{ Token::Type::DIV, "/" });
    define_operator(Operator{ Token::Type::PAR_OPEN, "(" });
    define_operator(Operator{ Token::Type::PAR_CLOSE, ")" });
    define_operator(Operator{ Token::Type::COMMA, "," });
    define_operator(Operator{ Token::Type::BRA_OPEN, "{" });
    define_operator(Operator{ Token::Type::BRA_CLOSE, "}" });
    define_operator(Operator{ Token::Type::COLON, ":" });
    define_operator(Operator{ Token::Type::LT, "<" });
    define_operator(Operator{ Token::Type::GT, ">" });

    define_keyword(Keyword{ Token::Type::BREAK, "break" });
    define_keyword(Keyword{ Token::Type::FUNC, "func" });
    define_keyword(Keyword{ Token::Type::RETURN, "return" });
    define_keyword(Keyword{ Token::Type::IF, "if" });
    define_keyword(Keyword{ Token::Type::ELSE, "else" });
    define_keyword(Keyword{ Token::Type::FOR, "for" });
}

void Tokenizer::define_operator(const Operator& op) { m_operators.push_back(op); }

void Tokenizer::define_keyword(const Keyword& op) { m_keywords.push_back(op); }

std::vector<Token> Tokenizer::tokenize(std::string_view code) const {
    std::vector<Token> tokens;
    for(auto i = 0u; i < code.size(); ++i) {
        Token token{ .m_category = deduce_token_category(code.substr(i, 1)) };
        if(token.m_category == Token::Category::NONE) { continue; }
        for(auto j = 1; j < code.size(); ++j) {
            if(token.m_category == Token::Category::STRING && code.at(i + j) != '"') { continue; }
            const auto cat = deduce_token_category(code.substr(i + j, 1));
            if((cat != token.m_category && !(token.m_category == Token::Category::UNRESOLVED && cat == Token::Category::NUMBER)) // allows variables with numbers in them
               || (cat == Token::Category::OPERATOR && !try_get_operator(code.substr(i, j + 1))) // splits operators (they are the same category, but --- should be dec and min)
               || (token.m_category == Token::Category::STRING && token.m_category == cat)) {
                token.m_value = code.substr(i, j);
                if(token.m_category == Token::Category::STRING) {
                    token.m_value = code.substr(i + 1, j - 1);
                    ++i;
                }
                deduce_token_type(token);
                tokens.push_back(token);
                token = Token{};
                i += j - 1;
                break;
            }
        }
    }
    return tokens;
}

const Operator* Tokenizer::try_get_operator(std::string_view value) const {
    auto it =
        std::find_if(m_operators.begin(), m_operators.end(), [value](const Operator& e) { return e.m_value == value; });
    if(it == m_operators.end()) { return nullptr; }
    return &*it;
}

const Keyword* Tokenizer::try_get_keyword(std::string_view value) const {
    auto it = std::find_if(m_keywords.begin(), m_keywords.end(), [value](const Keyword& e) { return e.m_value == value; });
    if(it == m_keywords.end()) { return nullptr; }
    return &*it;
}

bool Tokenizer::is_white_space(char c) const { return c == ' ' || c == '\t' || c == '\n'; }

Token::Category Tokenizer::deduce_token_category(std::string_view value) const {
    if(value.empty()) { return Token::Category::NONE; }
    if(is_white_space(value.at(0)) || value.at(0) == '\0') {
        return Token::Category::NONE;
    } else if(value.at(0) == ';') {
        return Token::Category::TERMINATOR;
    } else if(try_get_operator(value)) {
        return Token::Category::OPERATOR;
    } else if(value.starts_with('"')) {
        return Token::Category::STRING;
    } else if(std::isdigit(value.at(0)) || value.starts_with('.')) {
        return Token::Category::NUMBER;
    } else {
        return Token::Category::UNRESOLVED; // could be variable, keyword, func definition, call
    }
}

void Tokenizer::deduce_token_type(Token& token) const {
    std::string_view value = token.m_value;
    switch(token.m_category) {
    case Token::Category::UNRESOLVED: {
        auto type = try_get_keyword(value);
        if(type) {
            token.m_category = Token::Category::KEYWORD;
            token.m_type = type->m_type;
            return;
        }
        token.m_category = Token::Category::VARIABLE;
        token.m_type = Token::Type::IDENTIFIER;
        return;
    }
    case Token::Category::NUMBER: {
        if(value.find(".") != std::string::npos) {
            token.m_type = Token::Type::DOUBLE;
        } else {
            token.m_type = Token::Type::INT;
        }
        return;
    }
    case Token::Category::STRING: {
        token.m_type = Token::Type::STRING;
        return;
    }
    case Token::Category::OPERATOR: {
        auto type = try_get_operator(value);
        if(type) {
            token.m_type = type->m_type;
            return;
        }
        assert(false); // todo: consider throwing here
        return;
    }
    case Token::Category::TERMINATOR: {
        token.m_type = Token::Type::TERMINATOR;
        return;
    }
    default: {
        assert(false);
    }
    }
}

} // namespace lexer
