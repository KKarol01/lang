#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <algorithm>
#include "logger.hpp"

namespace lexer {
struct Token {
    // When adding new types or categories, remember to fill string_names arrays for them.
    enum class Category {
        NONE,
        TERMINATOR,
        UNRESOLVED,
        VARIABLE,
        NUMBER,
        STRING,
        OPERATOR,
        KEYWORD,
    };
    enum class Type {
        NONE,
        TERMINATOR,

        IDENTIFIER,
        INT,
        DOUBLE,
        STRING,

        PLUS_EQUALS,
        INC,
        DEC,
        EQUALS,
        MIN,
        PLUS,
        MUL,
        DIV,
        PAR_OPEN,
        PAR_CLOSE,
        COMMA,
        BRA_OPEN,
        BRA_CLOSE,

        BREAK,
        FUNC,
        RETURN,
    };

    std::string m_value;
    Type m_type{ Type::NONE };
    Category m_category{ Category::NONE };
};

struct Operator {
    Token::Type m_type;
    std::string m_value;
};

struct Keyword {
    Token::Type m_type;
    std::string m_value;
};

class Tokenizer {
  public:
    Tokenizer& define_operator(const Operator& op);
    Tokenizer& define_keyword(const Keyword& op);
    std::vector<Token> tokenize(std::string_view code) const;

  private:
    const Operator* try_get_operator(std::string_view value) const;
    const Keyword* try_get_keyword(std::string_view value) const;
    inline bool is_white_space(char c) const;
    Token::Category deduce_token_category(std::string_view value) const;
    void deduce_token_type(Token& token) const;

    std::vector<Operator> m_operators;
    std::vector<Keyword> m_keywords;
};

struct TokenUtils {
    const char* get_token_name(Token::Type type) const {
        const auto idx = std::to_underlying(type);
        if(idx >= sizeof(s_token_names) / sizeof(s_token_names[0])) {
            Logger::DebugWarn("Invalid name for token with idx {}", idx);
            return s_token_names[0];
        }
        return s_token_names[idx];
    }
    const char* get_category_name(Token::Category cat) const {
        const auto idx = std::to_underlying(cat);
        if(idx >= sizeof(s_category_names) / sizeof(s_category_names[0])) { return s_category_names[0]; }
        return s_category_names[idx];
    }

    inline static constexpr const char* s_token_names[]{
        "NONE",  "TERMINATOR", "IDENTIFIER", "INT",   "DOUBLE", "STRING", "PLUS_EQUALS", "INC",
        "DEC",   "EQUALS",     "MIN",        "PLUS",  "MUL",    "DIV",    "PAR_OPEN",    "PAR_CLOSE",
        "COMMA", "BRA_OPEN",   "BRA_CLOSE",  "BREAK", "FUNC",   "RETURN",
    };
    inline static constexpr const char* s_category_names[]{
        "NONE", "TERMINATOR", "UNRESOLVED", "VARIABLE", "NUMBER", "STRING", "OPERATOR", "KEYWORD",
    };
};

} // namespace lexer