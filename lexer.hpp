#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <algorithm>
#include "logger.hpp"

/**
 * @file lexer.hpp
 * @brief Definitions of structures and classes used for source code tokenization.
 */

namespace lexer {

/**
 * @struct Token
 * @brief Represents a single token extracted from the source code.
 */
struct Token {
    /**
     * @enum Category
     * @brief Categories of tokens, such as operator, variable, number, etc.
     */
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

    /**
     * @enum Type
     * @brief Specific token types, such as PLUS, IF, INT, etc.
     */
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
        NOT_EQUALS,
        GEQ,
        LEQ,
        LOGICAL_AND,
        LOGICAL_OR,
        AND,
        OR,
        ASSIGN,
        MIN,
        PLUS,
        MUL,
        DIV,
        PAR_OPEN,
        PAR_CLOSE,
        COMMA,
        BRA_OPEN,
        BRA_CLOSE,
        COLON,
        LT,
        GT,
        NOT,

        BREAK,
        FUNC,
        RETURN,
        IF,
        ELSE,
        FOR,
        PRINT,
    };

    std::string m_value;                   /// The string value of the token.
    Type m_type{ Type::NONE };             /// The token's type.
    Category m_category{ Category::NONE }; /// The token's category.
    uint32_t line_number{ ~0ul };          /// The line number in the source code.
};

/**
 * @struct Operator
 * @brief Structure representing an operator definition.
 */
struct Operator {
    Token::Type m_type;  /// The token type corresponding to the operator.
    std::string m_value; /// The string representation of the operator.
};

/**
 * @struct Keyword
 * @brief Structure representing a keyword definition.
 */
struct Keyword {
    Token::Type m_type;  /// The token type corresponding to the keyword.
    std::string m_value; /// The string representation of the keyword.
};

/**
 * @class Tokenizer
 * @brief Class responsible for lexical analysis of source code.
 */
class Tokenizer {
  public:
    /**
     * @brief Constructs a Tokenizer.
     */
    Tokenizer();

    /**
     * @brief Converts source code into a vector of tokens.
     * @param code Source code as a string.
     * @return A vector of generated tokens.
     */
    std::vector<Token> tokenize(std::string_view code) const;

  private:
    void define_operator(const Operator& op);
    void define_keyword(const Keyword& op);
    const Operator* try_get_operator(std::string_view value) const;
    const Keyword* try_get_keyword(std::string_view value) const;
    bool is_white_space(char c) const;
    Token::Category deduce_token_category(std::string_view value) const;
    void deduce_token_type(Token& token) const;

    std::vector<Operator> m_operators; /// List of known operators.
    std::vector<Keyword> m_keywords;   /// List of known keywords.
};

/**
 * @struct TokenUtils
 * @brief Utility functions for working with tokens.
 */
struct TokenUtils {
    /**
     * @brief Returns the token type name as a string.
     * @param type Token type.
     * @return Name of the token type.
     */
    static const char* get_token_name(Token::Type type) {
        const auto idx = std::to_underlying(type);
        if(idx >= sizeof(s_token_names) / sizeof(s_token_names[0])) {
            Logger::DebugWarn("Invalid name for token with idx {}", idx);
            return s_token_names[0];
        }
        return s_token_names[idx];
    }

    /**
     * @brief Returns the token category name as a string.
     * @param cat Token category.
     * @return Name of the token category.
     */
    static const char* get_category_name(Token::Category cat) {
        const auto idx = std::to_underlying(cat);
        if(idx >= sizeof(s_category_names) / sizeof(s_category_names[0])) { return s_category_names[0]; }
        return s_category_names[idx];
    }

    inline static constexpr const char* s_token_names[]{
        "NONE",  "TERMINATOR", "IDENTIFIER", "INT",   "DOUBLE", "STRING",      "PLUS_EQUALS", "INC",
        "DEC",   "EQUALS",     "NOT_EQUALS", "GEQ",   "LEQ",    "LOGICAL_AND", "LOGICAL_OR",  "AND",
        "OR",    "ASSIGN",     "MIN",        "PLUS",  "MUL",    "DIV",         "PAR_OPEN",    "PAR_CLOSE",
        "COMMA", "BRA_OPEN",   "BRA_CLOSE",  "COLON", "LT",     "GT",          "NOT",         "BREAK",
        "FUNC",  "RETURN",     "IF",         "ELSE",  "FOR",    "PRINT",
    };

    inline static constexpr const char* s_category_names[]{
        "NONE", "TERMINATOR", "UNRESOLVED", "VARIABLE", "NUMBER", "STRING", "OPERATOR", "KEYWORD",
    };
};

} // namespace lexer
