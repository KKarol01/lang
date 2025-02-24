#include <iostream>
#include <filesystem>
#include <fstream>
#include <string_view>
#include <cassert>
#include <print>
#include <variant>
#include <string>
#include <stack>
#include <unordered_map>
#include <parser/parser.hpp>

/*
a = 5
a = -5
*/

auto read() {
    std::ifstream file{ "script.lang" };
    if(!file.is_open()) { return std::string{}; }
    file.seekg(0, std::ios::end);
    const size_t size = file.tellg();
    std::string content(size, '\0');
    file.seekg(0);
    file.read(content.data(), content.size());
    return content;
}

namespace lexer {
struct Token {
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

        INDENTIFIER,
        INT,
        DOUBLE,
        STRING,

        PLUS_EQUALS,
        INC,
        DEC,
        EQUALS,
        MINUS,
        PLUS,
        MULT,
        DIV,

        BREAK,
    };
    auto is_empty() const { return type == Type::NONE; }
    auto clear() {
        value.clear();
        type = Type::NONE;
        category = Category::NONE;
    }
    std::string value;
    Type type{ Type::NONE };
    Category category{ Category::NONE };
};

static constexpr const char* token_type_string_names[]{
    "NONE", "TERMINATOR", "INDENTIFIER", "INT",  "DOUBLE", "STRING", "PLUS_EQUALS", "INC",
    "DEC",  "EQUALS",     "MINUS",       "PLUS", "MULT",   "DIV",    "BREAK",
};
static constexpr const char* token_category_string_names[]{
    "NONE", "TERMINATOR", "UNRESOLVED", "VARIABLE", "NUMBER", "STRING", "OPERATOR", "KEYWORD",
};

const char* get_token_type_string_name(Token::Type type) { return token_type_string_names[std::to_underlying(type)]; }
const char* get_token_category_string_name(Token::Category type) {
    return token_category_string_names[std::to_underlying(type)];
}

static constexpr const char* operators[]{ "+=", "++", "--", "=", "-", "+", "*", "/" };
static constexpr Token::Type operator_types[]{
    Token::Type::PLUS_EQUALS, Token::Type::INC,  Token::Type::DEC,  Token::Type::EQUALS,
    Token::Type::MINUS,       Token::Type::PLUS, Token::Type::MULT, Token::Type::DIV,
};
static constexpr uint32_t num_operators = sizeof(operators) / sizeof(operators[0]);

static constexpr const char* keywords[]{
    "break",
};
static constexpr Token::Type keyword_types[]{
    Token::Type::BREAK,
};
static constexpr uint32_t num_keywords = sizeof(keywords) / sizeof(keywords[0]);

auto is_white_space(char c) { return c == ' ' || c == '\n'; }

auto is_operator(std::string_view value) {
    return std::find(&operators[0], &operators[0] + num_operators, value) != &operators[0] + num_operators;
}

auto get_operator_type(std::string_view op) {
    for(auto i = 0; i < num_operators; ++i) {
        if(op.compare(operators[i]) == 0) { return operator_types[i]; }
    }
    return Token::Type::NONE;
}

auto get_keyword_type(std::string_view value) {
    for(auto i = 0; i < num_keywords; ++i) {
        if(value.compare(keywords[i]) == 0) { return keyword_types[i]; }
    }
    return Token::Type::NONE;
}

auto deduce_token_category(std::string_view value) {
    if(value.empty()) { return Token::Category::NONE; }
    if(is_white_space(value.at(0)) || value.at(0) == '\0') {
        return Token::Category::NONE;
    } else if(value.at(0) == ';') {
        return Token::Category::TERMINATOR;
    } else if(get_operator_type(value) != Token::Type::NONE) {
        return Token::Category::OPERATOR;
    } else if(value.starts_with('"')) {
        return Token::Category::STRING;
    } else if(std::isdigit(value.at(0)) || value.starts_with('.')) {
        return Token::Category::NUMBER;
    } else {
        return Token::Category::UNRESOLVED; // could be variable, keyword, func definition, call
    }
}

auto deduce_token_type(Token& token) {
    std::string_view value = token.value;
    switch(token.category) {
    case Token::Category::UNRESOLVED: {
        auto type = get_keyword_type(value);
        if(type != Token::Type::NONE) {
            token.category = Token::Category::KEYWORD;
            token.type = type;
            return;
        }
        token.category = Token::Category::VARIABLE;
        token.type = Token::Type::INDENTIFIER;
        return;
    }
    case Token::Category::NUMBER: {
        if(value.find(".") != std::string::npos) {
            token.type = Token::Type::DOUBLE;
        } else {
            token.type = Token::Type::INT;
        }
        return;
    }
    case Token::Category::STRING: {
        token.type = Token::Type::STRING;
        return;
    }
    case Token::Category::OPERATOR: {
        token.type = get_operator_type(value);
        assert(token.type != Token::Type::NONE);
        return;
    }
    case Token::Category::TERMINATOR: {
        token.type = Token::Type::TERMINATOR;
        return;
    }
    default: {
        assert(false);
    }
    }
}

auto tokenize(std::string_view code) {
    std::vector<Token> tokens;
    Token token;
    for(auto i = 0u; i < code.size(); ++i) {
        token.category = deduce_token_category(code.substr(i, 1));
        if(token.category == Token::Category::NONE) { continue; }
        for(auto j = 1; j < code.size(); ++j) {
            const auto cat = deduce_token_category(code.substr(i + j, 1));
            if((cat != token.category && !(token.category == Token::Category::UNRESOLVED && cat == Token::Category::NUMBER)) // allows variables with numbers in them
               || (cat == Token::Category::OPERATOR && get_operator_type(code.substr(i, j + 1)) == Token::Type::NONE) // splits operators (they are the same category, but --- should be double dec and one min)
            ) {
                token.value = code.substr(i, j);
                deduce_token_type(token);
                tokens.push_back(token);
                token.clear();
                i += j - 1;
                break;
            }
        }
    }
    return tokens;
}
} // namespace lexer

namespace compiler {
struct Grammar {
    struct Rule;
    struct self_t {};
    static inline constexpr self_t self{};
    using RuleBase = std::variant<lexer::Token::Type, Rule*, self_t>;
    using RuleAlternative = std::vector<RuleBase>;
    struct Rule {
        std::string name;
        std::vector<RuleAlternative> alts;
    };

    Rule* add_rule(Rule r) {
        rules.push_front(r);
        grammar.push_back(&rules.front());
        return &rules.front();
    }

    std::forward_list<Rule> rules;
    std::vector<Rule*> grammar;
};
} // namespace compiler

namespace parser {

bool match_rule(const compiler::Grammar::Rule* rule, size_t& token_pos, const std::vector<lexer::Token>& tokens,
                const compiler::Grammar& g) {
    using namespace lexer;
    using namespace compiler;
    using Rule = Grammar::Rule;
    for(auto& alt : rule->alts) {
        size_t pos = token_pos;
        auto token = tokens.at(pos);
        if(token.type == Token::Type::TERMINATOR) { return true; }
        bool match = true;
        for(auto& r : alt) {
            if(std::holds_alternative<Token::Type>(r)) {
                if(std::get<Token::Type>(r) != token.type) {
                    match = false;
                    break;
                }
                ++pos;
            } else if(std::holds_alternative<Rule*>(r)) {
                if(!match_rule(std::get<Rule*>(r), pos, tokens, g)) {
                    match = false;
                    break;
                }
            } else if(std::holds_alternative<Grammar::self_t>(r)) {
                if(!match_rule(rule, pos, tokens, g)) {
                    match = false;
                    break;
                }
            } else {
                assert(false);
            }
            token = tokens.at(pos);
        }
        if(match) {
            token_pos = pos;
        }
        pos = token_pos;
    }
}

struct Statement {};

class AST {
  public:
    std::vector<Statement> statements;
};

auto build_ast(const std::vector<lexer::Token>& tokens, const compiler::Grammar& g) {
    // AST ast;
    // size_t position = 0;

    //// Assuming assignment_expression is the top-level rule
    // while(position < tokens.size()) {
    //     if(match_rule(g.grammar.back(), tokens, position, g)) {
    //         // Matched a statement, now create an AST node for it
    //         Statement statement; // Replace with actual AST node creation logic
    //         ast.statements.push_back(statement);
    //     } else {
    //         // Error handling: No matching rule found
    //         std::cerr << "Error: Unexpected token at position " << position << std::endl;
    //         break; // Or handle the error in a more sophisticated way
    //     }
    // }

    // return ast;
}

} // namespace parser

int main() {
    using namespace lexer;
    using namespace compiler;
    using Rule = Grammar::Rule;

    Grammar g;
    auto primary_expression =
        g.add_rule(Rule{ .name = "primary_expression", .alts = { { Token::Type::INDENTIFIER }, { Token::Type::INT } } });
    auto postfix_expression = g.add_rule(Rule{
        .name = "postfix_expression ", .alts = { { Token::Type::INDENTIFIER }, { Grammar::self, Token::Type::INC } } });
    auto unary_operator = g.add_rule(Rule{ .name = "unary_operator ",
                                           .alts = {
                                               { Token::Type::MINUS },
                                           } });
    auto unary_expression = g.add_rule(Rule{ .name = "unary_expression ",
                                             .alts = {
                                                 { postfix_expression },
                                                 { Token::Type::INC, Grammar::self },
                                                 { unary_operator, Grammar::self },
                                             } });
    auto assignment_expression = g.add_rule(Rule{ .name = "assignment_expression ",
                                                  .alts = {
                                                      { unary_expression },
                                                      { unary_expression, Token::Type::EQUALS, Grammar::self },
                                                  } });
    auto s = lexer::tokenize(read());
    std::vector<bool> results;
    for(auto i = 0; i < g.grammar.size(); ++i) {
        auto& r = g.grammar.at(i);
        size_t pos = 0;
        results.push_back(parser::match_rule(r, pos, s, g));
    }

    for(auto& e : s) {
        std::println("[{}]\t{}: {}", lexer::get_token_category_string_name(e.category),
                     lexer::get_token_type_string_name(e.type), e.value);
    }
}