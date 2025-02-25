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

using rule_name_t = std::string;
using ast_node_t = std::variant<lexer::Token::Type, rule_name_t>;
using ast_stack_t = std::stack<ast_node_t>;
using program_t = std::vector<rule_name_t>;

class Grammar {
  public:
    struct Rule {
        using chain = std::vector<ast_node_t>;
        rule_name_t name;
        std::vector<chain> alternatives;
    };

    Grammar(const std::vector<Rule>& _rules) : rules{ _rules } {
        for(auto& r : rules) {
            name_to_rule[r.name] = &r;
        }
    }

    Rule& get_rule(const rule_name_t& name) { return *name_to_rule.at(name); }
    const Rule& get_rule(const rule_name_t& name) const { return *name_to_rule.at(name); }

    std::vector<Rule> rules;
    std::unordered_map<rule_name_t, Rule*> name_to_rule;
};

auto put_tokens_onto_stack(const std::vector<lexer::Token>& ts) {
    ast_stack_t stack;
    for(auto it = ts.rbegin(); it != ts.rend(); ++it) {
        stack.push(it->type);
    }
    return stack;
}

bool match_rule(const Grammar::Rule& rule, ast_stack_t& stack, program_t& program, const Grammar& grammar, int recursion_level) {
    std::println("trying to match rule {}", rule.name);
    bool matched_anything = false;
    bool result = false;
    bool match = true;
    do {
        matched_anything = false;
        match = true;
        for(const auto& chain : rule.alternatives) {
            ast_stack_t _stack;
            for(const auto& link : chain) {
                {
                    std::string link_name, stack_name;
                    if(std::holds_alternative<lexer::Token::Type>(link)) {
                        link_name = std::format("{}", lexer::get_token_type_string_name(std::get<lexer::Token::Type>(link)));
                    } else if(std::holds_alternative<rule_name_t>(link)) {
                        link_name = std::format("{}", std::get<rule_name_t>(link));
                    }
                    if(std::holds_alternative<lexer::Token::Type>(stack.top())) {
                        stack_name =
                            std::format("{}", lexer::get_token_type_string_name(std::get<lexer::Token::Type>(stack.top())));
                    } else if(std::holds_alternative<rule_name_t>(stack.top())) {
                        stack_name = std::format("{}", std::get<rule_name_t>(stack.top()));
                    }
                    std::println("Matching link {} against {}", link_name, stack_name);
                }

                if(std::holds_alternative<lexer::Token::Type>(link)) {
                    if(!std::holds_alternative<lexer::Token::Type>(stack.top()) ||
                       std::get<lexer::Token::Type>(stack.top()) != std::get<lexer::Token::Type>(link)) {
                        match = false;
                        break;
                    }
                } else if(std::holds_alternative<rule_name_t>(link)) {
                    if(std::holds_alternative<rule_name_t>(stack.top()) &&
                       std::get<rule_name_t>(link) != std::get<rule_name_t>(stack.top())) {
                        match = false;
                        break;
                    } else if(std::holds_alternative<lexer::Token::Type>(stack.top())) {
                        /* only call match_rule if previous link or chain matched */
                        const auto& link_rule_name = std::get<rule_name_t>(link);
                        const auto is_recursive = link_rule_name == rule.name;
                        // recursive rule call can only happen if at least one link of any chain in the said rule matched -- prevents infinite recursion
                        // otherwise call another rule (differently named -- look at is_recursive condition)
                        if((is_recursive && (!matched_anything || !match_rule(grammar.get_rule(link_rule_name), stack,
                                                                              program, grammar, recursion_level + 1))) ||
                           (!is_recursive &&
                            !match_rule(grammar.get_rule(link_rule_name), stack, program, grammar, recursion_level + 1))) {
                            match = false;
                            break;
                        }
                    } else if(!std::holds_alternative<rule_name_t>(stack.top()) &&
                              !std::holds_alternative<lexer::Token::Type>(stack.top())) {
                        assert(false);
                    }
                } else {
                    assert(false);
                }
                matched_anything = true;
                _stack.push(stack.top());
                stack.pop();
            }
            if(match) {
                if(recursion_level == 0 && std::holds_alternative<lexer::Token::Type>(stack.top()) &&
                   std::get<lexer::Token::Type>(stack.top()) == lexer::Token::Type::TERMINATOR) {
                    stack.pop();
                    program.push_back(rule.name);
                } else {
                    stack.push(rule.name);
                }
                result = true;
                break;
            } else {
                for(auto i = 0; i < _stack.size(); ++i) {
                    stack.push(_stack.top());
                    _stack.pop();
                }
            }
        }
    } while(match);
    return result;
}

auto parse(const std::vector<lexer::Token>& ts, const Grammar& g) {
    program_t program;
    auto stack = put_tokens_onto_stack(ts);
    while(stack.size() > 1) {
        bool matched = false;
        for(const auto& r : g.rules) {
            if(match_rule(r, stack, program, g, 0)) {
                matched = true;
                break;
            }
        }
        //assert(matched && "No grammar rules understand this!");
    }
    return program;
}

}; // namespace compiler

int main() {
    using namespace lexer;
    using namespace compiler;

    Grammar g{ {
        Grammar::Rule{ .name = "primary_expression",
                       .alternatives = { {
                           { Token::Type::INDENTIFIER },
                       } } },
        Grammar::Rule{ .name = "postfix_expression",
                       .alternatives = { {
                           { "primary_expression" },
                           { "postfix_expression", Token::Type::INC },
                       } } },
        Grammar::Rule{ .name = "unary_operator",
                       .alternatives = { {
                           { Token::Type::MINUS },
                       } } },
        Grammar::Rule{ .name = "unary_expression",
                       .alternatives = { {
                           { "postfix_expression" },
                           { Token::Type::INC, "unary_expression" },
                       } } },
    } };
    // auto assignment_expression = g.add_rule(Rule{ .name = "assignment_expression ",
    //                                               .alts = {
    //                                                   { unary_expression },
    //                                                   { unary_expression, Token::Type::EQUALS, Grammar::self },

    auto s = lexer::tokenize(read());
    auto ast = compiler::parse(s, g);

    for(auto& e : s) {
        std::println("[{}]\t{}: {}", lexer::get_token_category_string_name(e.category),
                     lexer::get_token_type_string_name(e.type), e.value);
    }
}