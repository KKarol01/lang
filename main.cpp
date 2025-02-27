#include <iostream>
#include <filesystem>
#include <fstream>
#include <string_view>
#include <cassert>
#include <deque>
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
    /*
        When adding new types or categories, remember to fill string_names arrays for them.
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
    /*
        When adding new types or categories, remember to fill string_names arrays for them.
    */
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
using ast_node_t = std::variant<lexer::Token, rule_name_t, lexer::Token::Type>;
using ast_stack_t = std::stack<ast_node_t>;
using program_t = std::vector<rule_name_t>;

static constexpr const char* PRIMARY_EXPRESSION_NAME = "primary_expression";
static constexpr const char* POSTFIX_EXPRESSION_NAME = "postfix_expression";
static constexpr const char* UNARY_OPERATOR_NAME = "unary_operator";
static constexpr const char* UNARY_EXPRESSION_NAME = "unary_expression";

enum class Type { NONE, OPERATOR, PRIMARY, POSTFIX, UNARY };
struct Expression {
    lexer::Token token;
    Expression* left{};
    Expression* right{};
};

struct AST {
    Expression* make_expr() { return &expressions.emplace_back(); }

    std::deque<Expression> expressions;
    Expression* root{};
};

Expression* add_ast_node(const rule_name_t& name, ast_stack_t& stack, AST& ast) {
    auto* expr = ast.make_expr();
    if(name == PRIMARY_EXPRESSION_NAME) {
        assert(stack.size() == 1 && std::holds_alternative<lexer::Token>(stack.top()));
        expr->token = std::get<lexer::Token>(stack.top());
        stack.pop();
        expr->left = ast.root;
    } else if(name == POSTFIX_EXPRESSION_NAME) {
        stack.pop();
        expr->left = ast.root;
        assert(stack.size() == 0 || (stack.size() == 1 && std::holds_alternative<lexer::Token>(stack.top())));
        if(!stack.empty()) {
            auto* inc_tok = ast.make_expr();
            inc_tok->token = std::get<lexer::Token>(stack.top());
            stack.pop();
            expr->right = inc_tok;
        }
    } else if(name == UNARY_OPERATOR_NAME) {
        assert(false);
    } else if(name == UNARY_EXPRESSION_NAME) {
        expr->right = ast.root;
        assert(stack.size() == 0 || (stack.size() == 1 && std::holds_alternative<rule_name_t>(stack.top())));
        if(!stack.empty()) {
            auto* postfix_expr = ast.make_expr();
            postfix_expr->token = std::get<lexer::Token>(stack.top());
            stack.pop();
            expr->left = postfix_expr;
        }
        stack.pop();
    } else {
        assert(false);
        return nullptr;
    }
    ast.root = expr;
}

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

void match_rule2(const Grammar::Rule& rule, ast_stack_t& stack, program_t& program, const Grammar& grammar,
                 int recursion_level, bool& modified_stack);

void match_rule_base(ast_stack_t& stack, program_t& program, const Grammar& grammar, int recursion_level, bool& modified_stack) {
    for(const auto& r : grammar.rules) {
        if(modified_stack) { break; }
        match_rule2(r, stack, program, grammar, recursion_level, modified_stack);
    }
}

auto is_token(const ast_node_t& n) { return std::holds_alternative<lexer::Token::Type>(n); }
auto is_rule(const ast_node_t& n) { return std::holds_alternative<rule_name_t>(n); }

void match_rule2(const Grammar::Rule& rule, ast_stack_t& stack, program_t& program, const Grammar& grammar,
                 int recursion_level, bool& modified_stack) {
    bool matched_any_link = false;
    for(const auto& chain : rule.alternatives) {
        bool matched_chain = true;
        ast_stack_t _stack;
        for(const auto& link : chain) {
            matched_chain = link == stack.top();
            if(!matched_chain) { break; }
            _stack.push(stack.top());
            stack.pop();
            match_rule_base(stack, program, grammar, recursion_level + 1, modified_stack);
        }
        if(matched_chain) {
            modified_stack = true;
            if(recursion_level == 0 && stack.size() > 0 && is_token(stack.top()) &&
               std::get<lexer::Token>(stack.top()).type == lexer::Token::Type::TERMINATOR) {
                stack.pop();
                program.push_back(rule.name);
            } else {
                stack.push(rule.name);
            }
        } else {
            for(auto i = 0; i < _stack.size(); ++i) {
                stack.push(_stack.top());
                _stack.pop();
            }
        }
    }
}

auto parse(const std::vector<lexer::Token>& ts, const Grammar& g) {
    program_t program;
    auto stack = put_tokens_onto_stack(ts);
    while(stack.size() > 1) {
        bool modified_stack = false;
        match_rule_base(stack, program, g, 0, modified_stack);
        assert(modified_stack);
    }
    return program;
}

}; // namespace compiler

int main() {
    using namespace lexer;
    using namespace compiler;

    Grammar g{ {
        Grammar::Rule{ .name = PRIMARY_EXPRESSION_NAME,
                       .alternatives = { {
                           { Token::Type::INDENTIFIER },
                       } } },
        Grammar::Rule{ .name = POSTFIX_EXPRESSION_NAME,
                       .alternatives = { {
                           { PRIMARY_EXPRESSION_NAME },
                           { POSTFIX_EXPRESSION_NAME, Token::Type::INC },
                       } } },
        Grammar::Rule{ .name = UNARY_OPERATOR_NAME,
                       .alternatives = { {
                           { Token::Type::MINUS },
                       } } },
        Grammar::Rule{ .name = UNARY_EXPRESSION_NAME,
                       .alternatives = { {
                           { POSTFIX_EXPRESSION_NAME },
                           { Token::Type::INC, UNARY_EXPRESSION_NAME },
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