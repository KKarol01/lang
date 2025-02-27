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
               || (cat == Token::Category::OPERATOR && get_operator_type(code.substr(i, j + 1)) == Token::Type::NONE) // splits operators (they are the same category, but --- should be dec and min)
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
using parse_expression_t = rule_name_t;
using parse_node_t = std::variant<parse_expression_t, lexer::Token>;
using parse_stack_t = std::stack<parse_node_t>;

struct rule_self_t {};
static constexpr rule_self_t rule_self{};

enum class Type { NONE, OPERATOR, PRIMARY, POSTFIX, UNARY };
struct Expression {
    lexer::Token token;
    Expression* left{};
    Expression* right{};
};

using ast_node_t = Expression*;
using ast_stack_t = std::stack<ast_node_t>;
using program_t = std::vector<ast_node_t>;

struct AST {
    ast_node_t make_expr() { return &expressions.emplace_back(); }
    std::deque<Expression> expressions;
    ast_node_t root{};
};

Expression* add_ast_node(const rule_name_t& name, parse_stack_t& stack, AST& ast) {
    auto* e = ast.make_expr();
    if(name == "primary_expression") {
        assert(std::holds_alternative<lexer::Token>(stack.top()));
        e->left = ast.root;
        e->token = std::get<lexer::Token>(stack.top());
    } else if(name == "postfix_expression") {
        assert(stack.size() > 0 && stack.size() <= 2);
        e->left = ast.root;
        if(stack.size() == 2) {
            assert(std::holds_alternative<lexer::Token>(stack.top()));
            e->right = ast.make_expr();
            e->right->token = std::get<lexer::Token>(stack.top());
        }
    } else {
        assert(false && "Unrecognized rule name");
    }
    ast.root = e;
    int x = 1;
    return nullptr;
}

class Grammar {
  public:
    struct Rule {
        using chain = std::vector<parse_node_t>;
        rule_name_t name;
        std::vector<chain> alternatives;
    };

    void add_rule(const Rule& rule) {
        rules.push_back(rule.name);
        storage[rule.name] = rule;
    }
    Rule& get_rule(const rule_name_t& name) { return storage.at(name); }
    const Rule& get_rule(const rule_name_t& name) const { return storage.at(name); }

    std::vector<rule_name_t> rules;
    std::unordered_map<rule_name_t, Rule> storage;
};

auto put_tokens_onto_stack(const std::vector<lexer::Token>& ts) {
    parse_stack_t stack;
    for(auto it = ts.rbegin(); it != ts.rend(); ++it) {
        stack.push(*it);
    }
    return stack;
}

void match_rule(const Grammar::Rule& rule, parse_stack_t& stack, program_t& program, const Grammar& grammar, AST& ast,
                int recursion_level, bool& modified_stack);

void match_rule_base(parse_stack_t& stack, program_t& program, const Grammar& grammar, AST& ast, int recursion_level,
                     bool& modified_stack) {
    for(const auto& r : grammar.rules) {
        if(modified_stack) { break; }
        match_rule(grammar.get_rule(r), stack, program, grammar, ast, recursion_level, modified_stack);
    }
}

auto is_token(const parse_node_t& n) { return std::holds_alternative<lexer::Token>(n); }
auto is_rule(const parse_node_t& n) { return std::holds_alternative<rule_name_t>(n); }
auto compare_parse_nodes(const parse_node_t& a, const parse_node_t& b) {
    return a.index() == b.index() &&
           ((std::holds_alternative<rule_name_t>(a) && std::get<rule_name_t>(a) == std::get<rule_name_t>(b)) ||
            (std::holds_alternative<lexer::Token>(a) && std::get<lexer::Token>(a).type == std::get<lexer::Token>(b).type));
}

void match_rule(const Grammar::Rule& rule, parse_stack_t& stack, program_t& program, const Grammar& grammar, AST& ast,
                int recursion_level, bool& modified_stack) {
    for(const auto& chain : rule.alternatives) {
        bool matched_chain = true;
        parse_stack_t _stack;
        for(const auto& link : chain) {
            matched_chain = compare_parse_nodes(link, stack.top());
            if(!matched_chain) { break; }
            _stack.push(stack.top());
            stack.pop();
            match_rule_base(stack, program, grammar, ast, recursion_level + 1, modified_stack);
        }
        if(matched_chain) {
            modified_stack = true;
            add_ast_node(rule.name, _stack, ast);
            if(recursion_level == 0 && is_token(stack.top()) &&
               std::get<lexer::Token>(stack.top()).type == lexer::Token::Type::TERMINATOR) {
                stack.pop();
                program.push_back(ast.root);
                ast.root = nullptr;
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

auto parse(const std::vector<lexer::Token>& ts, const Grammar& g, AST& ast) {
    program_t program;
    auto stack = put_tokens_onto_stack(ts);
    while(stack.size() > 1) {
        bool modified_stack = false;
        match_rule_base(stack, program, g, ast, 0, modified_stack);
        assert(modified_stack);
    }
    return program;
}

}; // namespace compiler

void print_ast(const compiler::Expression* node, int indent = 0) {
    if(node == nullptr) { return; }

    for(int i = 0; i < indent; ++i) {
        std::cout << "  ";
    }
    std::cout << node->token.value << " (" << lexer::get_token_type_string_name(node->token.type) << ")\n";

    print_ast(node->left, indent + 1);
    print_ast(node->right, indent + 1);
}

int main() {
    using namespace lexer;
    using namespace compiler;

    Grammar g;
    g.add_rule(Grammar::Rule{ .name = "primary_expression",
                              .alternatives = { {
                                  { Token{ .type = Token::Type::INDENTIFIER } },
                              } } });
    g.add_rule(Grammar::Rule{ .name = "postfix_expression",
                              .alternatives = { {
                                  { "primary_expression" },
                                  { "postfix_expression", Token{ .type = Token::Type::INC } },
                              } } });

    AST ast;

    auto source_code = read();
    std::println("Source code:\n{}\n", read());

    auto s = lexer::tokenize(source_code);
    std::println("Lexical analysis:");
    for(auto& e : s) {
        std::println("[{}]\t{}: {}", lexer::get_token_category_string_name(e.category),
                     lexer::get_token_type_string_name(e.type), e.value);
    }
    std::println("");

    auto program = compiler::parse(s, g, ast);
    std::println("Abstract syntax tree: ");
    for(const auto& node : program) {
        print_ast(node);
    }
}