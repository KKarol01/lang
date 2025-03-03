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

auto read_source_code() {
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

        IDENTIFIER,
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
        PAR_OPEN,
        PAR_CLOSE,
        COMMA,

        BREAK,
        FUNC,
    };

    auto is_empty() const { return m_type == Type::NONE; }
    auto clear() {
        m_value.clear();
        m_type = Type::NONE;
        m_category = Category::NONE;
    }
    std::string m_value;
    Type m_type{ Type::NONE };
    Category m_category{ Category::NONE };
};

static constexpr const char* token_type_string_names[]{
    "NONE",  "TERMINATOR", "INDENTIFIER", "INT", "DOUBLE",   "STRING",    "PLUS_EQUALS", "INC",   "DEC",  "EQUALS",
    "MINUS", "PLUS",       "MULT",        "DIV", "PAR_OPEN", "PAR_CLOSE", "COMMA",       "BREAK", "FUNC",
};
static constexpr const char* token_category_string_names[]{
    "NONE", "TERMINATOR", "UNRESOLVED", "VARIABLE", "NUMBER", "STRING", "OPERATOR", "KEYWORD",
};

const char* get_token_type_string_name(Token::Type type) { return token_type_string_names[std::to_underlying(type)]; }
const char* get_token_category_string_name(Token::Category type) {
    return token_category_string_names[std::to_underlying(type)];
}

static constexpr const char* operators[]{ "+=", "++", "--", "=", "-", "+", "*", "/", "(", ")", "," };
static constexpr Token::Type operator_types[]{
    Token::Type::PLUS_EQUALS, Token::Type::INC,       Token::Type::DEC,   Token::Type::EQUALS,
    Token::Type::MINUS,       Token::Type::PLUS,      Token::Type::MULT,  Token::Type::DIV,
    Token::Type::PAR_OPEN,    Token::Type::PAR_CLOSE, Token::Type::COMMA,
};
static constexpr uint32_t num_operators = sizeof(operators) / sizeof(operators[0]);

static constexpr const char* keywords[]{ "break", "func" };
static constexpr Token::Type keyword_types[]{
    Token::Type::BREAK,
    Token::Type::FUNC,
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
    std::string_view value = token.m_value;
    switch(token.m_category) {
    case Token::Category::UNRESOLVED: {
        auto type = get_keyword_type(value);
        if(type != Token::Type::NONE) {
            token.m_category = Token::Category::KEYWORD;
            token.m_type = type;
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
        token.m_type = get_operator_type(value);
        assert(token.m_type != Token::Type::NONE);
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

auto tokenize(std::string_view code) {
    std::vector<Token> tokens;
    Token token;
    for(auto i = 0u; i < code.size(); ++i) {
        token.m_category = deduce_token_category(code.substr(i, 1));
        if(token.m_category == Token::Category::NONE) { continue; }
        for(auto j = 1; j < code.size(); ++j) {
            const auto cat = deduce_token_category(code.substr(i + j, 1));
            if((cat != token.m_category && !(token.m_category == Token::Category::UNRESOLVED && cat == Token::Category::NUMBER)) // allows variables with numbers in them
               || (cat == Token::Category::OPERATOR && get_operator_type(code.substr(i, j + 1)) == Token::Type::NONE) // splits operators (they are the same category, but --- should be dec and min)
            ) {
                token.m_value = code.substr(i, j);
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

namespace parser {

using rule_name_t = std::string;
using parse_node_t = lexer::Token;
using parse_stack_t = std::stack<parse_node_t>;

struct Expression;
using parse_expr_t = Expression*;
using program_t = std::vector<parse_expr_t>;

struct Expression {
    parse_node_t token;
    parse_expr_t left{};
    parse_expr_t right{};
};

class Parser {
  public:
    Parser(std::string_view code) {
        auto tokens = lexer::tokenize(code);
        put_tokens_onto_parse_stack(tokens);
    }

    std::vector<parse_expr_t> build_ast() {
        m_program.clear();
        while(!m_stack.empty()) {
            m_program.push_back(parse_statement());
            assert(get().m_type == lexer::Token::Type::TERMINATOR);
            m_stack.pop();
        }
        return m_program;
    }

  private:
    void put_tokens_onto_parse_stack(const std::vector<lexer::Token>& tokens) {
        for(auto it = tokens.rbegin(); it != tokens.rend(); ++it) {
            m_stack.push(*it);
        }
    }

    parse_node_t& get() { return m_stack.top(); }
    parse_node_t take() {
        auto top = m_stack.top();
        m_stack.pop();
        return top;
    }
    parse_expr_t make_expr() { return &m_ast.emplace_back(); }
    parse_expr_t make_expr(const lexer::Token& tok) { return &m_ast.emplace_back(Expression{ .token = tok }); }
    parse_expr_t make_expr(const Expression& expr) { return &m_ast.emplace_back(expr); }

    parse_expr_t parse_prim_expr() {
        auto t = get();
        assert(t.m_type == lexer::Token::Type::IDENTIFIER || t.m_type == lexer::Token::Type::INT ||
               t.m_type == lexer::Token::Type::FUNC);
        m_stack.pop();
        return make_expr(t);
    }

    parse_expr_t parse_expr_list() {
        auto left = parse_add_expr();
        while(get().m_value == ",") {
            auto op = take().m_value;
            auto right = parse_add_expr();
            left = make_expr(Expression{ .token = op, .left = left, .right = right });
        }
        return left; 
    }

    parse_expr_t parse_func_def() {
        auto left = parse_prim_expr();
        if(left->token.m_type != lexer::Token::Type::FUNC) { return left; }
        auto name = parse_prim_expr();
        assert(name->token.m_type == lexer::Token::Type::IDENTIFIER);
        assert(get().m_type == lexer::Token::Type::PAR_OPEN);
        take();
        auto list = parse_expr_list();
        assert(get().m_type == lexer::Token::Type::PAR_CLOSE);
        take();
        return make_expr(Expression{ .token = left->token, .left = name, .right = list });
    }

    parse_expr_t parse_mul_expr() {
        auto left = parse_func_def();
        while(get().m_value == "/" || get().m_value == "*") {
            auto op = take().m_value;
            auto right = parse_func_def();
            left = make_expr(Expression{ .token = op, .left = left, .right = right });
        }
        return left;
    }

    parse_expr_t parse_add_expr() {
        auto left = parse_mul_expr();
        while(get().m_value == "+" || get().m_value == "-") {
            auto op = take().m_value;
            auto right = parse_mul_expr();
            left = make_expr(Expression{ .token = op, .left = left, .right = right });
        }
        return left;
    }

    parse_expr_t parse_assign_expr() {
        auto left = parse_add_expr();
        while(get().m_value == "=") {
            auto op = take().m_value;
            auto right = parse_add_expr();
            left = make_expr(Expression{ .token = op, .left = left, .right = right });
        }
        return left;
    }

    parse_expr_t parse_expr() { return parse_assign_expr(); }

    parse_expr_t parse_statement() { return parse_expr(); }

    program_t m_program;
    parse_stack_t m_stack;
    std::deque<Expression> m_ast;
};

void print_ast(const parser::parse_expr_t node, int indent = 0) {
    if(node == nullptr) { return; }

    for(int i = 0; i < indent; ++i) {
        std::cout << "  ";
    }
    std::cout << node->token.m_value << " (" << lexer::get_token_type_string_name(node->token.m_type) << ")\n";

    print_ast(node->left, indent + 1);
    print_ast(node->right, indent + 1);
}
}; // namespace parser

int main() {
    using namespace lexer;
    using namespace parser;

    parser::Parser p{ read_source_code() };
    auto program = p.build_ast();
    for(auto& p : program) {
        print_ast(p);
    }
    int x = 1;
}