#include <iostream>
#include <filesystem>
#include <fstream>
#include <string_view>
#include <cassert>
#include <deque>
#include <queue>
#include <print>
#include <variant>
#include <string>
#include <stack>
#include <any>
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

static constexpr const char* TOKEN_NAMES[]{
    "NONE",  "TERMINATOR", "INDENTIFIER", "INT",   "DOUBLE", "STRING", "PLUS_EQUALS", "INC",
    "DEC",   "EQUALS",     "MIN",         "PLUS",  "MUL",    "DIV",    "PAR_OPEN",    "PAR_CLOSE",
    "COMMA", "BRA_OPEN",   "BRA_CLOSE",   "BREAK", "FUNC",   "RETURN",
};
static constexpr const char* TOKEN_CAT_NAMES[]{
    "NONE", "TERMINATOR", "UNRESOLVED", "VARIABLE", "NUMBER", "STRING", "OPERATOR", "KEYWORD",
};

const char* get_token_type_name(Token::Type type) { return TOKEN_NAMES[std::to_underlying(type)]; }
const char* get_token_category_string_name(Token::Category type) { return TOKEN_CAT_NAMES[std::to_underlying(type)]; }

static constexpr const char* OPERATORS[]{ "+=", "++", "--", "=", "-", "+", "*", "/", "(", ")", ",", "{", "}" };
static constexpr Token::Type OPERATOR_TYPES[]{
    Token::Type::PLUS_EQUALS, Token::Type::INC,       Token::Type::DEC,   Token::Type::EQUALS,
    Token::Type::MIN,         Token::Type::PLUS,      Token::Type::MUL,   Token::Type::DIV,
    Token::Type::PAR_OPEN,    Token::Type::PAR_CLOSE, Token::Type::COMMA, Token::Type::BRA_OPEN,
    Token::Type::BRA_CLOSE,
};
static constexpr uint32_t NUM_OPERATORS = sizeof(OPERATORS) / sizeof(OPERATORS[0]);

static constexpr const char* KEYWORDS[]{
    "break",
    "func",
    "return",
};
static constexpr Token::Type KEYWORD_TYPES[]{
    Token::Type::BREAK,
    Token::Type::FUNC,
    Token::Type::RETURN,
};
static constexpr uint32_t NUM_KEYWORDS = sizeof(KEYWORDS) / sizeof(KEYWORDS[0]);

auto is_white_space(char c) { return c == ' ' || c == '\t' || c == '\n'; }

auto is_operator(std::string_view value) {
    return std::find(&OPERATORS[0], &OPERATORS[0] + NUM_OPERATORS, value) != &OPERATORS[0] + NUM_OPERATORS;
}

auto get_operator_type(std::string_view op) {
    for(auto i = 0; i < NUM_OPERATORS; ++i) {
        if(op.compare(OPERATORS[i]) == 0) { return OPERATOR_TYPES[i]; }
    }
    return Token::Type::NONE;
}

auto get_keyword_type(std::string_view value) {
    for(auto i = 0; i < NUM_KEYWORDS; ++i) {
        if(value.compare(KEYWORDS[i]) == 0) { return KEYWORD_TYPES[i]; }
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
            if(token.m_category == Token::Category::STRING && code.at(i + j) != '"') { continue; }
            const auto cat = deduce_token_category(code.substr(i + j, 1));
            if((cat != token.m_category && !(token.m_category == Token::Category::UNRESOLVED && cat == Token::Category::NUMBER)) // allows variables with numbers in them
               || (cat == Token::Category::OPERATOR && get_operator_type(code.substr(i, j + 1)) == Token::Type::NONE) // splits operators (they are the same category, but --- should be dec and min)
               || (token.m_category == Token::Category::STRING && token.m_category == cat)) {
                token.m_value = code.substr(i, j);
                if(token.m_category == Token::Category::STRING) {
                    token.m_value = code.substr(i + 1, j - 1);
                    ++i;
                }
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

using parse_node_t = lexer::Token;
using parse_stack_t = std::stack<parse_node_t>;

struct Expression;
using parse_expr_t = Expression*;
using program_t = std::vector<parse_expr_t>;

struct Expression {
    enum class Type {
        NONE,
        PRIMARY,
        POSTFIX,
        UNARY,
        MIN,
        MUL,
        DIV,
        ADD,
        ASSIGN,
        FUNC_DECL,
        FUNC_CALL,
        EXPR_LIST,
        RETURN_STMNT,
    };
    Type m_type;
    parse_node_t m_node;
    parse_expr_t m_left{};
    parse_expr_t m_right{};
};

static constexpr const char* EXPR_NAMES[]{
    "NONE", "PRIMARY", "POSTFIX",   "UNARY",     "MIN",       "MUL",          "DIV",
    "ADD",  "ASSIGN",  "FUNC_DECL", "FUNC_CALL", "EXPR_LIST", "RETURN_STMNT",
};
static constexpr uint32_t NUM_EXPRS = sizeof(EXPR_NAMES) / sizeof(EXPR_NAMES[0]);

// todo: move this to utils
auto get_expr_name(Expression::Type type) { return EXPR_NAMES[std::to_underlying(type)]; }

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
        }
        return m_program;
    }

  private:
    void put_tokens_onto_parse_stack(const std::vector<parse_node_t>& tokens) {
        for(auto it = tokens.rbegin(); it != tokens.rend(); ++it) {
            m_stack.push(*it);
        }
    }

    // some functions use so as to not force the user to write semicolons after
    // for example brackets: func f1() {}; or if statements.
    void insert_terminator() {
        m_stack.push(parse_node_t{
            .m_value = ";", .m_type = lexer::Token::Type::TERMINATOR, .m_category = lexer::Token::Category::TERMINATOR });
    }

    parse_node_t& get() { return m_stack.top(); }
    parse_node_t take() {
        auto top = m_stack.top();
        m_stack.pop();
        return top;
    }
    parse_expr_t make_expr() { return &m_ast.emplace_back(); }
    parse_expr_t make_expr(const Expression& expr) { return &m_ast.emplace_back(expr); }

    parse_expr_t parse_prim_expr() {
        auto node = get();
        assert(node.m_type == parse_node_t::Type::IDENTIFIER || node.m_type == parse_node_t::Type::INT ||
               node.m_type == parse_node_t::Type::DOUBLE || node.m_type == parse_node_t::Type::STRING ||
               node.m_type == parse_node_t::Type::FUNC);
        m_stack.pop();
        return make_expr(Expression{ .m_type = Expression::Type::PRIMARY, .m_node = node });
    }

    parse_expr_t parse_func_expr() {
        parse_expr_t func_kw = parse_prim_expr();
        parse_expr_t func_name{};
        parse_expr_t func_param_list{};
        parse_expr_t func_body{};
        if(func_kw->m_node.m_type == parse_node_t::Type::FUNC) {
            assert(get().m_type == parse_node_t::Type::IDENTIFIER);
            func_name = parse_prim_expr();
        } else if(func_kw->m_node.m_type == parse_node_t::Type::IDENTIFIER && get().m_type == parse_node_t::Type::PAR_OPEN) {
            func_name = func_kw;
        } else {
            return func_kw;
        }

        assert(take().m_type == parse_node_t::Type::PAR_OPEN);
        if(get().m_type == parse_node_t::Type::PAR_CLOSE) {
            func_param_list = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST });
        } else {
            func_param_list = parse_expr_list();
        }
        assert(get().m_type == parse_node_t::Type::PAR_CLOSE);
        take();

        parse_expr_t func_expr{};
        if(get().m_type == parse_node_t::Type::BRA_OPEN) {
            take();
            func_body = parse_statement();
            while(get().m_type != parse_node_t::Type::BRA_CLOSE) {
                auto right = parse_statement();
                func_body = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = func_body, .m_right = right });
            }
            take();
            func_expr = make_expr(Expression{
                .m_type = Expression::Type::FUNC_DECL, .m_node = func_name->m_node, .m_left = func_param_list, .m_right = func_body });
            insert_terminator();
        } else {
            func_expr = make_expr(Expression{ .m_type = Expression::Type::FUNC_CALL, .m_node = func_name->m_node, .m_left = func_param_list });
        }
        return func_expr;
    }

    parse_expr_t parse_post_expr() {
        auto left = parse_func_expr();
        while(get().m_type == parse_node_t::Type::INC || get().m_type == parse_node_t::Type::DEC) {
            auto node = take();
            left = make_expr(Expression{ .m_type = Expression::Type::POSTFIX, .m_node = node, .m_left = left });
        }
        return left;
    }

    parse_expr_t parse_unar_expr() {
        parse_expr_t left{ nullptr };
        parse_expr_t* next{ nullptr };
        while(get().m_type == parse_node_t::Type::INC || get().m_type == parse_node_t::Type::DEC ||
              get().m_type == parse_node_t::Type::MIN) {
            auto node = take();
            if(!next) {
                left = make_expr(Expression{ .m_type = Expression::Type::UNARY, .m_node = node });
                next = &left->m_left;
            } else {
                *next = make_expr(Expression{ .m_type = Expression::Type::UNARY, .m_node = node });
                next = &((*next)->m_left);
            }
        }
        if(next) {
            *next = parse_post_expr();
        } else {
            left = parse_post_expr();
        }
        return left;
    }

    parse_expr_t parse_mul_expr() {
        auto left = parse_unar_expr();
        while(get().m_type == parse_node_t::Type::MUL || get().m_type == parse_node_t::Type::DIV) {
            auto node = take();
            auto right = parse_unar_expr();
            left = make_expr(Expression{ .m_type = node.m_type == lexer::Token::Type::MUL ? Expression::Type::MUL : Expression::Type::DIV,
                                         .m_node = node,
                                         .m_left = left,
                                         .m_right = right });
        }
        return left;
    }

    parse_expr_t parse_add_expr() {
        auto left = parse_mul_expr();
        while(get().m_type == parse_node_t::Type::PLUS || get().m_type == parse_node_t::Type::MIN) {
            auto node = take();
            auto right = parse_mul_expr();
            left = make_expr(Expression{ .m_type = Expression::Type::ADD, .m_node = node, .m_left = left, .m_right = right });
        }
        return left;
    }

    parse_expr_t parse_assign_expr() {
        auto left = parse_add_expr();
        while(get().m_type == parse_node_t::Type::EQUALS) {
            auto node = take();
            auto right = parse_add_expr();
            left = make_expr(Expression{ .m_type = Expression::Type::ASSIGN, .m_node = node, .m_left = left, .m_right = right });
        }
        return left;
    }

    parse_expr_t parse_expr_list() {
        auto left = parse_assign_expr();
        while(get().m_type == parse_node_t::Type::COMMA) {
            auto node = take();
            auto right = parse_assign_expr();
            left = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_node = node, .m_left = left, .m_right = right });
        }
        return left;
    }

    parse_expr_t parse_expr() { return parse_expr_list(); }

    parse_expr_t parse_return_statement() {
        if(get().m_type == lexer::Token::Type::RETURN) {
            auto ret = take();
            auto list = parse_expr_list();
            return make_expr(Expression{ .m_type = Expression::Type::RETURN_STMNT, .m_node = ret, .m_left = list });
        }
        return parse_expr_list();
    }

    parse_expr_t parse_statement() {
        auto list = parse_return_statement();
        assert(get().m_type == parse_node_t::Type::TERMINATOR);
        take();
        return list;
    }

    program_t m_program;
    parse_stack_t m_stack;
    std::deque<Expression> m_ast;
};

void print_ast(const parser::parse_expr_t node, int indent = 0) {
    if(node == nullptr) { return; }

    for(int i = 0; i < indent; ++i) {
        std::cout << "  ";
    }
    std::println("{} ({} | {})", node->m_node.m_value, lexer::get_token_type_name(node->m_node.m_type),
                 parser::get_expr_name(node->m_type));

    print_ast(node->m_left, indent + 1);
    print_ast(node->m_right, indent + 1);
}
}; // namespace parser

namespace interpreter {

/*  NONE,
    PRIMARY,
    POSTFIX,
    UNARY,
    MIN,
    MUL,
    DIV,
    ADD,
    ASSIGN,
    FUNC_DECL,
    FUNC_CALL,
    EXPR_LIST,*/

class Expression;
using exec_expr_t = std::unique_ptr<Expression>;
using literal_t = std::variant<std::monostate, int, double, std::string>;

class Executor;

class ExecutorAllocator {
  public:
    struct StackFrame {
        literal_t& get_allocation(const std::string& var_name) {
            auto ret = m_variables.insert({ var_name, {} });
            if(ret.second) { m_variable_creation_order.push_back(var_name); }
            return ret.first->second;
        }
        std::deque<std::string> m_variable_creation_order;
        std::unordered_map<std::string, literal_t> m_variables;
        // std::stack<literal_t> m_ephemeral; // for literals; basically crude register emulation
    };

    ExecutorAllocator(Executor* exec) : m_executor(exec) {}

    StackFrame& get_top_stack_frame() { return m_stack_frames.front(); }

    Executor* m_executor{};
    std::deque<StackFrame> m_stack_frames;
};

struct ExpressionResult {
    using memory_t = std::variant<literal_t*, literal_t>;
    // literal_t* on m_type == identifier, literal_t on m_type == int,double,string
    memory_t m_memory;
    std::vector<literal_t> m_return_values;
    lexer::Token::Type m_type{};
};

class Executor {
  public:
    Executor(const parser::program_t& p);

    exec_expr_t make_expr(const parser::parse_expr_t expr);
    Expression* get_func_decl(const std::string& name);

  private:
    parser::program_t m_program;
    std::vector<std::unique_ptr<Expression>> m_exprs;
    std::unordered_map<std::string, std::unique_ptr<Expression>> m_func_decls;
    ExecutorAllocator m_alloc;
};

class Expression {
    friend class FuncCallExpression;
    friend class ReturnStmntExpression;

  public:
    Expression(Executor* exec, const parser::parse_expr_t expr);
    virtual ~Expression() = default;
    virtual ExpressionResult eval(ExecutorAllocator* alloc) = 0;

  protected:
    void assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc) {
        if(!right->m_return_values.empty()) {
            auto& top = alloc->get_top_stack_frame();
            assert(top.m_variable_creation_order.size() >= right->m_return_values.size());
            auto stack_it = top.m_variable_creation_order.end();
            auto vec_it = right->m_return_values.end();
            for(auto i = 0; i < right->m_return_values.size(); ++i) {
                auto& alloc = top.get_allocation(*--stack_it);
                assert(std::holds_alternative<std::monostate>(alloc)); // othwerise: func returns too many values. TODO: check if func returns too few.
                alloc = *--vec_it;
            }
            return;
        }

        auto* assigned = get_pmem(*right);
        if(!std::holds_alternative<literal_t*>(left->m_memory)) {
            assert(false);
            return;
        }
        *std::get<literal_t*>(left->m_memory) = *assigned;
    }
    literal_t* get_pmem(ExpressionResult& res) {
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }
    const literal_t* get_pmem(const ExpressionResult& res) const {
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }
    bool is_int(const ExpressionResult& res) const { return std::holds_alternative<int>(*get_pmem(res)); }
    bool is_double(const ExpressionResult& res) const { return std::holds_alternative<double>(*get_pmem(res)); }
    bool is_string(const ExpressionResult& res) const { return std::holds_alternative<std::string>(*get_pmem(res)); }

    exec_expr_t m_left{};
    exec_expr_t m_right{};
    const parser::parse_expr_t m_expr{};
};

class PrimaryExpression final : public Expression {
  public:
    PrimaryExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~PrimaryExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto& stack = alloc->get_top_stack_frame();
        if(m_expr->m_node.m_type == lexer::Token::Type::IDENTIFIER) {
            auto& any_alloc = stack.get_allocation(m_expr->m_node.m_value);
            return ExpressionResult{ .m_memory = &any_alloc, .m_type = lexer::Token::Type::IDENTIFIER };
        } else {
            literal_t value;
            if(m_expr->m_node.m_type == lexer::Token::Type::INT) {
                value = std::stoi(m_expr->m_node.m_value);
            } else if(m_expr->m_node.m_type == lexer::Token::Type::DOUBLE) {
                value = std::stod(m_expr->m_node.m_value);
            } else if(m_expr->m_node.m_type == lexer::Token::Type::STRING) {
                value = m_expr->m_node.m_value;
            } else {
                assert(false);
                return ExpressionResult{};
            }
            return ExpressionResult{ .m_memory = value, .m_type = m_expr->m_node.m_type };
        }
    }
};

class PostfixExpression final : public Expression {
  public:
};

class UnaryExpression final : public Expression {
  public:
    UnaryExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~UnaryExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto left = m_left->eval(alloc);
        ExpressionResult res{ .m_type = m_expr->m_node.m_type };
        if(!(is_int(left) || is_double(left))) {
            assert(false);
            return {};
        }
        if(m_expr->m_node.m_type == lexer::Token::Type::MIN) {
            res.m_memory = literal_t{ -(is_int(left) ? std::get<int>(*get_pmem(left)) : std::get<double>(*get_pmem(left))) };
        } else if(m_expr->m_node.m_type == lexer::Token::Type::DEC) {
            res.m_memory = literal_t{ (is_int(left) ? --std::get<int>(*get_pmem(left)) : --std::get<double>(*get_pmem(left))) };
        } else if(m_expr->m_node.m_type == lexer::Token::Type::INC) {
            res.m_memory = literal_t{ (is_int(left) ? ++std::get<int>(*get_pmem(left)) : ++std::get<double>(*get_pmem(left))) };
        }
        return res;
    }
};

class AddExpression final : public Expression {
  public:
    AddExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~AddExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto left = m_left->eval(alloc);
        auto right = m_right->eval(alloc);
        auto l_mem = get_pmem(left);
        auto r_mem = get_pmem(right);
        ExpressionResult res{ .m_type = m_expr->m_node.m_type };
        if((is_int(left) && is_double(right)) || (is_int(right) && is_double(left)) || (is_double(left) && is_double(right))) {
            res.m_memory = literal_t{ (double)(is_int(left) ? std::get<int>(*l_mem) : std::get<double>(*l_mem)) +
                                      (double)(is_int(right) ? std::get<int>(*r_mem) : std::get<double>(*r_mem)) };
        } else if(is_int(left) && is_int(right)) {
            res.m_memory = literal_t{ std::get<int>(*l_mem) + std::get<int>(*r_mem) };
        } else if(is_string(left) && is_string(right)) {
            res.m_memory = literal_t{ std::get<std::string>(*l_mem) + std::get<std::string>(*r_mem) };
        } else {
            assert(false);
        }
        return res;
    }
};

class MulExpression final : public Expression {
  public:
    MulExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~MulExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto left = m_left->eval(alloc);
        auto right = m_right->eval(alloc);
        auto l_mem = get_pmem(left);
        auto r_mem = get_pmem(right);
        ExpressionResult res{ .m_type = m_expr->m_node.m_type };
        if((is_int(left) && is_double(right)) || (is_int(right) && is_double(left)) || (is_double(left) && is_double(right))) {
            res.m_memory = literal_t{ (double)(is_int(left) ? std::get<int>(*l_mem) : std::get<double>(*l_mem)) *
                                      (double)(is_int(right) ? std::get<int>(*r_mem) : std::get<double>(*r_mem)) };
        } else if(is_int(left) && is_int(right)) {
            res.m_memory = literal_t{ std::get<int>(*l_mem) * std::get<int>(*r_mem) };
        } else if(is_string(left) && is_string(right)) {
            assert(false);
        } else {
            assert(false);
        }
        return res;
    }
};

class DivExpression final : public Expression {
  public:
    DivExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~DivExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto left = m_left->eval(alloc);
        auto right = m_right->eval(alloc);
        auto l_mem = get_pmem(left);
        auto r_mem = get_pmem(right);
        ExpressionResult res{ .m_type = m_expr->m_node.m_type };
        if((is_int(left) && is_double(right)) || (is_int(right) && is_double(left)) || (is_double(left) && is_double(right))) {
            res.m_memory = literal_t{ (double)(is_int(left) ? std::get<int>(*l_mem) : std::get<double>(*l_mem)) /
                                      (double)(is_int(right) ? std::get<int>(*r_mem) : std::get<double>(*r_mem)) };
        } else if(is_int(left) && is_int(right)) {
            res.m_memory = literal_t{ std::get<int>(*l_mem) / std::get<int>(*r_mem) };
        } else if(is_string(left) && is_string(right)) {
            assert(false);
        } else {
            assert(false);
        }
        return res;
    }
};

class AssignExpression final : public Expression {
  public:
    AssignExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~AssignExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto assignee = m_left->eval(alloc);
        auto assigned = m_right->eval(alloc);
        assign(&assignee, &assigned, alloc);
        return assignee;
    }
};

class FuncDeclExpression final : public Expression {
  public:
    FuncDeclExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~FuncDeclExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        assert(false);
        return ExpressionResult{};
    }
};

class ExprListExpression final : public Expression {
  public:
    ExprListExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~ExprListExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        m_left->eval(alloc);
        return m_right->eval(alloc);
    }
};

class ReturnStmntExpression final : public Expression {
  public:
    ReturnStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~ReturnStmntExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        std::vector<literal_t> results;
        std::stack<Expression*> expr_list_stack;
        expr_list_stack.push(&*m_left);
        while(!expr_list_stack.empty()) {
            auto expr = expr_list_stack.top();
            expr_list_stack.pop();
            results.push_back(*get_pmem(expr->m_right->eval(alloc)));
            if(expr->m_left->m_expr->m_type == parser::Expression::Type::EXPR_LIST) {
                expr_list_stack.push(&*expr->m_left);
            } else {
                results.push_back(*get_pmem(expr->m_left->eval(alloc)));
            }
        }
        return ExpressionResult{ .m_return_values = { results.rbegin(), results.rend() }, .m_type = m_expr->m_node.m_type };
    }
};

class FuncCallExpression final : public Expression {
  public:
    FuncCallExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~FuncCallExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final {
        auto* func_decl = alloc->m_executor->get_func_decl(m_expr->m_node.m_value);
        transfer_call_args(func_decl, alloc);
        auto ret_values = func_decl->m_right->eval(alloc);
        alloc->m_stack_frames.pop_front();
        return ret_values;
    }

  private:
    void transfer_call_args(Expression* func_decl_expr, ExecutorAllocator* alloc) {
        std::stack<Expression*> expr_list_stack;
        std::stack<Expression*> param_list_stack;
        std::queue<std::string> param_list_var_names;
        expr_list_stack.push(&*m_left);
        param_list_stack.push(&*func_decl_expr->m_left);
        while(!param_list_stack.empty()) {
            auto expr = param_list_stack.top();
            param_list_stack.pop();
            if(!expr->m_left && !expr->m_right) { break; }                    // for empty param list
            if(expr->m_expr->m_type != parser::Expression::Type::EXPR_LIST) { // for param list with one param
                param_list_var_names.push(expr->m_expr->m_node.m_value);
                break;
            }
            if(expr->m_right) { param_list_var_names.push(expr->m_right->m_expr->m_node.m_value); }
            if(expr->m_left->m_expr->m_type == parser::Expression::Type::EXPR_LIST) {
                param_list_stack.push(&*expr->m_left);
            } else {
                param_list_var_names.push(expr->m_left->m_expr->m_node.m_value);
            }
        }
        ExecutorAllocator::StackFrame stack_frame;
        while(!expr_list_stack.empty()) {
            auto expr = expr_list_stack.top();
            expr_list_stack.pop();
            // if(!expr->m_left && !expr->m_right) { break; }                     // for empty param list
            // if(expr->m_expr->m_type != parser::Expression::Type::EXPR_LIST) { // for param list with one param
            //     param_list_var_names.push(expr->m_expr->m_node.m_value);
            //     break;
            // }
            if(expr->m_right) {
                auto& val = *get_pmem(expr->m_right->eval(alloc));
                stack_frame.get_allocation(param_list_var_names.front()) = val;
                param_list_var_names.pop();
            }
            if(expr->m_left->m_expr->m_type == parser::Expression::Type::EXPR_LIST) {
                expr_list_stack.push(&*expr->m_left);
            } else {
                auto& val = *get_pmem(expr->m_left->eval(alloc));
                stack_frame.get_allocation(param_list_var_names.front()) = val;
                param_list_var_names.pop();
            }
        }
        alloc->m_stack_frames.push_front(std::move(stack_frame));
    }
};

Expression::Expression(Executor* exec, const parser::parse_expr_t expr) : m_expr(expr) {
    m_left = exec->make_expr(expr->m_left);
    m_right = exec->make_expr(expr->m_right);
}

Executor::Executor(const parser::program_t& p) : m_program(p), m_alloc(this) {
    m_exprs.reserve(m_program.size());
    m_alloc.m_stack_frames.emplace_front();
    for(auto& p : m_program) {
        if(p->m_type == parser::Expression::Type::FUNC_DECL) {
            m_func_decls[p->m_node.m_value] = make_expr(p);
        } else {
            m_exprs.push_back(make_expr(p));
            m_exprs.back()->eval(&m_alloc);
        }

        std::println("[Stack variables]");
        for(auto& ms : m_alloc.get_top_stack_frame().m_variables) {
            std::string type_name;
            if(std::holds_alternative<int>(ms.second)) {
                std::println("[{} | {}] : {}", ms.first, "i32", std::get<int>(ms.second));
            } else if(std::holds_alternative<double>(ms.second)) {
                std::println("[{} | {}] : {}", ms.first, "f64", std::get<double>(ms.second));
            } else if(std::holds_alternative<std::string>(ms.second)) {
                std::println("[{} | {}] : {}", ms.first, "str", std::get<std::string>(ms.second));
            } else {
                // here is undeclared variables used in expressions and something else i do not know about yet.
                assert(false);
                std::println("[{} | {}] : {}", "", "VOID", "ERROR");
            }
        }
    }
    m_alloc.m_stack_frames.pop_front();
}

exec_expr_t Executor::make_expr(const parser::parse_expr_t expr) {
    if(!expr) { return nullptr; }
    switch(expr->m_type) {
    case parser::Expression::Type::ASSIGN: {
        return std::make_unique<AssignExpression>(this, expr);
    }
    case parser::Expression::Type::PRIMARY: {
        return std::make_unique<PrimaryExpression>(this, expr);
    }
    case parser::Expression::Type::FUNC_DECL: {
        return std::make_unique<FuncDeclExpression>(this, expr);
    }
    case parser::Expression::Type::EXPR_LIST: {
        return std::make_unique<ExprListExpression>(this, expr);
    }
    case parser::Expression::Type::ADD: {
        return std::make_unique<AddExpression>(this, expr);
    }
    case parser::Expression::Type::MUL: {
        return std::make_unique<MulExpression>(this, expr);
    }
    case parser::Expression::Type::DIV: {
        return std::make_unique<DivExpression>(this, expr);
    }
    case parser::Expression::Type::RETURN_STMNT: {
        return std::make_unique<ReturnStmntExpression>(this, expr);
    }
    case parser::Expression::Type::UNARY: {
        return std::make_unique<UnaryExpression>(this, expr);
    }
    case parser::Expression::Type::FUNC_CALL: {
        return std::make_unique<FuncCallExpression>(this, expr);
    }
    default: {
        assert(false);
        return nullptr;
    }
    }
}

Expression* Executor::get_func_decl(const std::string& name) { return &*m_func_decls.at(name); }

} // namespace interpreter

int main() {
    using namespace lexer;
    using namespace parser;

    parser::Parser p{ read_source_code() };
    auto program = p.build_ast();
    for(auto& p : program) {
        print_ast(p);
    }
    interpreter::Executor exec{ program };
}