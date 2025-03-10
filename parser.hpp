#pragma once

#include <vector>
#include <utility>
#include <stack>
#include <deque>
#include "lexer.hpp"

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
inline auto get_expr_name(Expression::Type type) { return EXPR_NAMES[std::to_underlying(type)]; }

class Parser {
  public:
    Parser(const std::vector<lexer::Token>& tokens);

    std::vector<parse_expr_t> build_ast();

  private:
    void put_tokens_onto_parse_stack(const std::vector<parse_node_t>& tokens);

    // some functions use so as to not force the user to write semicolons after
    // for example brackets: func f1() {}; or if statements.
    void insert_terminator();

    inline parse_node_t& get() { return m_stack.top(); }
    inline parse_node_t take() {
        auto top = m_stack.top();
        m_stack.pop();
        return top;
    }
    inline parse_expr_t make_expr() { return &m_ast.emplace_back(); }
    inline parse_expr_t make_expr(const Expression& expr) { return &m_ast.emplace_back(expr); }

    parse_expr_t parse_prim_expr();
    parse_expr_t parse_func_expr();
    parse_expr_t parse_post_expr();
    parse_expr_t parse_unar_expr();
    parse_expr_t parse_mul_expr();
    parse_expr_t parse_add_expr();
    parse_expr_t parse_assign_expr();
    parse_expr_t parse_expr_list();
    parse_expr_t parse_expr();
    parse_expr_t parse_return_statement();
    parse_expr_t parse_statement();

    program_t m_program;
    parse_stack_t m_stack;
    std::deque<Expression> m_ast;
};

void print_ast(const parser::parse_expr_t node, int indent = 0);
}; // namespace parser