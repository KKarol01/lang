#pragma once

#include <vector>
#include <utility>
#include <stack>
#include <deque>
#include "logger.hpp"
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
        SUB,
        MUL,
        DIV,
        ADD,
        ASSIGN,
        LOGICAL_COMPARE,
        LOGICAL_OP,
        FUNC_DECL,
        FUNC_CALL,
        EXPR_LIST,
        RETURN_STMNT,
        IF_STMNT,
        FOR_STMNT,
        BREAK_STMNT,
        PRINT_STMNT,
    };
    Type m_type{ Type::NONE };
    parse_node_t m_node{};
    parse_expr_t m_left{};
    parse_expr_t m_right{};
};

struct ExpressionUtils {
    static const char* get_expression_name(Expression::Type type) {
        const auto idx = std::to_underlying(type);
        if(idx >= sizeof(s_expr_names) / sizeof(s_expr_names[0])) {
            Logger::DebugWarn("Invalid name for token with idx {}", idx);
            return s_expr_names[0];
        }
        return s_expr_names[idx];
    }

    inline static constexpr const char* s_expr_names[]{
        "NONE",         "PRIMARY",  "POSTFIX",         "UNARY",       "SUB",         "MUL",       "DIV",
        "ADD",          "ASSIGN",   "LOGICAL_COMPARE", "LOGICAL_OP",  "FUNC_DECL",   "FUNC_CALL", "EXPR_LIST",
        "RETURN_STMNT", "IF_STMNT", "FOR_STMNT",       "BREAK_STMNT", "PRINT_STMNT",
    };
};

class Parser {
  public:
    Parser(const std::vector<lexer::Token>& tokens);

    std::vector<parse_expr_t> build_ast();

  private:
    void put_tokens_onto_parse_stack(const std::vector<parse_node_t>& tokens);

    // some functions use so as to not force the user to write semicolons after
    // for example brackets: func f1() {}; or if statements.
    void insert_terminator();

    parse_node_t& get() { return m_stack.top(); }
    parse_node_t take() {
        auto top = m_stack.top();
        m_stack.pop();
        return top;
    }
    void check_or_throw(parse_node_t::Type expected, bool take = false);
    parse_expr_t make_expr() { return &m_ast.emplace_back(); }
    parse_expr_t make_expr(const Expression& expr) { return &m_ast.emplace_back(expr); }

    parse_expr_t parse_prim_expr();
    parse_expr_t parse_func_expr();
    parse_expr_t parse_if_expr();
    parse_expr_t parse_for_expr();
    parse_expr_t parse_post_expr();
    parse_expr_t parse_unar_expr();
    parse_expr_t parse_mul_expr();
    parse_expr_t parse_add_sub_expr();
    parse_expr_t parse_comp_expr();
    parse_expr_t parse_logical_expr();
    parse_expr_t parse_assign_expr();
    parse_expr_t parse_expr_list();
    parse_expr_t parse_expr();
    parse_expr_t parse_return_statement();
    parse_expr_t parse_break_statement();
    parse_expr_t parse_print_statement();
    parse_expr_t parse_statement();

    program_t m_program;
    parse_stack_t m_stack;
    std::deque<Expression> m_ast;
};

void print_ast(const parser::parse_expr_t node, int indent = 0);
}; // namespace parser