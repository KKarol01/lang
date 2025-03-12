#pragma once

#include <memory>
#include <vector>
#include <deque>
#include <unordered_map>
#include <variant>
#include <string>

#include "lexer.hpp"
#include "parser.hpp"

namespace interpreter {

class Expression;
using exec_expr_t = std::unique_ptr<Expression>;
using literal_t = std::variant<std::monostate, int, double, std::string>;

class Executor;

struct ExpressionResult {
    using memory_t = std::variant<literal_t*, literal_t>; // literal_t* on m_type == identifier, literal_t on m_type == int,double,string
    memory_t m_memory;
    std::vector<literal_t> m_return_values;
    lexer::Token::Type m_type{};
};

class ExecutorAllocator {
  public:
    struct StackFrame {
        literal_t& get_allocation(const std::string& var_name);
        literal_t* try_find_allocation(const std::string& var_name);
        bool m_return_stmn_hit{};
        std::deque<std::pair<std::string, literal_t>> m_variables;
    };

    ExecutorAllocator(Executor* exec);
    StackFrame& get_top_stack_frame() { return m_stack_frames.front(); }

    Executor* m_executor{};
    std::deque<StackFrame> m_stack_frames;
};

class Executor {
  public:
    Executor(const parser::program_t& p);

    exec_expr_t make_expr(const parser::parse_expr_t expr);
    Expression* get_func_decl(const std::string& name) { return &*m_func_decls.at(name); }

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
    const parser::parse_expr_t get_parse_expr() const { return m_expr; }
    std::string_view get_node_value() const {
        if(!m_expr) { return std::string_view{}; }
        return m_expr->m_node.m_value;
    }

  protected:
    static void dfs_traverse(Expression* expr, const auto& callback) {
        if(!expr) { return; }
        if(expr->m_left) { dfs_traverse(&*expr->m_left, callback); }
        callback(expr);
        if(expr->m_right) { dfs_traverse(&*expr->m_right, callback); }
    }
    static void dfs_traverse_expr_list(Expression* expr, const auto& callback) {
        if(!expr) { return; }
        if(expr->m_expr->m_type != parser::Expression::Type::EXPR_LIST) {
            callback(&*expr);
        } else {
            if(expr->m_left) { dfs_traverse_expr_list(&*expr->m_left, callback); }
            if(expr->m_right) { dfs_traverse_expr_list(&*expr->m_right, callback); }
        }
    }
    // static void assign(literal_t& literal, const ExpressionResult& res);

    void assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc);
    static literal_t* get_pmem(ExpressionResult& res) {
        if(!res.m_return_values.empty()) { return &res.m_return_values.back(); }
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }
    static const literal_t* get_pmem(const ExpressionResult& res) {
        if(!res.m_return_values.empty()) { return &res.m_return_values.back(); }
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
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class PostfixExpression final : public Expression {
  public:
};

class UnaryExpression final : public Expression {
  public:
    UnaryExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~UnaryExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class AddExpression final : public Expression {
  public:
    AddExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~AddExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class MulExpression final : public Expression {
  public:
    MulExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~MulExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class DivExpression final : public Expression {
  public:
    DivExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~DivExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class AssignExpression final : public Expression {
  public:
    AssignExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~AssignExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class FuncDeclExpression final : public Expression {
  public:
    FuncDeclExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~FuncDeclExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class ExprListExpression final : public Expression {
  public:
    ExprListExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~ExprListExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class ReturnStmntExpression final : public Expression {
  public:
    ReturnStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~ReturnStmntExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class FuncCallExpression final : public Expression {
  public:
    FuncCallExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~FuncCallExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;

  private:
    void transfer_call_args(Expression* func_decl_expr, ExecutorAllocator* alloc);
};

class IfStmntExpression final : public Expression {
  public:
    IfStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~IfStmntExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class LogicalOpExpression final : public Expression {
  public:
    LogicalOpExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~LogicalOpExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class LogicalCompExpression final : public Expression {
  public:
    LogicalCompExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}
    ~LogicalCompExpression() final = default;
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

} // namespace interpreter