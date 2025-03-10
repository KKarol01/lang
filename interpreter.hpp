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

class ExecutorAllocator {
  public:
    struct StackFrame {
        literal_t& get_allocation(const std::string& var_name);
        std::deque<std::string> m_variable_creation_order;
        std::unordered_map<std::string, literal_t> m_variables;
    };

    ExecutorAllocator(Executor* exec);
    inline StackFrame& get_top_stack_frame() { return m_stack_frames.front(); }

    Executor* m_executor{};
    std::deque<StackFrame> m_stack_frames;
};

struct ExpressionResult {
    using memory_t = std::variant<literal_t*, literal_t>; // literal_t* on m_type == identifier, literal_t on m_type == int,double,string
    memory_t m_memory;
    std::vector<literal_t> m_return_values;
    lexer::Token::Type m_type{};
};

class Executor {
  public:
    Executor(const parser::program_t& p);

    exec_expr_t make_expr(const parser::parse_expr_t expr);
    inline Expression* get_func_decl(const std::string& name) { return &*m_func_decls.at(name); }

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
    void assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc);
    inline literal_t* get_pmem(ExpressionResult& res) {
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }
    inline const literal_t* get_pmem(const ExpressionResult& res) const {
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }
    inline bool is_int(const ExpressionResult& res) const { return std::holds_alternative<int>(*get_pmem(res)); }
    inline bool is_double(const ExpressionResult& res) const { return std::holds_alternative<double>(*get_pmem(res)); }
    inline bool is_string(const ExpressionResult& res) const {
        return std::holds_alternative<std::string>(*get_pmem(res));
    }

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

} // namespace interpreter