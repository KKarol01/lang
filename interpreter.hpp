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
/**
 * @brief Unique pointer to an Expression object
 */
using exec_expr_t = std::unique_ptr<Expression>;

/**
 * @brief Variant type representing possible literal values in the interpreter
 */
using literal_t = std::variant<std::monostate, int, double, std::string>;

class Executor;

/**
 * @brief Structure representing the result of evaluating an expression
 */
struct ExpressionResult {
    /**
     * @brief Variant type for storing either a pointer to a literal or a literal value
     *
     * literal_t* when m_type == identifier
     * literal_t when m_type == int, double, or string
     */
    using memory_t = std::variant<literal_t*, literal_t>;

    memory_t m_memory;                      /// Memory storage for the result
    std::vector<literal_t> m_return_values; /// Vector of return values (used for multiple returns)
    lexer::Token::Type m_type{};            /// Type of the result
};

/**
 * @brief Class responsible for memory allocation and stack frame management
 */
class ExecutorAllocator {
  public:
    /**
     * @brief Structure representing a stack frame
     */
    struct StackFrame {
        /**
         * @brief Get or create an allocation for a variable
         * @param var_name Name of the variable
         * @return Reference to the variable's storage
         */
        literal_t& get_allocation(const std::string& var_name);

        /**
         * @brief Try to find an existing allocation for a variable
         * @param var_name Name of the variable
         * @return Pointer to the variable's storage if found, nullptr otherwise
         */
        literal_t* try_find_allocation(const std::string& var_name);

        bool m_return_stmn_hit{};                                  /// Flag indicating if a return statement was hit
        bool m_break_stmn_hit{};                                   /// Flag indicating if a break statement was hit
        bool m_prev_if_in_chain_succeded{};                        /// Flag for if-chain control flow
        int m_if_chain_recursion_level{};                          /// Counter for nested if statements
        std::deque<std::pair<std::string, literal_t>> m_variables; /// Storage for variables in this frame
    };

    /**
     * @brief Construct a new ExecutorAllocator
     * @param exec Pointer to the Executor
     */
    ExecutorAllocator(Executor* exec);

    /**
     * @brief Get the top stack frame
     * @return Reference to the top stack frame
     */
    StackFrame& get_top_stack_frame() { return m_stack_frames.front(); }

    Executor* m_executor{};                /// Pointer to the associated Executor
    std::deque<StackFrame> m_stack_frames; /// Stack frames storage
};

/**
 * @brief Main executor class that manages expression evaluation
 */
class Executor {
  public:
    /**
     * @brief Construct a new Executor with a program
     * @param p The program to execute
     */
    Executor(const parser::program_t& p);

    /**
     * @brief Create an expression from a parse expression
     * @param expr The parse expression to convert
     * @return Unique pointer to the created expression
     */
    exec_expr_t make_expr(const parser::parse_expr_t expr);

    /**
     * @brief Get a function declaration by name
     * @param name Name of the function
     * @return Pointer to the function declaration expression if found, nullptr otherwise
     */
    Expression* get_func_decl(const std::string& name) {
        if(auto it = m_func_decls.find(name); it != m_func_decls.end()) { return &*it->second; }
        return nullptr;
    }

  private:
    parser::program_t m_program;                                               /// The program being executed
    std::vector<std::unique_ptr<Expression>> m_exprs;                          /// Storage for expressions
    std::unordered_map<std::string, std::unique_ptr<Expression>> m_func_decls; /// Function declarations
    ExecutorAllocator m_alloc;                                                 /// Memory allocator
};

/**
 * @brief Base class for all expression types
 */
class Expression {
    friend class FuncCallExpression;
    friend class ReturnStmntExpression;
    friend class ForStmntExpression;

  public:
    /**
     * @brief Construct a new Expression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    Expression(Executor* exec, const parser::parse_expr_t expr);

    /**
     * @brief Destroy the Expression
     */
    virtual ~Expression() = default;

    /**
     * @brief Evaluate the expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    virtual ExpressionResult eval(ExecutorAllocator* alloc) = 0;

    /**
     * @brief Get the parse expression
     * @return The parse expression
     */
    const parser::parse_expr_t get_parse_expr() const { return m_expr; }

    /**
     * @brief Get the node value
     * @return String view of the node value
     */
    std::string_view get_node_value() const {
        if(!m_expr) { return std::string_view{}; }
        return m_expr->m_node.m_value;
    }

  protected:
    /**
     * @brief Depth-first traversal of expression tree
     * @param expr Root expression
     * @param callback Callback function for each node
     */
    static void dfs_traverse(Expression* expr, const auto& callback) {
        if(!expr) { return; }
        if(expr->m_left) { dfs_traverse(&*expr->m_left, callback); }
        callback(expr);
        if(expr->m_right) { dfs_traverse(&*expr->m_right, callback); }
    }

    /**
     * @brief Depth-first traversal specifically for expression lists
     * @param expr Root expression
     * @param callback Callback function for each node
     */
    static void dfs_traverse_expr_list(Expression* expr, const auto& callback) {
        if(!expr) { return; }
        if(expr->m_expr->m_type != parser::Expression::Type::EXPR_LIST) {
            callback(&*expr);
        } else {
            if(expr->m_left) { dfs_traverse_expr_list(&*expr->m_left, callback); }
            if(expr->m_right) { dfs_traverse_expr_list(&*expr->m_right, callback); }
        }
    }

    /**
     * @brief Assign a value to a memory location
     * @param left Left-hand side (target)
     * @param right Right-hand side (source)
     * @param alloc Pointer to the allocator
     */
    void assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc);

    /**
     * @brief Get pointer to memory from an ExpressionResult
     * @param res The ExpressionResult
     * @return Pointer to the memory
     */
    static literal_t* get_pmem(ExpressionResult& res) {
        if(!res.m_return_values.empty()) { return &res.m_return_values.back(); }
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }

    /**
     * @brief Get const pointer to memory from an ExpressionResult
     * @param res The ExpressionResult
     * @return Const pointer to the memory
     */
    static const literal_t* get_pmem(const ExpressionResult& res) {
        if(!res.m_return_values.empty()) { return &res.m_return_values.back(); }
        return std::holds_alternative<literal_t>(res.m_memory) ? &std::get<literal_t>(res.m_memory)
                                                               : std::get<literal_t*>(res.m_memory);
    }

    /**
     * @brief Check if an ExpressionResult holds an integer
     * @param res The ExpressionResult to check
     * @return true if holds an integer, false otherwise
     */
    static bool is_int(const ExpressionResult& res) { return std::holds_alternative<int>(*get_pmem(res)); }

    /**
     * @brief Check if an ExpressionResult holds a double
     * @param res The ExpressionResult to check
     * @return true if holds a double, false otherwise
     */
    static bool is_double(const ExpressionResult& res) { return std::holds_alternative<double>(*get_pmem(res)); }

    /**
     * @brief Check if an ExpressionResult holds a string
     * @param res The ExpressionResult to check
     * @return true if holds a string, false otherwise
     */
    static bool is_string(const ExpressionResult& res) { return std::holds_alternative<std::string>(*get_pmem(res)); }

    exec_expr_t m_left{};                /// Left child expression
    exec_expr_t m_right{};               /// Right child expression
    const parser::parse_expr_t m_expr{}; /// The parse expression
};

/**
 * @brief Expression class for primary expressions (literals and identifiers)
 */
class PrimaryExpression final : public Expression {
  public:
    /**
     * @brief Construct a new PrimaryExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    PrimaryExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the PrimaryExpression
     */
    ~PrimaryExpression() final = default;

    /**
     * @brief Evaluate the primary expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

class PostfixExpression final : public Expression {
  public:
};

/**
 * @brief Expression class for unary operations
 */
class UnaryExpression final : public Expression {
  public:
    /**
     * @brief Construct a new UnaryExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    UnaryExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the UnaryExpression
     */
    ~UnaryExpression() final = default;

    /**
     * @brief Evaluate the unary expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for addition operations
 */
class AddExpression final : public Expression {
  public:
    /**
     * @brief Construct a new AddExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    AddExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the AddExpression
     */
    ~AddExpression() final = default;

    /**
     * @brief Evaluate the addition expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for subtraction operations
 */
class SubExpression final : public Expression {
  public:
    /**
     * @brief Construct a new SubExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    SubExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the SubExpression
     */
    ~SubExpression() final = default;

    /**
     * @brief Evaluate the subtraction expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for multiplication operations
 */
class MulExpression final : public Expression {
  public:
    /**
     * @brief Construct a new MulExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    MulExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the MulExpression
     */
    ~MulExpression() final = default;

    /**
     * @brief Evaluate the multiplication expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for division operations
 */
class DivExpression final : public Expression {
  public:
    /**
     * @brief Construct a new DivExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    DivExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the DivExpression
     */
    ~DivExpression() final = default;

    /**
     * @brief Evaluate the division expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for assignment operations
 */
class AssignExpression final : public Expression {
  public:
    /**
     * @brief Construct a new AssignExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    AssignExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the AssignExpression
     */
    ~AssignExpression() final = default;

    /**
     * @brief Evaluate the assignment expression
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for function declarations
 */
class FuncDeclExpression final : public Expression {
  public:
    /**
     * @brief Construct a new FuncDeclExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    FuncDeclExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the FuncDeclExpression
     */
    ~FuncDeclExpression() final = default;

    /**
     * @brief Evaluate the function declaration
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for expression lists
 */
class ExprListExpression final : public Expression {
  public:
    /**
     * @brief Construct a new ExprListExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    ExprListExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the ExprListExpression
     */
    ~ExprListExpression() final = default;

    /**
     * @brief Evaluate the expression list
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for return statements
 */
class ReturnStmntExpression final : public Expression {
  public:
    /**
     * @brief Construct a new ReturnStmntExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    ReturnStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the ReturnStmntExpression
     */
    ~ReturnStmntExpression() final = default;

    /**
     * @brief Evaluate the return statement
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for break statements
 */
class BreakStmntExpression final : public Expression {
  public:
    /**
     * @brief Construct a new BreakStmntExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    BreakStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the BreakStmntExpression
     */
    ~BreakStmntExpression() final = default;

    /**
     * @brief Evaluate the break statement
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for function calls
 */
class FuncCallExpression final : public Expression {
  public:
    /**
     * @brief Construct a new FuncCallExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    FuncCallExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the FuncCallExpression
     */
    ~FuncCallExpression() final = default;

    /**
     * @brief Evaluate the function call
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;

  private:
    /**
     * @brief Transfer call arguments to the function's stack frame
     * @param func_decl_expr The function declaration expression
     * @param alloc Pointer to the allocator
     */
    void transfer_call_args(Expression* func_decl_expr, ExecutorAllocator* alloc);
};

/**
 * @brief Expression class for if statements
 */
class IfStmntExpression final : public Expression {
  public:
    /**
     * @brief Construct a new IfStmntExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    IfStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the IfStmntExpression
     */
    ~IfStmntExpression() final = default;

    /**
     * @brief Evaluate the if statement
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for for loops
 */
class ForStmntExpression final : public Expression {
  public:
    /**
     * @brief Construct a new ForStmntExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    ForStmntExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the ForStmntExpression
     */
    ~ForStmntExpression() final = default;

    /**
     * @brief Evaluate the for loop
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for print statements
 */
class PrintStmnt final : public Expression {
  public:
    /**
     * @brief Construct a new PrintStmnt
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    PrintStmnt(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the PrintStmnt
     */
    ~PrintStmnt() final = default;

    /**
     * @brief Evaluate the print statement
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for logical operations (AND, OR)
 */
class LogicalOpExpression final : public Expression {
  public:
    /**
     * @brief Construct a new LogicalOpExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    LogicalOpExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the LogicalOpExpression
     */
    ~LogicalOpExpression() final = default;

    /**
     * @brief Evaluate the logical operation
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

/**
 * @brief Expression class for logical comparisons (==, !=, <, >, etc.)
 */
class LogicalCompExpression final : public Expression {
  public:
    /**
     * @brief Construct a new LogicalCompExpression
     * @param exec Pointer to the Executor
     * @param expr The parse expression
     */
    LogicalCompExpression(Executor* exec, const parser::parse_expr_t expr) : Expression(exec, expr) {}

    /**
     * @brief Destroy the LogicalCompExpression
     */
    ~LogicalCompExpression() final = default;

    /**
     * @brief Evaluate the logical comparison
     * @param alloc Pointer to the allocator
     * @return The result of evaluation
     */
    ExpressionResult eval(ExecutorAllocator* alloc) final;
};

} // namespace interpreter