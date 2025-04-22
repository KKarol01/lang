#pragma once

#include <vector>
#include <utility>
#include <stack>
#include <deque>
#include "logger.hpp"
#include "lexer.hpp"

namespace parser {

using parse_node_t = lexer::Token;              /// Type alias for parser nodes (using lexer tokens)
using parse_stack_t = std::stack<parse_node_t>; /// Type alias for the parser's stack

struct Expression;
using parse_expr_t = Expression*;            /// Type alias for expression pointers
using program_t = std::vector<parse_expr_t>; /// Type alias for a program as a vector of expressions

/**
 * @brief Represents an abstract syntax tree (AST) node
 *
 * This struct stores information about expressions in the parsed program,
 * including their type, associated token, and child expressions.
 */
struct Expression {
    /**
     * @brief Enumeration of all possible expression types
     */
    enum class Type {
        NONE,            /// Uninitialized expression
        PRIMARY,         /// Primary expressions (literals, identifiers)
        POSTFIX,         /// Postfix operations (++, --)
        UNARY,           /// Unary operations (+, -, ++, --)
        SUB,             /// Subtraction operation
        MUL,             /// Multiplication operation
        DIV,             /// Division operation
        ADD,             /// Addition operation
        ASSIGN,          /// Assignment operation (=)
        LOGICAL_COMPARE, /// Comparison operations (<, >, ==, etc.)
        LOGICAL_OP,      /// Logical operations (&&, ||)
        FUNC_DECL,       /// Function declaration
        FUNC_CALL,       /// Function call
        EXPR_LIST,       /// Expression list (comma-separated)
        RETURN_STMNT,    /// Return statement
        IF_STMNT,        /// If statement
        FOR_STMNT,       /// For loop statement
        BREAK_STMNT,     /// Break statement
        PRINT_STMNT,     /// Print statement
    };

    Type m_type{ Type::NONE }; /// The type of this expression
    parse_node_t m_node{};     /// The token associated with this expression
    parse_expr_t m_left{};     /// Left child expression (if any)
    parse_expr_t m_right{};    /// Right child expression (if any)
};

/**
 * @brief Utility class for working with Expression types
 */
struct ExpressionUtils {
    /**
     * @brief Get the name of an expression type as a string
     *
     * @param type The expression type to get the name for
     * @return const char* The name of the expression type
     */
    static const char* get_expression_name(Expression::Type type) {
        const auto idx = std::to_underlying(type);
        if(idx >= sizeof(s_expr_names) / sizeof(s_expr_names[0])) {
            Logger::DebugWarn("Invalid name for token with idx {}", idx);
            return s_expr_names[0];
        }
        return s_expr_names[idx];
    }

    inline static constexpr const char* s_expr_names[]{
        /// Array of expression type names
        "NONE",         "PRIMARY",  "POSTFIX",         "UNARY",       "SUB",         "MUL",       "DIV",
        "ADD",          "ASSIGN",   "LOGICAL_COMPARE", "LOGICAL_OP",  "FUNC_DECL",   "FUNC_CALL", "EXPR_LIST",
        "RETURN_STMNT", "IF_STMNT", "FOR_STMNT",       "BREAK_STMNT", "PRINT_STMNT",
    };
};

/**
 * @brief The main parser class that converts tokens into an abstract syntax tree (AST)
 *
 * This class implements a recursive descent parser that builds an AST from
 * a sequence of tokens provided by the lexer.
 */
class Parser {
  public:
    /**
     * @brief Construct a new Parser object
     *
     * @param tokens The vector of tokens to parse
     */
    Parser(const std::vector<lexer::Token>& tokens);

    /**
     * @brief Build the abstract syntax tree (AST) from the tokens
     *
     * @return std::vector<parse_expr_t> The parsed program as a vector of expressions
     */
    std::vector<parse_expr_t> build_ast();

  private:
    /**
     * @brief Helper function to put tokens onto the parse stack in reverse order
     *
     * @param tokens The tokens to put on the stack
     */
    void put_tokens_onto_parse_stack(const std::vector<parse_node_t>& tokens);

    /**
     * @brief Insert a terminator (semicolon) token onto the stack
     *
     * Used to handle automatic semicolon insertion in cases like
     * function declarations or control structures.
     */
    void insert_terminator();

    /**
     * @brief Get the top token from the stack without removing it
     *
     * @return parse_node_t& Reference to the top token
     */
    parse_node_t& get() { return m_stack.top(); }

    /**
     * @brief Remove and return the top token from the stack
     *
     * @return parse_node_t The removed token
     */
    parse_node_t take() {
        auto top = m_stack.top();
        m_stack.pop();
        return top;
    }

    /**
     * @brief Check if the current token matches the expected type
     *
     * @param expected The expected token type
     * @param take Whether to consume the token if it matches
     * @throws std::runtime_error If the token doesn't match the expected type
     */
    void check_or_throw(parse_node_t::Type expected, bool take = false);

    /**
     * @brief Create a new empty expression in the AST
     *
     * @return parse_expr_t Pointer to the new expression
     */
    parse_expr_t make_expr() { return &m_ast.emplace_back(); }

    /**
     * @brief Create a new expression in the AST with the given values
     *
     * @param expr The expression to copy into the AST
     * @return parse_expr_t Pointer to the new expression
     */
    parse_expr_t make_expr(const Expression& expr) { return &m_ast.emplace_back(expr); }

    // Expression parsing methods
    parse_expr_t parse_prim_expr();        /// Parse primary expressions
    parse_expr_t parse_func_expr();        /// Parse function expressions
    parse_expr_t parse_if_expr();          /// Parse if expressions
    parse_expr_t parse_for_expr();         /// Parse for expressions
    parse_expr_t parse_post_expr();        /// Parse postfix expressions
    parse_expr_t parse_unar_expr();        /// Parse unary expressions
    parse_expr_t parse_mul_expr();         /// Parse multiplication/division expressions
    parse_expr_t parse_add_sub_expr();     /// Parse addition/subtraction expressions
    parse_expr_t parse_comp_expr();        /// Parse comparison expressions
    parse_expr_t parse_logical_expr();     /// Parse logical expressions
    parse_expr_t parse_assign_expr();      /// Parse assignment expressions
    parse_expr_t parse_expr_list();        /// Parse expression lists
    parse_expr_t parse_expr();             /// Parse generic expressions
    parse_expr_t parse_return_statement(); /// Parse return statements
    parse_expr_t parse_break_statement();  /// Parse break statements
    parse_expr_t parse_print_statement();  /// Parse print statements
    parse_expr_t parse_statement();        /// Parse statements

    program_t m_program;          /// The parsed program as a sequence of expressions
    parse_stack_t m_stack;        /// The stack used during parsing
    std::deque<Expression> m_ast; /// Storage for all AST nodes
};

/**
 * @brief Print the abstract syntax tree (AST) to stdout
 *
 * @param node The root node of the AST to print
 * @param indent The current indentation level (used for pretty printing)
 */
void print_ast(const parser::parse_expr_t node, int indent = 0);

}; // namespace parser