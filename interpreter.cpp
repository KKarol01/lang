#include "interpreter.hpp"
#include <cassert>
#include <queue>
#include <stack>
#include <print>

namespace interpreter {
ExecutorAllocator::ExecutorAllocator(Executor* exec) : m_executor(exec) {}

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

literal_t& ExecutorAllocator::StackFrame::get_allocation(const std::string& var_name) {
    auto ret = m_variables.insert({ var_name, {} });
    if(ret.second) { m_variable_creation_order.push_back(var_name); }
    return ret.first->second;
}

Expression::Expression(Executor* exec, const parser::parse_expr_t expr) : m_expr(expr) {
    m_left = exec->make_expr(expr->m_left);
    m_right = exec->make_expr(expr->m_right);
}

void Expression::assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc) {
    if(!right->m_return_values.empty()) {
        // handles assignments to variables from function calls: a, k = f()
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

ExpressionResult PrimaryExpression::eval(ExecutorAllocator* alloc) {
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

ExpressionResult UnaryExpression::eval(ExecutorAllocator* alloc) {
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

ExpressionResult AddExpression::eval(ExecutorAllocator* alloc) {
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

ExpressionResult MulExpression::eval(ExecutorAllocator* alloc) {
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

ExpressionResult DivExpression::eval(ExecutorAllocator* alloc) {
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

ExpressionResult AssignExpression::eval(ExecutorAllocator* alloc) {
    auto assignee = m_left->eval(alloc);
    auto assigned = m_right->eval(alloc);
    assign(&assignee, &assigned, alloc);
    return assignee;
}

ExpressionResult FuncDeclExpression::eval(ExecutorAllocator* alloc) {
    assert(false);
    return ExpressionResult{};
}

ExpressionResult ExprListExpression::eval(ExecutorAllocator* alloc) {
    m_left->eval(alloc);
    return m_right->eval(alloc);
}

ExpressionResult ReturnStmntExpression::eval(ExecutorAllocator* alloc) {
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

ExpressionResult FuncCallExpression::eval(ExecutorAllocator* alloc) {
    auto* func_decl = alloc->m_executor->get_func_decl(m_expr->m_node.m_value);
    transfer_call_args(func_decl, alloc);
    auto ret_values = func_decl->m_right->eval(alloc);
    alloc->m_stack_frames.pop_front();
    return ret_values;
}

void FuncCallExpression::transfer_call_args(Expression* func_decl_expr, ExecutorAllocator* alloc) {
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

} // namespace interpreter
