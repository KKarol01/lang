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
                // assert(false); todo: fix that
                // std::println("[{} | {}] : {}", "", "VOID");
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
    case parser::Expression::Type::LOGICAL_COMPARE: {
        return std::make_unique<LogicalCompExpression>(this, expr);
    }
    case parser::Expression::Type::LOGICAL_OP: {
        return std::make_unique<LogicalOpExpression>(this, expr);
    }
    case parser::Expression::Type::IF_STMNT: {
        return std::make_unique<IfStmntExpression>(this, expr);
    }
    default: {
        assert(false);
        return nullptr;
    }
    }
}

literal_t& ExecutorAllocator::StackFrame::get_allocation(const std::string& var_name) {
    auto it = std::find_if(m_variables.begin(), m_variables.end(),
                           [var_name](const auto& var) { return var.first == var_name; });
    if(it == m_variables.end()) {
        m_variables.push_back({ var_name, {} });
        it = m_variables.begin() + (m_variables.size() - 1);
    }
    return it->second;
}

literal_t* ExecutorAllocator::StackFrame::try_find_allocation(const std::string& var_name) {
    auto it = std::find_if(m_variables.begin(), m_variables.end(),
                           [&var_name](const auto& var) { return var.first == var_name; });
    if(it == m_variables.end()) { return nullptr; }
    return &it->second;
}

Expression::Expression(Executor* exec, const parser::parse_expr_t expr) : m_expr(expr) {
    m_left = exec->make_expr(expr->m_left);
    m_right = exec->make_expr(expr->m_right);
}

//void Expression::assign(literal_t& literal, const ExpressionResult& res) {
//    if(res.m_return_values.empty()) {
//        literal = *get_pmem(res);
//        return;
//    }
//    literal = res.m_return_values.back();
//}

void Expression::assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc) {
    assert(right->m_return_values.empty());
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
    if(m_right->get_parse_expr()->m_type != parser::Expression::Type::FUNC_CALL) {
        auto assignee = m_left->eval(alloc);
        auto assigned = m_right->eval(alloc);
        assign(&assignee, &assigned, alloc);
        return assignee;
    } else {
        auto ret_vals = m_right->eval(alloc);
        assert(!ret_vals.m_return_values.empty());
        int ret_idx = 0;
        dfs_traverse_expr_list(&*m_left, [alloc, &ret_vals, &ret_idx](Expression* expr) {
            assert(expr->get_parse_expr()->m_type == parser::Expression::Type::PRIMARY);
            alloc->get_top_stack_frame().get_allocation(std::string{ expr->get_node_value() }) =
                ret_vals.m_return_values.at(ret_idx++);
        });
        return ExpressionResult{ .m_memory = literal_t{ ret_vals.m_return_values.back() }, .m_type = m_expr->m_node.m_type };
    }
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
#if 1
    ExpressionResult res{ .m_type = m_expr->m_node.m_type };
    dfs_traverse_expr_list(&*m_left, [&res, alloc](Expression* expr) {
        res.m_return_values.push_back(*get_pmem(expr->eval(alloc)));
    });
    return res;
#else
    std::vector<literal_t> results;
    std::stack<Expression*> expr_list_stack;
    expr_list_stack.push(&*m_left);
    while(!expr_list_stack.empty()) {
        auto expr = expr_list_stack.top();
        expr_list_stack.pop();
        if(expr->m_left) {
            if(expr->m_left->m_expr->m_type == parser::Expression::Type::EXPR_LIST) {
                expr_list_stack.push(&*expr->m_left);
            } else {
                results.push_back(*get_pmem(expr->m_left->eval(alloc)));
            }
        }
        if(!expr->m_left && !expr->m_right) { results.push_back(*get_pmem(expr->eval(alloc))); }
        if(expr->m_right) { results.push_back(*get_pmem(expr->m_right->eval(alloc))); }
    }
    return ExpressionResult{ .m_return_values = { results.rbegin(), results.rend() }, .m_type = m_expr->m_node.m_type };
#endif
}

ExpressionResult FuncCallExpression::eval(ExecutorAllocator* alloc) {
    auto* func_decl = alloc->m_executor->get_func_decl(m_expr->m_node.m_value);
    transfer_call_args(func_decl, alloc);
    auto ret_values = func_decl->m_right->eval(alloc);
    alloc->m_stack_frames.pop_front();
    return ret_values;
}

void FuncCallExpression::transfer_call_args(Expression* func_decl_expr, ExecutorAllocator* alloc) {
#if 1
    ExecutorAllocator::StackFrame sf;
    dfs_traverse(&*func_decl_expr->m_left, [&sf](Expression* expr) {
        if(expr->get_parse_expr()->m_type == parser::Expression::Type::PRIMARY) {
            sf.get_allocation(std::string{ expr->get_node_value() });
        }
    });
    uint32_t offset = 0;
    dfs_traverse_expr_list(&*m_left, [alloc, &sf, &offset](Expression* expr) {
        (sf.m_variables.begin() + offset++)->second = *get_pmem(expr->eval(alloc));
    });
    alloc->m_stack_frames.push_front(std::move(sf));
#else
    std::stack<Expression*> expr_list_stack;
    std::stack<Expression*> param_list_stack;
    std::queue<std::string> param_list_var_names;
    expr_list_stack.push(&*m_left);
    param_list_stack.push(&*func_decl_expr->m_left);
    while(!param_list_stack.empty()) {
        auto expr = param_list_stack.top();
        param_list_stack.pop();
        if(expr->m_expr->m_type != parser::Expression::Type::EXPR_LIST) { // for param list with one param
            param_list_var_names.push(expr->m_expr->m_node.m_value);
            break;
        }
        if(expr->m_right) { param_list_var_names.push(expr->m_right->m_expr->m_node.m_value); }
        if(expr->m_left) {
            if(expr->m_left->m_expr->m_type == parser::Expression::Type::EXPR_LIST) {
                param_list_stack.push(&*expr->m_left);
            } else {
                param_list_var_names.push(expr->m_left->m_expr->m_node.m_value);
            }
        }
    }
    ExecutorAllocator::StackFrame stack_frame;
    while(!expr_list_stack.empty()) {
        auto expr = expr_list_stack.top();
        expr_list_stack.pop();
        if(expr->m_right) {
            auto& val = *get_pmem(expr->m_right->eval(alloc));
            stack_frame.get_allocation(param_list_var_names.front()) = val;
            param_list_var_names.pop();
        }
        if(expr->m_left) {
            if(expr->m_left->m_expr->m_type == parser::Expression::Type::EXPR_LIST) {
                expr_list_stack.push(&*expr->m_left);
            } else {
                auto& val = *get_pmem(expr->m_left->eval(alloc));
                stack_frame.get_allocation(param_list_var_names.front()) = val;
                param_list_var_names.pop();
            }
        }
        if(!expr->m_left && !expr->m_right && expr->m_expr->m_type != parser::Expression::Type::EXPR_LIST) {
            auto& val = *get_pmem(expr->eval(alloc)); // todo: else above probably is dead code
            stack_frame.get_allocation(param_list_var_names.front()) = val;
            param_list_var_names.pop();
        }
    }
    alloc->m_stack_frames.push_front(std::move(stack_frame));
#endif
}

ExpressionResult IfStmntExpression::eval(ExecutorAllocator* alloc) {
    auto condition = m_left->eval(alloc);
    if(std::get<int>(*get_pmem(condition)) == 1) {
        alloc->m_stack_frames.push_front(alloc->get_top_stack_frame());
        m_right->eval(alloc);
        auto front = alloc->m_stack_frames.front();
        alloc->m_stack_frames.pop_front();
        for(auto& [name, val] : front.m_variables) {
            auto* palloc = alloc->get_top_stack_frame().try_find_allocation(name);
            if(palloc) { *palloc = val; }
        }
    }
    return ExpressionResult{ .m_type = m_expr->m_node.m_type };
}

ExpressionResult LogicalOpExpression::eval(ExecutorAllocator* alloc) {
    auto left = m_left->eval(alloc);
    auto right = m_right->eval(alloc);
    if(!(is_int(left) && is_int(right))) {
        assert(false);
        return ExpressionResult{ .m_memory = literal_t{ 0 }, .m_type = m_expr->m_node.m_type };
    }

    int res = 0;
    if(m_expr->m_node.m_type == lexer::Token::Type::LOGICAL_AND) {
        res = std::get<int>(*get_pmem(left)) * std::get<int>(*get_pmem(right));
    } else {
        Logger::DebugWarn("Unhandled logical operator: {}", get_node_value());
        assert(false);
    }

    res = std::min(1, std::max(0, res));
    return ExpressionResult{ .m_memory = literal_t{ res }, .m_type = m_expr->m_node.m_type };
}

ExpressionResult LogicalCompExpression::eval(ExecutorAllocator* alloc) {
    auto left = m_left->eval(alloc);
    auto right = m_right->eval(alloc);
    if(!(is_int(left) || is_double(left)) && !(is_double(right) || is_double(right))) {
        Logger::DebugWarn("Trying to compare number with string or string with string: {} {} {}",
                          m_left->get_node_value(), get_node_value(), m_right->get_node_value());
        assert(false); // todo: probably throw
        return ExpressionResult{ .m_memory = literal_t{ 0 }, .m_type = m_expr->m_node.m_type };
    }

    double vleft = is_int(left) ? std::get<int>(*get_pmem(left)) : std::get<double>(*get_pmem(left));
    double vright = is_int(right) ? std::get<int>(*get_pmem(right)) : std::get<double>(*get_pmem(right));
    int res = 0;
    if(m_expr->m_node.m_type == lexer::Token::Type::LT) {
        res = (vleft < vright) ? 1 : 0;
    } else if(m_expr->m_node.m_type == lexer::Token::Type::GT) {
        res = (vleft > vright) ? 1 : 0;
    } else {
        Logger::DebugWarn("Unhandled comparison operator: {}", get_node_value());
        assert(false);
    }
    return ExpressionResult{ .m_memory = literal_t{ res }, .m_type = m_expr->m_node.m_type };
}

} // namespace interpreter
