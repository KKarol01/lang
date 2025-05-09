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

#ifdef DEBUG_PRINT_INFO
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
#endif
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
    case parser::Expression::Type::SUB: {
        return std::make_unique<SubExpression>(this, expr);
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
    case parser::Expression::Type::BREAK_STMNT: {
        return std::make_unique<BreakStmntExpression>(this, expr);
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
    case parser::Expression::Type::FOR_STMNT: {
        return std::make_unique<ForStmntExpression>(this, expr);
    }
    case parser::Expression::Type::PRINT_STMNT: {
        return std::make_unique<PrintStmnt>(this, expr);
    }
    default: {
        Logger::DebugError("Unrecognized expression type: {}", parser::ExpressionUtils::get_expression_name(expr->m_type));
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
    if(expr->m_left) { m_left = exec->make_expr(expr->m_left); }
    if(expr->m_right) { m_right = exec->make_expr(expr->m_right); }
}

void Expression::assign(ExpressionResult* left, const ExpressionResult* right, ExecutorAllocator* alloc) {
    assert(right->m_return_values.empty());
    auto* assigned = get_pmem(*right);
    if(!std::holds_alternative<literal_t*>(left->m_memory)) {
        assert(false);
        throw std::runtime_error{
            std::format("[{}] Cannot assign to left-hand side of assign expression, because the memory is null.",
                        m_expr->m_node.line_number)
        };
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
            throw std::runtime_error{ std::format("[{}] Unrecognized primary expression type.", m_expr->m_node.line_number) };
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
        res.m_memory = literal_t{ (is_int(left) ? -std::get<int>(*get_pmem(left)) : -std::get<double>(*get_pmem(left))) };
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
        throw std::runtime_error{ std::format("[{}] Trying to add to unrelated types: {} + {}",
                                              m_expr->m_node.line_number, lexer::TokenUtils::get_token_name(left.m_type),
                                              lexer::TokenUtils::get_token_name(right.m_type)) };
    }
    return res;
}

ExpressionResult SubExpression::eval(ExecutorAllocator* alloc) {
    auto left = m_left->eval(alloc);
    auto right = m_right->eval(alloc);
    auto l_mem = get_pmem(left);
    auto r_mem = get_pmem(right);
    ExpressionResult res{ .m_type = m_expr->m_node.m_type };
    if((is_int(left) && is_double(right)) || (is_int(right) && is_double(left)) || (is_double(left) && is_double(right))) {
        res.m_memory = literal_t{ (double)(is_int(left) ? std::get<int>(*l_mem) : std::get<double>(*l_mem)) -
                                  (double)(is_int(right) ? std::get<int>(*r_mem) : std::get<double>(*r_mem)) };
    } else if(is_int(left) && is_int(right)) {
        res.m_memory = literal_t{ std::get<int>(*l_mem) - std::get<int>(*r_mem) };
    } else if(is_string(left) && is_string(right)) {
        throw std::runtime_error{ std::format("[{}] Trying to subtract strings. This is not allowed.", m_expr->m_node.line_number) };
    } else {
        assert(false);
        throw std::runtime_error{ std::format("[{}] Trying to subtract to unrelated types: {} - {}",
                                              m_expr->m_node.line_number, lexer::TokenUtils::get_token_name(left.m_type),
                                              lexer::TokenUtils::get_token_name(right.m_type)) };
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
        throw std::runtime_error{ std::format("[{}] Trying to multiply strings. This is not allowed.", m_expr->m_node.line_number) };
    } else {
        throw std::runtime_error{ std::format("[{}] Trying to multiply to unrelated types: {} * {}",
                                              m_expr->m_node.line_number, lexer::TokenUtils::get_token_name(left.m_type),
                                              lexer::TokenUtils::get_token_name(right.m_type)) };
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
        throw std::runtime_error{ std::format("[{}] Trying to divide strings. This is not allowed.", m_expr->m_node.line_number) };
    } else {
        throw std::runtime_error{ std::format("[{}] Trying to divide to unrelated types: {} / {}",
                                              m_expr->m_node.line_number, lexer::TokenUtils::get_token_name(left.m_type),
                                              lexer::TokenUtils::get_token_name(right.m_type)) };
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
        if(ret_vals.m_return_values.empty()) {
            assert(false);
            throw std::runtime_error{ std::format(
                "[{}] Func call does not return any values and it's result cannot be assigned to the variable {}",
                m_expr->m_node.line_number, m_expr->m_node.m_value) };
        }
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
    auto left_res = m_left->eval(alloc);
    if(alloc->get_top_stack_frame().m_return_stmn_hit || alloc->get_top_stack_frame().m_break_stmn_hit) {
        return left_res;
    }
    return m_right->eval(alloc);
}

ExpressionResult ReturnStmntExpression::eval(ExecutorAllocator* alloc) {
    ExpressionResult res{ .m_type = m_expr->m_node.m_type };
    dfs_traverse_expr_list(&*m_left, [&res, alloc](Expression* expr) {
        res.m_return_values.push_back(*get_pmem(expr->eval(alloc)));
    });
    alloc->get_top_stack_frame().m_return_stmn_hit = true;
    return res;
}

ExpressionResult BreakStmntExpression::eval(ExecutorAllocator* alloc) {
    alloc->get_top_stack_frame().m_break_stmn_hit = true;
    return ExpressionResult{ .m_type = m_expr->m_node.m_type };
}

ExpressionResult FuncCallExpression::eval(ExecutorAllocator* alloc) {
    auto* func_decl = alloc->m_executor->get_func_decl(m_expr->m_node.m_value);
    if(!func_decl) {
        throw std::runtime_error{ std::format("[{}] Function {} is not defined.", m_expr->m_node.line_number,
                                              m_expr->m_node.m_value) };
    }
    transfer_call_args(func_decl, alloc);
    auto ret_values = func_decl->m_right->eval(alloc);
    alloc->m_stack_frames.pop_front();
    return ret_values;
}

void FuncCallExpression::transfer_call_args(Expression* func_decl_expr, ExecutorAllocator* alloc) {
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
}

ExpressionResult IfStmntExpression::eval(ExecutorAllocator* alloc) {
    ++alloc->get_top_stack_frame().m_if_chain_recursion_level;
    auto condition = m_left->eval(alloc);

    if(alloc->get_top_stack_frame().m_prev_if_in_chain_succeded) {
        --alloc->get_top_stack_frame().m_if_chain_recursion_level;
        if(alloc->get_top_stack_frame().m_if_chain_recursion_level == 0) {
            alloc->get_top_stack_frame().m_prev_if_in_chain_succeded = false;
        }
        return condition;
    }
    ExpressionResult body_result;
    if(m_right->get_parse_expr()->m_type != parser::Expression::Type::IF_STMNT && std::get<int>(*get_pmem(condition)) == 1) {
        alloc->get_top_stack_frame().m_prev_if_in_chain_succeded = true;
        alloc->m_stack_frames.push_front(alloc->get_top_stack_frame());
        body_result = m_right->eval(alloc);
        auto front = alloc->m_stack_frames.front();
        alloc->m_stack_frames.pop_front();
        for(auto& [name, val] : front.m_variables) {
            auto* palloc = alloc->get_top_stack_frame().try_find_allocation(name);
            if(palloc) { *palloc = val; }
        }
        alloc->get_top_stack_frame().m_return_stmn_hit = front.m_return_stmn_hit;
    } else if(m_right->get_parse_expr()->m_type == parser::Expression::Type::IF_STMNT) {
        body_result = m_right->eval(alloc);
    }
    --alloc->get_top_stack_frame().m_if_chain_recursion_level;
    if(alloc->get_top_stack_frame().m_if_chain_recursion_level == 0) {
        alloc->get_top_stack_frame().m_prev_if_in_chain_succeded = false;
    }
    return body_result;
}

ExpressionResult LogicalOpExpression::eval(ExecutorAllocator* alloc) {
    auto left = m_left->eval(alloc);
    auto right = m_right->eval(alloc);
    if(!(is_int(left) && is_int(right))) {
        assert(false);
        throw std::runtime_error{ std::format("[{}] Operands of logical expression {} should both evaluate to ints",
                                              m_expr->m_node.line_number, m_expr->m_node.m_value) };
        return ExpressionResult{ .m_memory = literal_t{ 0 }, .m_type = m_expr->m_node.m_type };
    }

    int res = 0;
    if(m_expr->m_node.m_type == lexer::Token::Type::LOGICAL_AND) {
        res = std::get<int>(*get_pmem(left)) * std::get<int>(*get_pmem(right));
    } else if(m_expr->m_node.m_type == lexer::Token::Type::LOGICAL_OR) {
        res = std::max(std::get<int>(*get_pmem(left)), std::get<int>(*get_pmem(right)));
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
    const auto is_any_string = is_string(left) || is_string(right);
    const auto is_any_not_string = !is_string(left) || !is_string(right);
    if(is_any_string && is_any_not_string) {
        assert(false);
        throw std::runtime_error{
            std::format("[{}] Trying to compare number with string or string with string: {} {} {}",
                        m_expr->m_node.line_number, m_left->get_node_value(), get_node_value(), m_right->get_node_value())
        };
        return ExpressionResult{ .m_memory = literal_t{ 0 }, .m_type = m_expr->m_node.m_type };
    }

    const auto compare = [this](lexer::Token::Type type, const auto& a, const auto& b) {
        if(type == lexer::Token::Type::LT) {
            return a < b;
        } else if(type == lexer::Token::Type::GT) {
            return a > b;
        } else if(type == lexer::Token::Type::EQUALS) {
            return a == b;
        } else if(type == lexer::Token::Type::NOT_EQUALS) {
            return a != b;
        } else if(type == lexer::Token::Type::GEQ) {
            return a >= b;
        } else if(type == lexer::Token::Type::LEQ) {
            return a <= b;
        } else {
            Logger::DebugWarn("Unhandled comparison operator: {}", get_node_value());
            assert(false);
            return false;
        }
    };

    ExpressionResult result{ .m_type = m_expr->m_node.m_type };
    if(!is_any_not_string) {
        result.m_memory = literal_t{ compare(m_expr->m_node.m_type, std::get<std::string>(*get_pmem(left)),
                                             std::get<std::string>(*get_pmem(right))) };
    } else {
        double vleft = is_int(left) ? std::get<int>(*get_pmem(left)) : std::get<double>(*get_pmem(left));
        double vright = is_int(right) ? std::get<int>(*get_pmem(right)) : std::get<double>(*get_pmem(right));
        result.m_memory = literal_t{ compare(m_expr->m_node.m_type, vleft, vright) };
    }
    return result;
}

ExpressionResult ForStmntExpression::eval(ExecutorAllocator* alloc) {
    dfs_traverse_expr_list(&*m_left, [alloc](auto* e) { e->eval(alloc); });
    ExpressionResult res{};
    alloc->m_stack_frames.push_front(alloc->get_top_stack_frame());
    alloc->get_top_stack_frame().m_if_chain_recursion_level = 0;
    alloc->get_top_stack_frame().m_prev_if_in_chain_succeded = false;
    while(std::get<int>(*get_pmem(m_left->m_right->eval(alloc))) > 0) {
        res = m_right->m_right->eval(alloc);
        if(alloc->get_top_stack_frame().m_return_stmn_hit || alloc->get_top_stack_frame().m_break_stmn_hit) { break; }
        m_right->m_left->eval(alloc);
    }
    auto front = alloc->m_stack_frames.front();
    alloc->m_stack_frames.pop_front();
    for(auto& [name, val] : front.m_variables) {
        auto* palloc = alloc->get_top_stack_frame().try_find_allocation(name);
        if(palloc) { *palloc = val; }
    }
    alloc->get_top_stack_frame().m_return_stmn_hit = front.m_return_stmn_hit;

    return res;
}

ExpressionResult PrintStmnt::eval(ExecutorAllocator* alloc) {
    std::print("[COUT] ");
    dfs_traverse_expr_list(&*m_left, [alloc](auto* e) {
        auto ret = e->eval(alloc);
        literal_t* res = get_pmem(ret);
        assert(!std::holds_alternative<std::monostate>(*res));
        if(auto* e = std::get_if<int>(res)) {
            std::print("{}", *e);
        } else if(auto* e = std::get_if<double>(res)) {
            std::print("{}", *e);
        } else if(auto* e = std::get_if<std::string>(res)) {
            std::print("{}", *e);
        }
    });
    std::print("\n");
    return ExpressionResult{ .m_type = m_expr->m_node.m_type };
}

} // namespace interpreter
