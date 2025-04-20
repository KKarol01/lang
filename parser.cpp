#include "parser.hpp"
#include <cassert>
#include <iostream>
#include <print>

namespace parser {

Parser::Parser(const std::vector<lexer::Token>& tokens) { put_tokens_onto_parse_stack(tokens); }

std::vector<parse_expr_t> Parser::build_ast() {
    m_program.clear();
    while(!m_stack.empty()) {
        m_program.push_back(parse_statement());
    }
    return m_program;
}

void Parser::put_tokens_onto_parse_stack(const std::vector<parse_node_t>& tokens) {
    for(auto it = tokens.rbegin(); it != tokens.rend(); ++it) {
        m_stack.push(*it);
    }
}

void Parser::insert_terminator() {
    m_stack.push(parse_node_t{ .m_value = ";", .m_type = lexer::Token::Type::TERMINATOR, .m_category = lexer::Token::Category::TERMINATOR });
}

void Parser::check_or_throw(parse_node_t::Type expected, bool take) {
    if(get().m_type != expected) {
        assert(false);
        throw std::runtime_error{ std::format("[{}] Expected {}, but got {}", get().line_number,
                                              lexer::TokenUtils::get_token_name(expected),
                                              lexer::TokenUtils::get_token_name(get().m_type)) };
    }
    if(take) { this->take(); }
}

parse_expr_t Parser::parse_prim_expr() {
    auto node = get();
    if(!(node.m_type == parse_node_t::Type::IDENTIFIER || node.m_type == parse_node_t::Type::INT ||
         node.m_type == parse_node_t::Type::DOUBLE || node.m_type == parse_node_t::Type::STRING ||
         node.m_type == parse_node_t::Type::FUNC || node.m_type == parse_node_t::Type::IF ||
         node.m_type == parse_node_t::Type::FOR || node.m_type == parse_node_t::Type::BREAK)) {
        assert(false);
        return nullptr; // todo: maybe throw here
    }
    take();
    return make_expr(Expression{ .m_type = Expression::Type::PRIMARY, .m_node = node });
}

parse_expr_t Parser::parse_if_expr() {
    auto if_kw = parse_prim_expr();
    if(if_kw->m_node.m_type != parse_node_t::Type::IF) { return if_kw; }
    auto if_cond = parse_logical_expr();
    check_or_throw(parse_node_t::Type::COLON, true);
    check_or_throw(parse_node_t::Type::BRA_OPEN, true);
    auto if_body = parse_statement();
    while(get().m_type != parse_node_t::Type::BRA_CLOSE) {
        auto right = parse_statement();
        if_body = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = if_body, .m_right = right });
    }
    take();

    if(get().m_type == parse_node_t::Type::ELSE) {
        take();
        if(get().m_type != parse_node_t::Type::IF) {
            // handle else {} by transforming it to: else if 1: {}, so always true else if, that has to be handled anyway
            check_or_throw(parse_node_t::Type::BRA_OPEN);
            m_stack.push(parse_node_t{
                .m_value = ":", .m_type = lexer::Token::Type::COLON, .m_category = lexer::Token::Category::OPERATOR });
            m_stack.push(parse_node_t{ .m_value = "1", .m_type = lexer::Token::Type::INT, .m_category = lexer::Token::Category::NUMBER });
            m_stack.push(parse_node_t{ .m_value = "if", .m_type = lexer::Token::Type::IF, .m_category = lexer::Token::Category::KEYWORD });
        }
        check_or_throw(parse_node_t::Type::IF);
        auto nested_ifs = parse_if_expr();
        if_cond = make_expr(Expression{
            .m_type = Expression::Type::IF_STMNT,
            .m_left = make_expr(Expression{
                .m_type = Expression::Type::IF_STMNT, .m_node = if_kw->m_node, .m_left = if_cond, .m_right = if_body }),
            .m_right = nested_ifs });
    } else {
        insert_terminator();
    }
    return make_expr(Expression{ .m_type = Expression::Type::IF_STMNT, .m_node = if_kw->m_node, .m_left = if_cond, .m_right = if_body });
}

parse_expr_t Parser::parse_for_expr() {
    auto for_expr = parse_if_expr();
    if(for_expr->m_node.m_type != parse_node_t::Type::FOR) { return for_expr; }

    parse_expr_t for_assignments{};
    while(get().m_type != parse_node_t::Type::TERMINATOR) {
        for_assignments = make_expr(Expression{
            .m_type = Expression::Type::EXPR_LIST, .m_left = for_assignments, .m_right = parse_assign_expr() });
    }
    take();
    parse_expr_t logic_expr = parse_logical_expr();
    check_or_throw(parse_node_t::Type::TERMINATOR, true);
    parse_expr_t it_ops = parse_assign_expr();
    while(get().m_type != parse_node_t::Type::COLON) {
        it_ops = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = it_ops, .m_right = parse_assign_expr() });
    }
    take();
    check_or_throw(parse_node_t::Type::BRA_OPEN, true);
    parse_expr_t func_body = parse_statement();
    while(get().m_type != parse_node_t::Type::BRA_CLOSE) {
        auto right = parse_statement();
        func_body = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = func_body, .m_right = right });
    }
    take();
    for_expr->m_type = Expression::Type::FOR_STMNT;
    for_expr->m_left =
        make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = for_assignments, .m_right = logic_expr });
    for_expr->m_right = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = it_ops, .m_right = func_body });
    insert_terminator();
    return for_expr;
}

parse_expr_t Parser::parse_func_expr() {
    parse_expr_t func_kw = parse_for_expr();
    parse_expr_t func_name{};
    parse_expr_t func_param_list{};
    parse_expr_t func_body{};
    if(func_kw->m_node.m_type == parse_node_t::Type::FUNC) {
        check_or_throw(parse_node_t::Type::IDENTIFIER);
        func_name = parse_prim_expr();
    } else if(func_kw->m_node.m_type == parse_node_t::Type::IDENTIFIER && get().m_type == parse_node_t::Type::PAR_OPEN) {
        func_name = func_kw;
    } else {
        return func_kw;
    }

    check_or_throw(parse_node_t::Type::PAR_OPEN, true);
    if(get().m_type == parse_node_t::Type::PAR_CLOSE) {
        func_param_list = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST });
    } else {
        func_param_list = parse_expr_list();
    }
    check_or_throw(parse_node_t::Type::PAR_CLOSE, true);

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

parse_expr_t Parser::parse_post_expr() {
    auto left = parse_func_expr();
    while(get().m_type == parse_node_t::Type::INC || get().m_type == parse_node_t::Type::DEC) {
        auto node = take();
        left = make_expr(Expression{ .m_type = Expression::Type::POSTFIX, .m_node = node, .m_left = left });
    }
    return left;
}

parse_expr_t Parser::parse_unar_expr() {
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

parse_expr_t Parser::parse_mul_expr() {
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

parse_expr_t Parser::parse_add_expr() {
    auto left = parse_mul_expr();
    while(get().m_type == parse_node_t::Type::PLUS || get().m_type == parse_node_t::Type::MIN) {
        auto node = take();
        auto right = parse_mul_expr();
        left = make_expr(Expression{ .m_type = Expression::Type::ADD, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_comp_expr() {
    auto left = parse_add_expr();
    while(get().m_type == parse_node_t::Type::LT || get().m_type == parse_node_t::Type::GT ||
          get().m_type == parse_node_t::Type::EQUALS) {
        auto node = take();
        auto right = parse_add_expr();
        left = make_expr(Expression{ .m_type = Expression::Type::LOGICAL_COMPARE, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_logical_expr() {
    auto left = parse_comp_expr();
    while(get().m_type == parse_node_t::Type::LOGICAL_AND) {
        auto node = take();
        auto right = parse_comp_expr();
        left = make_expr(Expression{ .m_type = Expression::Type::LOGICAL_OP, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_expr_list() {
    auto left = parse_logical_expr();
    while(get().m_type == parse_node_t::Type::COMMA) {
        auto node = take();
        auto right = parse_logical_expr();
        left = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_assign_expr() {
    auto left = parse_expr_list();
    while(get().m_type == parse_node_t::Type::ASSIGN) {
        auto node = take();
        auto right = parse_expr_list();
        left = make_expr(Expression{ .m_type = Expression::Type::ASSIGN, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_expr() { return parse_assign_expr(); }

parse_expr_t Parser::parse_return_statement() {
    if(get().m_type == lexer::Token::Type::RETURN) {
        auto ret = take();
        auto list = parse_assign_expr();
        return make_expr(Expression{ .m_type = Expression::Type::RETURN_STMNT, .m_node = ret, .m_left = list });
    }
    return parse_assign_expr();
}

parse_expr_t Parser::parse_break_statement() {
    if(get().m_type == lexer::Token::Type::BREAK) {
        auto ret = take();
        return make_expr(Expression{ .m_type = Expression::Type::BREAK_STMNT, .m_node = ret });
    }
    return parse_return_statement();
}

parse_expr_t Parser::parse_statement() {
    auto list = parse_break_statement();
    check_or_throw(parse_node_t::Type::TERMINATOR, true);
    return list;
}

void print_ast(const parser::parse_expr_t node, int indent) {
#ifdef DEBUG_PRINT_INFO
    if(node == nullptr) { return; }

    for(int i = 0; i < indent; ++i) {
        std::cout << "  ";
    }

    Logger::DebugLog("{} ({} | {})", node->m_node.m_value, lexer::TokenUtils::get_token_name(node->m_node.m_type),
                     parser::ExpressionUtils::get_expression_name(node->m_type));

    print_ast(node->m_left, indent + 1);
    print_ast(node->m_right, indent + 1);
#endif
}

} // namespace parser
