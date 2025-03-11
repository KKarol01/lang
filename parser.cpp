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

parse_expr_t Parser::parse_prim_expr() {
    auto node = get();
    if(!(node.m_type == parse_node_t::Type::IDENTIFIER || node.m_type == parse_node_t::Type::INT ||
         node.m_type == parse_node_t::Type::DOUBLE || node.m_type == parse_node_t::Type::STRING ||
         node.m_type == parse_node_t::Type::FUNC || node.m_type == parse_node_t::Type::IF)) {
        assert(false);
        return nullptr; // todo: maybe throw here
    }
    m_stack.pop();
    return make_expr(Expression{ .m_type = Expression::Type::PRIMARY, .m_node = node });
}

parse_expr_t Parser::parse_if_expr() {
    auto if_kw = parse_prim_expr();
    if(if_kw->m_node.m_type != parse_node_t::Type::IF) { return if_kw; }
    auto if_cond = parse_logical_expr();
    assert(get().m_type == parse_node_t::Type::COLON);
    take();
    assert(get().m_type == parse_node_t::Type::BRA_OPEN);
    take();
    auto if_body = parse_statement();
    while(get().m_type != parse_node_t::Type::BRA_CLOSE) {
        auto right = parse_statement();
        if_body = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_left = if_body, .m_right = right });
    }
    take();
    insert_terminator();
    return make_expr(Expression{ .m_type = Expression::Type::IF_STMNT, .m_node = if_kw->m_node, .m_left = if_cond, .m_right = if_body });
}

parse_expr_t Parser::parse_func_expr() {
    parse_expr_t func_kw = parse_if_expr();
    parse_expr_t func_name{};
    parse_expr_t func_param_list{};
    parse_expr_t func_body{};
    if(func_kw->m_node.m_type == parse_node_t::Type::FUNC) {
        assert(get().m_type == parse_node_t::Type::IDENTIFIER);
        func_name = parse_prim_expr();
    } else if(func_kw->m_node.m_type == parse_node_t::Type::IDENTIFIER && get().m_type == parse_node_t::Type::PAR_OPEN) {
        func_name = func_kw;
    } else {
        return func_kw;
    }

    assert(get().m_type == parse_node_t::Type::PAR_OPEN);
    take();
    if(get().m_type == parse_node_t::Type::PAR_CLOSE) {
        func_param_list = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST });
    } else {
        func_param_list = parse_expr_list();
    }
    assert(get().m_type == parse_node_t::Type::PAR_CLOSE);
    take();

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
    while(get().m_type == parse_node_t::Type::LT || get().m_type == parse_node_t::Type::GT) {
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

parse_expr_t Parser::parse_assign_expr() {
    auto left = parse_logical_expr();
    while(get().m_type == parse_node_t::Type::EQUALS) {
        auto node = take();
        auto right = parse_logical_expr();
        left = make_expr(Expression{ .m_type = Expression::Type::ASSIGN, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_expr_list() {
    auto left = parse_assign_expr();
    while(get().m_type == parse_node_t::Type::COMMA) {
        auto node = take();
        auto right = parse_assign_expr();
        left = make_expr(Expression{ .m_type = Expression::Type::EXPR_LIST, .m_node = node, .m_left = left, .m_right = right });
    }
    return left;
}

parse_expr_t Parser::parse_expr() { return parse_expr_list(); }

parse_expr_t Parser::parse_return_statement() {
    if(get().m_type == lexer::Token::Type::RETURN) {
        auto ret = take();
        auto list = parse_expr_list();
        return make_expr(Expression{ .m_type = Expression::Type::RETURN_STMNT, .m_node = ret, .m_left = list });
    }
    return parse_expr_list();
}

parse_expr_t Parser::parse_statement() {
    auto list = parse_return_statement();
    assert(get().m_type == parse_node_t::Type::TERMINATOR);
    take();
    return list;
}

void print_ast(const parser::parse_expr_t node, int indent) {
    if(node == nullptr) { return; }

    for(int i = 0; i < indent; ++i) {
        std::cout << "  ";
    }
    Logger::DebugLog("[TODO]: fix the println below");
    // std::println("{} ({} | {})", node->m_node.m_value, lexer::get_token_type_name(node->m_node.m_type),
    //              parser::get_expr_name(node->m_type));

    print_ast(node->m_left, indent + 1);
    print_ast(node->m_right, indent + 1);
}

} // namespace parser
