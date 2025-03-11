#include <iostream>
#include <filesystem>
#include <fstream>
#include <string_view>
#include <cassert>
#include <deque>
#include <queue>
#include <print>
#include <variant>
#include <string>
#include <stack>
#include <any>
#include <unordered_map>

#include "lexer.hpp"
#include "interpreter.hpp"

auto read_source_code() {
    std::ifstream file{ "script.lang" };
    if(!file.is_open()) { return std::string{}; }
    file.seekg(0, std::ios::end);
    const size_t size = file.tellg();
    std::string content(size, '\0');
    file.seekg(0);
    file.read(content.data(), content.size());
    return content;
}

int main() {
    using namespace lexer;
    using namespace parser;
    Logger::DebugWarn("Invalid name for token with idx {}", 1);
    lexer::Tokenizer tokenizer;

    const auto source_code = read_source_code();
    const auto tokens = tokenizer.tokenize(source_code);

    parser::Parser p{ tokens };
    auto program = p.build_ast();
    for(auto& p : program) {
        print_ast(p);
    }
    interpreter::Executor exec{ program };
}