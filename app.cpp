#include "app.hpp"
#include "logger.hpp"
#include "lexer.hpp"
#include "interpreter.hpp"
#include <fstream>

Application::Application(const std::filesystem::path& _source_code) : source_code(read_source_code(_source_code)) {
    if(source_code.empty()) {
        Logger::Warn("Source code file is empty. Cannot proceed.");
        return;
    }

    lexer::Tokenizer tokenizer;
    std::vector<lexer::Token> tokens;
    try {
        tokens = tokenizer.tokenize(source_code);
    } catch(const std::exception& error) {
        Logger::Error("[Tokenizer] {}", error.what());
        return;
    }

    parser::Parser parser{ tokens };
    parser::program_t program;
    try {
        program = parser.build_ast();
    } catch(const std::exception& error) {
        Logger::Error("[Parser | AST construction] {}", error.what());
        return;
    }

    for(auto& p : program) {
        parser::print_ast(p);
    }

    try {
        interpreter::Executor exec{ program };
    } catch(const std::exception& error) { Logger::Error("[Execution] {}", error.what()); }
}

std::string Application::read_source_code(const std::filesystem::path& source_code) {
    std::ifstream file{ source_code };
    if(!file.is_open()) { return std::string{}; }
    file.seekg(0, std::ios::end);
    const size_t size = file.tellg();
    std::string content(size, '\0');
    file.seekg(0);
    file.read(content.data(), content.size());
    return content;
}
