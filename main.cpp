#include "app.hpp"
#include "logger.hpp"

int main(int argc, const char* const* argv) {
    if(argc > 1) {
        Application app{ argv[1] };
    } else {
        Logger::Warn("Please provide a valid path to a source code file like so: scriptlang path/to/file.lang");
    }
}