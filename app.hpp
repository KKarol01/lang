#pragma once

#include <filesystem>

class Application {
  public:
    Application(const std::filesystem::path& source_code);

  private:
    static std::string read_source_code(const std::filesystem::path& source_code);

    std::string source_code;
};