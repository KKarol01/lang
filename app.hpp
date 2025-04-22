/**
 * @class Application
 * @brief Represents an application that works with source code from a file.
 */
class Application {
  public:
    /**
     * @brief Constructs an Application object.
     *
     * @param source_code The path to the source code file.
     */
    Application(const std::filesystem::path& source_code);

  private:
    static std::string read_source_code(const std::filesystem::path& source_code);

    std::string source_code;
};
