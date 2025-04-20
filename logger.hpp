#pragma once

#include <print>
#include <string_view>

class Logger {
  public:
    template <typename... Args> static void DebugLog(std::string_view msg, const Args&... args) {
#ifndef NDEBUG
        std::println("{}", format_msg("[DEBUG] {}", msg, args...));
#endif
    }

    template <typename... Args> static void DebugWarn(std::string_view msg, const Args&... args) {
#ifndef NDEBUG
        std::println("{}", format_msg("[WARN] {}", msg, args...));
#endif
    }

    template <typename... Args> static void Warn(std::string_view msg, const Args&... args) {
        std::println("{}", format_msg("[WARN] {}", msg, args...));
    }

    template <typename... Args> static void Error(std::string_view msg, const Args&... args) {
        std::println("{}", format_msg("[Error] {}", msg, args...));
    }

  private:
    template <typename... Args>
    static std::string format_msg(std::string_view prefix, std::string_view msg, const Args&... args) {
        auto str = std::vformat(msg, std::make_format_args(args...));
        return std::vformat(prefix, std::make_format_args(str));
    }
};