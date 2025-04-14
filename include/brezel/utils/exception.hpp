#pragma once

#include <fmt/core.h>
#include <fmt/format.h>

#include <source_location>
#include <stdexcept>
#include <string>

namespace brezel::utils {
/**
 * @brief Base exception class for all brezel-specific exceptions
 *
 * This class extends std::runtime_error and adds additional
 * context like source location.
 */
class Exception : public std::runtime_error {
public:
    /**
     * @brief Construct a new Exception
     *
     * @param message Error message
     * @param location Source location where the exception occurred
     */
    explicit Exception(const std::string& message,
                       const std::source_location& loc = std::source_location::current())
        : std::runtime_error(format_message(message, loc)),
          m_message(message),
          m_file(loc.file_name()),
          m_line(loc.line()),
          m_function(loc.function_name()) {}

    /**
     * @brief Get the original message without location information
     *
     * @return const std::string& The original error message
     */
    const std::string& message() const { return m_message; }

    /**
     * @brief Get the file where the exception occurred
     *
     * @return const std::string& File name
     */
    const std::string& file() const { return m_file; }

    /**
     * @brief Get the line where the exception occurred
     *
     * @return int Line number
     */
    int line() const { return m_line; }

    /**
     * @brief Get the function where the exception occurred
     *
     * @return const std::string& Function name
     */
    const std::string& function() const { return m_function; }

private:
    /**
     * @brief Format a detailed error message including source location
     *
     * @param message User-provided error message
     * @param location Source location information
     * @return std::string Formatted error message
     */
    static std::string format_message(const std::string& message,
                                      const std::source_location& location) {
        return fmt::format("{} [int {} at {}:{}]", message, location.function_name(),
                           location.file_name(), location.line());
    }

    std::string m_message;
    std::string m_file;
    int m_line;
    std::string m_function;
};

/**
 * @brief Exception thrown for shape-related errors in tensors
 */
class ShapeError : public Exception {
public:
    explicit ShapeError(const std::string& message,
                        const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a ShapeError exception with a formatted error message
     * and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit ShapeError(fmt::format_string<Args...> fmt_string, Args&&... args,
                        const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};

/**
 * @brief Exception thrown for invalid device operations
 */
class DeviceError : public Exception {
public:
    explicit DeviceError(const std::string& message,
                         const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a DeviceError exception with a formatted error message
     * and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit DeviceError(fmt::format_string<Args...> fmt_string, Args&&... args,
                         const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};

/**
 * @brief Exception thrown for runtime errors during tensor operations
 */
class RuntimeError : public Exception {
public:
    explicit RuntimeError(const std::string& message,
                          const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a RuntimeError exception with a formatted error message
     * and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit RuntimeError(fmt::format_string<Args...> fmt_string, Args&&... args,
                          const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};

/**
 * @brief Exception thrown for invalid arguments in tensor operations
 */
class ValueError : public Exception {
public:
    explicit ValueError(const std::string& message,
                        const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a ValueError exception with a formatted error message
     * and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit ValueError(fmt::format_string<Args...> fmt_string, Args&&... args,
                        const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};

/**
 * @brief Exception thrown for out-of-bounds indexing
 */
class IndexError : public Exception {
public:
    explicit IndexError(const std::string& message,
                        const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a IndexError exception with a formatted error message
     * and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit IndexError(fmt::format_string<Args...> fmt_string, Args&&... args,
                        const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};

/**
 * @brief Exception thrown for memory-related errors
 */
class MemoryError : public Exception {
public:
    explicit MemoryError(const std::string& message,
                         const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a MemoryError exception with a formatted error message
     * and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit MemoryError(fmt::format_string<Args...> fmt_string, Args&&... args,
                         const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};

/**
 * @brief Exception thrown for not implemented features
 */
class NotImplementedError : public Exception {
public:
    explicit NotImplementedError(const std::string& message = "This feature is not implemented yet",
                                 const std::source_location& loc = std::source_location::current())
        : Exception(message, loc) {}

    /**
     * @brief Constructs a NotImplementedError exception with a
     * formatted error message and source location.
     *
     * @tparam Args Variadic template parameter pack for the types of
     *         arguments used in the format string.
     * @param fmt_string A format string compatible with the fmt library.
     * @param args Arguments to be formatted into the format string.
     * @param loc The source location where the exception is created.
     *            Defaults to the current source location.
     */
    template <typename... Args>
    explicit NotImplementedError(fmt::format_string<Args...> fmt_string, Args&&... args,
                                 const std::source_location& loc = std::source_location::current())
        : Exception(fmt::format(fmt_string, std::forward<Args>(args)...), loc) {}
};
}  // namespace brezel::utils