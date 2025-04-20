/**
 * @file context.hpp
 * @author Carlos Salguero
 * @brief Error context definitions for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines the ErrorContext struct which provides detailed
 * informatino about errors, including source location, message, error code,
 * and more.
 */

#pragma once

#include <optional>
#include <source_location>
#include <string>
#include <string_view>

#include <brezel/core/config.hpp>
#include <brezel/core/error/categories.hpp>
#include <brezel/core/error/codes.hpp>

#include <fmt/core.h>
#include <fmt/format.h>

namespace brezel::error {
/**
 * @brief Error context information
 *
 * Contains information about where and why an error occurred.
 */
struct ErrorContext {
    std::source_location location;     ///< Source location of the error
    std::string message;               ///< Error message
    Code code;                         ///< Error code
    Category category;                 ///< Error category
    std::optional<int> system_error;   ///< Optional system error code (e.g., errno)
    std::optional<int> library_error;  ///< Optional library-specific error code

    /**
     * @brief Constructor with message and source location
     *
     * @param msg Error message
     * @param err_code Error code
     * @param loc Source location
     */
    ErrorContext(std::string msg, Code err_code = Code::Unknown,
                 const std::source_location& loc = std::source_location::current())
        : location(loc), message(std::move(msg)), code(err_code),
          category(get_category_for_code(err_code)), system_error(std::nullopt),
          library_error(std::nullopt) {}

    /**
     * @brief Constructor with all fields
     *
     * @param msg Error message
     * @param err_code Error code
     * @param err_category Error category
     * @param sys_err System error code
     * @param lib_err Library error code
     * @param loc Source location
     */
    ErrorContext(std::string msg, Code err_code, Category err_category,
                 std::optional<int> sys_err = std::nullopt,
                 std::optional<int> lib_err = std::nullopt,
                 const std::source_location& loc = std::source_location::current())
        : location(loc), message(std::move(msg)), code(err_code), category(err_category),
          system_error(sys_err), library_error(lib_err) {}

    /**
     * @brief Create a formatted string representation
     *
     * @return std::string Formatted error context
     */
    [[nodiscard]] std::string to_string() const {
        std::string result = fmt::format("[{}/{}] {} (at {}:{}:{})", category_to_string(category),
                                         code_to_string(code), message, location.file_name(),
                                         location.line(), location.column());

        if (system_error) {
            result += fmt::format(" (system error: {})", *system_error);
        }

        if (library_error) {
            result += fmt::format(" (library error: {})", *library_error);
        }

        return result;
    }
};

}  // namespace brezel::error