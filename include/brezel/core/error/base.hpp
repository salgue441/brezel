/**
 * @file base.hpp
 * @author Carlos Salguero
 * @brief Base error class for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines the base Error class that all specific error types in
 * the brezel framework inherit from.
 */

#pragma once

#include <exception>
#include <optional>
#include <source_location>
#include <string>
#include <string_view>

#include <brezel/core/config.hpp>
#include <brezel/core/error/context.hpp>

namespace brezel::error {
/**
 * @brief Base exception class for brezel errors
 */
class BREZEL_API Error : public std::exception {
public:
    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit Error(ErrorContext context)
        : m_context(std::move(context)), m_what_message(context.to_string()) {}

    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    Error(std::string message, Code code = Code::Unknown,
          const std::source_location& location = std::source_location::current())
        : m_context(std::move(message), code, location), m_what_message(m_context.to_string()) {}

    /**
     * @brief Get the error message
     *
     * @return const char* Error message
     */
    [[nodiscard]] const char* what() const noexcept override { return m_what_message.c_str(); }

    /**
     * @brief Get the error context
     *
     * @return const ErrorContext& Error context
     */
    [[nodiscard]] const ErrorContext& context() const noexcept { return m_context; }

    /**
     * @brief Get the error code
     *
     * @return Code Error code
     */
    [[nodiscard]] Code code() const noexcept { return m_context.code; }

    /**
     * @brief Get the error category
     *
     * @return Category Error category
     */
    [[nodiscard]] Category category() const noexcept { return m_context.category; }

    /**
     * @brief Get the error message
     *
     * @return const std::string& Error message
     */
    [[nodiscard]] const std::string& message() const noexcept { return m_context.message; }

    /**
     * @brief Get the source location
     *
     * @return const std::source_location& Source location
     */
    [[nodiscard]] const std::source_location& location() const noexcept {
        return m_context.location;
    }

    /**
     * @brief Get the system error code if available
     *
     * @return std::optional<int> System error code
     */
    [[nodiscard]] std::optional<int> system_error() const noexcept {
        return m_context.system_error;
    }

    /**
     * @brief Get the library error code if available
     *
     * @return std::optional<int> Library error code
     */
    [[nodiscard]] std::optional<int> library_error() const noexcept {
        return m_context.library_error;
    }

private:
    ErrorContext m_context;
    std::string m_what_message;
};
}  // namespace brezel::error