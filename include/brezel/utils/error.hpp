/**
 * @file error.hpp
 * @author Carlos Salguero
 * @brief Error handling utilities for Brezel
 * @version 0.1
 * @date 2025-03-16
 *
 * @copyright Copyright (c) 2025
 *
 * This file provides error handling utilities for the Brezel tensor framework,
 * including error classes and the Expected type for Rust-like error handling
 */

#pragma once

#include <brezel/macros.hpp>

#include <exception>
#include <functional>
#include <memory>
#include <optional>
#include <source_location>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>

namespace brezel::utils {
/**
 * @brief Error source locatiom
 */
struct SourceLocation {
    std::string_view file;
    std::string_view function;
    int line;

    /**
     * @brief Retrieves the current source location.
     *
     * This function returns a SourceLocation object that contains information
     * about the current source location, including the file name, function
     * name, and line number.
     *
     * @param loc The source location to use. Defaults to the current
     *            source location.
     * @return A SourceLocation object containing the file name,
     *         function name, and line number.
     */
    static SourceLocation current(
        const std::source_location& loc = std::source_location::current()) {
        return {loc.file_name(), loc.function_name(), static_cast<int>(loc.line())};
    }
};

/**
 * @brief Error severity levels
 */
enum class ErrorSeverity { Info, Warning, Error, Fatal };

/**
 * @brief Converts a Severity to a string
 *
 * @param severity The severity
 * @return String representation of the severity
 */
constexpr std::string_view severity_to_string(ErrorSeverity severity) {
    switch (severity) {
        case ErrorSeverity::Info:
            return "INFO";

        case ErrorSeverity::Warning:
            return "WARNING";

        case ErrorSeverity::Error:
            return "ERROR";

        case ErrorSeverity::Fatal:
            return "FATAL";

        default:
            return "UNKNOWN";
    }
}

/**
 * @brief Bae class for all Brezel errors
 */
class BREZEL_API Error : public std::exception {
public:
    /**
     * @brief Creates a new error with a message
     *
     * @param message Error message
     * @param severity Error severity
     */
    explicit Error(std::string message, ErrorSeverity severity = ErrorSeverity::Error) noexcept
        : m_message(std::move(message)), m_severity(severity) {}

    /**
     * @brief Creates a new error message with source location
     *
     * @param message Error message
     * @param location Source location
     * @param severity Error severity
     */
    Error(std::string message, SourceLocation location,
          ErrorSeverity severity = ErrorSeverity::Error) noexcept
        : m_message(std::move(message)), m_location(location), m_severity(severity) {}

    /**
     * @brief Returns the error message
     * @return The error message
     */
    const std::string& message() const noexcept { return m_message; }

    /**
     * @brief Returns the error source location
     * @return The error source location
     */
    const std::optional<SourceLocation>& location() const noexcept { return m_location; }

    /**
     * @brief Returns the severity of the error
     * @return The error severity
     */
    ErrorSeverity severity() const noexcept { return m_severity; }

    /**
     * @brief Returns the error message (for std::exception compatibility)
     * @return The error meesage
     */
    const char* what() const noexcept override { return m_message.c_str(); }

    /**
     * @brief Returns a formatted string representation of the error
     * @return Formatted string representation
     */
    virtual std::string format() const {
        std::string result = "[" + std::string(severity_to_string(m_severity)) + "]" + m_message;

        if (m_location) {
            result += " (at " + std::string(m_location->file) + ":" +
                      std::to_string(m_location->line) + ")";
        }

        return result;
    }

private:
    std::string m_message;
    std::optional<SourceLocation> m_location;
    ErrorSeverity m_severity;
};

/**
 * @brief Creates an error with the current source location
 *
 * @param message Error message
 * @param severity Error severity
 * @return A new Error instance
 */
inline Error make_error(std::string message, ErrorSeverity severity = ErrorSeverity::Error) {
    return Error(std::move(message), SourceLocation::current(), severity);
}

// Specializations
/**
 * @brief Runtime error
 */
class RuntimeError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result =
            "[RUNTIME " + std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};

/**
 * @brief Invalid argument error
 */
class InvalidArgumentError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result = "[INVALID_ARGUMENT_ERROR " +
                             std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};

/**
 * @brief Shape error
 */
class ShapeError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result =
            "[SHAPE_ERROR " + std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};

/**
 * @brief Device error
 */
class DeviceError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result =
            "[DEVICE_ERROR " + std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};

/**
 * @brief Not implemented error
 */
class NotImplementedError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result = "[NOT_IMPLEMENTED_ERROR " +
                             std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};

/**
 * @brief Index error (for out-of-bounds access)
 */
class IndexError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result =
            "[INDEX_ERROR " + std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};

/**
 * @brief Memory error
 */
class MemoryError : public Error {
public:
    using Error::Error;

    std::string format() const override {
        std::string result =
            "[MEMORY_ERROR " + std::string(severity_to_string(severity())) + "]" + message();

        if (location()) {
            result += " ( at " + std::string(location()->file) + ":" +
                      std::to_string(location()->line) + ")";
        }

        return result;
    }
};
}  // namespace brezel::utils