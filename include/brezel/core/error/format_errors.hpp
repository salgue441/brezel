/**
 * @file format_errors.hpp
 * @author Carlos Salguero
 * @brief Format-related error classes for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines error classes related to formatting, parsing,
 * serialization, and validation operations in the brezel framework.
 */

#pragma once

#include <source_location>
#include <string>
#include <string_view>

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>

#include <fmt/core.h>
#include <fmt/format.h>

namespace brezel::error {
/**
 * @brief Format/parse error base class
 */
class BREZEL_API FormatError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    FormatError(std::string message, Code code = Code::FormatError,
                const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit FormatError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Parse error for syntax or input errors
 */
class BREZEL_API ParseError : public FormatError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit ParseError(std::string message,
                        const std::source_location& location = std::source_location::current())
        : FormatError(std::move(message), Code::ParseError, location) {}

    /**
     * @brief Construct with detailed parsing information
     *
     * @param format_name Format name (e.g., "JSON", "CSV")
     * @param detail Error details
     * @param position Optional position information
     * @param location Source location
     */
    ParseError(std::string_view format_name, std::string detail, std::string_view position = "",
               const std::source_location& location = std::source_location::current())
        : FormatError(position.empty()
                          ? fmt::format("Parse error in {} format: {}", format_name, detail)
                          : fmt::format("Parse error in {} format at {}: {}", format_name, position,
                                        detail),
                      Code::ParseError, location) {}
};

/**
 * @brief Validation error for schema/constraint violations
 */
class BREZEL_API ValidationError : public FormatError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit ValidationError(std::string message,
                             const std::source_location& location = std::source_location::current())
        : FormatError(std::move(message), Code::ValidationError, location) {}

    /**
     * @brief Construct with detailed validation information
     *
     * @param field Field or property name
     * @param constraint Description of the constraint
     * @param actual_value Actual value found
     * @param location Source location
     */
    ValidationError(std::string_view field, std::string constraint, std::string_view actual_value,
                    const std::source_location& location = std::source_location::current())
        : FormatError(fmt::format("Validation error for field '{}': expected {}, got '{}'", field,
                                  constraint, actual_value),
                      Code::ValidationError, location) {}
};

/**
 * @brief Serialization error
 */
class BREZEL_API SerializationError : public FormatError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit SerializationError(
        std::string message, const std::source_location& location = std::source_location::current())
        : FormatError(std::move(message), Code::SerializationError, location) {}

    /**
     * @brief Construct with object type and reason
     *
     * @param type Object type
     * @param reason Serialization failure reason
     * @param location Source location
     */
    SerializationError(std::string_view type, std::string reason,
                       const std::source_location& location = std::source_location::current())
        : FormatError(fmt::format("Failed to serialize {} object: {}", type, reason),
                      Code::SerializationError, location) {}
};

/**
 * @brief Deserialization error
 */
class BREZEL_API DeserializationError : public FormatError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeserializationError(
        std::string message, const std::source_location& location = std::source_location::current())
        : FormatError(std::move(message), Code::DeserializationError, location) {}

    /**
     * @brief Construct with format, object type and reason
     *
     * @param format Format name (e.g., "JSON", "binary")
     * @param type Target object type
     * @param reason Deserialization failure reason
     * @param location Source location
     */
    DeserializationError(std::string_view format, std::string_view type, std::string reason,
                         const std::source_location& location = std::source_location::current())
        : FormatError(fmt::format("Failed to deserialize {} from {} format: {}", type, format,
                                  reason),
                      Code::DeserializationError, location) {}
};

/**
 * @brief Error for encoding issues (character encoding, etc.)
 */
class BREZEL_API EncodingError : public FormatError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit EncodingError(std::string message,
                           const std::source_location& location = std::source_location::current())
        : FormatError(std::move(message), Code::FormatError, location) {}

    /**
     * @brief Construct with encoding information
     *
     * @param source_encoding Source encoding name
     * @param target_encoding Target encoding name
     * @param reason Encoding error reason
     * @param location Source location
     */
    EncodingError(std::string_view source_encoding, std::string_view target_encoding,
                  std::string reason,
                  const std::source_location& location = std::source_location::current())
        : FormatError(fmt::format("Encoding error converting from {} to {}: {}", source_encoding,
                                  target_encoding, reason),
                      Code::FormatError, location) {}
};

/**
 * @brief Error for file format issues
 */
class BREZEL_API FileFormatError : public FormatError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit FileFormatError(std::string message,
                             const std::source_location& location = std::source_location::current())
        : FormatError(std::move(message), Code::InvalidFile, location) {}

    /**
     * @brief Construct with filename and format information
     *
     * @param filename File name or path
     * @param expected_format Expected format description
     * @param actual_format Actual format description (if known)
     * @param location Source location
     */
    FileFormatError(std::string_view filename, std::string_view expected_format,
                    std::string_view actual_format = "",
                    const std::source_location& location = std::source_location::current())
        : FormatError(actual_format.empty()
                          ? fmt::format("Invalid file format for '{}': expected {}", filename,
                                        expected_format)
                          : fmt::format("Invalid file format for '{}': expected {}, got {}",
                                        filename, expected_format, actual_format),
                      Code::InvalidFile, location) {}
};

/**
 * @brief Check if a string is valid JSON
 *
 * @param json JSON string to validate
 * @param error_message Optional error message to populate
 * @return bool True if valid JSON
 */
bool is_valid_json(std::string_view json, std::string* error_message = nullptr);

/**
 * @brief Check if a string is valid XML
 *
 * @param xml XML string to validate
 * @param error_message Optional error message to populate
 * @return bool True if valid XML
 */
bool is_valid_xml(std::string_view xml, std::string* error_message = nullptr);

/**
 * @brief Check if a string is valid CSV
 *
 * @param csv CSV string to validate
 * @param delimiter CSV delimiter (default: comma)
 * @param error_message Optional error message to populate
 * @return bool True if valid CSV
 */
bool is_valid_csv(std::string_view csv, char delimiter = ',', std::string* error_message = nullptr);

}