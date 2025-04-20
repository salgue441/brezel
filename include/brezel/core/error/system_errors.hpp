/**
 * @file system_errors.hpp
 * @author Carlos Salguero
 * @brief System-related error classes for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines error classes related to system operations
 * in the brezel framework, such as I/O, memory, and library errors
 */

#pragma once

#include <optional>
#include <source_location>
#include <string>
#include <string_view>
#include <system_error>

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>

#include <fmt/core.h>
#include <fmt/format.h>

namespace brezel::error {
/**
 * @brief System error wrapper
 */
class BREZEL_API SystemError : public Error {
public:
    /**
     * @brief Construct from system error code and message
     *
     * @param error_code System error code
     * @param message Additional context message
     * @param location Source location
     */
    SystemError(int error_code, std::string message,
                const std::source_location& location = std::source_location::current())
        : Error(ErrorContext(fmt::format("{}: {}", message, std::strerror(error_code)),
                             Code::SystemError, Category::System, error_code, std::nullopt,
                             location)) {}

    /**
     * @brief Construct from std::error_code and message
     *
     * @param error_code Standard error code
     * @param message Additional context message
     * @param location Source location
     */
    SystemError(const std::error_code& error_code, std::string message,
                const std::source_location& location = std::source_location::current())
        : Error(ErrorContext(fmt::format("{}: {}", message, error_code.message()),
                             Code::SystemError, Category::System, error_code.value(), std::nullopt,
                             location)) {}
};

/**
 * @brief Memory-related errors
 */
class BREZEL_API MemoryError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    MemoryError(std::string message, Code code = Code::MemoryAllocationFailed,
                const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit MemoryError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Out of memory error
 */
class BREZEL_API OutOfMemoryError : public MemoryError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit OutOfMemoryError(
        std::string message, const std::source_location& location = std::source_location::current())
        : MemoryError(std::move(message), Code::OutOfMemory, location) {}

    /**
     * @brief Construct with allocation size information
     *
     * @param requested_bytes Requested allocation size
     * @param location Source location
     */
    explicit OutOfMemoryError(size_t requested_bytes, const std::source_location& location =
                                                          std::source_location::current())
        : MemoryError(fmt::format("Out of memory: failed to allocate {} bytes", requested_bytes),
                      Code::OutOfMemory, location) {}
};

/**
 * @brief Memory allocation failed error
 */
class BREZEL_API MemoryAllocationFailedError : public MemoryError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit MemoryAllocationFailedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : MemoryError(std::move(message), Code::MemoryAllocationFailed, location) {}
};

/**
 * @brief Invalid memory access error
 */
class BREZEL_API InvalidMemoryAccessError : public MemoryError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvalidMemoryAccessError(
        std::string message, const std::source_location& location = std::source_location::current())
        : MemoryError(std::move(message), Code::InvalidMemoryAccess, location) {}
};

/**
 * @brief I/O error base class
 */
class BREZEL_API IOError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    IOError(std::string message, Code code = Code::FileIOError,
            const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit IOError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief File not found error
 */
class BREZEL_API FileNotFoundError : public IOError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit FileNotFoundError(
        std::string message, const std::source_location& location = std::source_location::current())
        : IOError(std::move(message), Code::FileNotFound, location) {}

    /**
     * @brief Construct with filename
     *
     * @param filename File path
     * @param location Source location
     */
    explicit FileNotFoundError(std::string_view filename, const std::source_location& location =
                                                              std::source_location::current())
        : IOError(fmt::format("File not found: {}", filename), Code::FileNotFound, location) {}
};

/**
 * @brief File I/O error
 */
class BREZEL_API FileIOError : public IOError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit FileIOError(std::string message,
                         const std::source_location& location = std::source_location::current())
        : IOError(std::move(message), Code::FileIOError, location) {}

    /**
     * @brief Construct with filename and operation
     *
     * @param filename File path
     * @param operation Operation description
     * @param location Source location
     */
    FileIOError(std::string_view filename, std::string_view operation,
                const std::source_location& location = std::source_location::current())
        : IOError(fmt::format("File I/O error during {} on file: {}", operation, filename),
                  Code::FileIOError, location) {}
};

/**
 * @brief Library error base class
 */
class BREZEL_API LibraryError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    LibraryError(std::string message, Code code = Code::LibraryError,
                 const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit LibraryError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Library not found error
 */
class BREZEL_API LibraryNotFoundError : public LibraryError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit LibraryNotFoundError(
        std::string message, const std::source_location& location = std::source_location::current())
        : LibraryError(std::move(message), Code::LibraryNotFound, location) {}

    /**
     * @brief Construct with library name
     *
     * @param library_name Library name
     * @param location Source location
     */
    explicit LibraryNotFoundError(
        std::string_view library_name,
        const std::source_location& location = std::source_location::current())
        : LibraryError(fmt::format("Library not found: {}", library_name), Code::LibraryNotFound,
                       location) {}
};

/**
 * @brief Symbol not found error
 */
class BREZEL_API SymbolNotFoundError : public LibraryError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit SymbolNotFoundError(
        std::string message, const std::source_location& location = std::source_location::current())
        : LibraryError(std::move(message), Code::SymbolNotFound, location) {}

    /**
     * @brief Construct with symbol and library names
     *
     * @param symbol_name Symbol name
     * @param library_name Library name
     * @param location Source location
     */
    SymbolNotFoundError(std::string_view symbol_name, std::string_view library_name,
                        const std::source_location& location = std::source_location::current())
        : LibraryError(fmt::format("Symbol not found: {} in library {}", symbol_name, library_name),
                       Code::SymbolNotFound, location) {}
};
}