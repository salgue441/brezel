/**
 * @file exception.hpp
 * @author Carlos Salguero
 * @brief Defines exception classes for the brezel library
 * @version 0.1
 * @date 2025-04-27
 * 
 * @copyright Copyright (c) 2025
 * 
 * This file contains custom exception classes for various
 * error conditions that can occur in the brezel library.
 */

#pragma once

#include <source_location>
#include <sstream>
#include <stdexcept>
#include <string>

#include <fmt/format.h>

namespace brezel {

/**
 * @brief Base class for all brezel exceptions
 */
class BrezelError : public std::runtime_error {
public:
    /**
     * @brief Construct a new Brezel Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit BrezelError(const std::string& message,
                         const std::source_location& location = std::source_location::current())
        : std::runtime_error(format_message(message, location)) {}

private:
    /**
     * @brief Format the error message with source location information
     *
     * @param message Error message
     * @param location Source location
     * @return std::string Formatted message
     */
    static std::string format_message(const std::string& message,
                                      const std::source_location& location) {
        return fmt::format("{}:{} in {}: {}", location.file_name(), location.line(),
                           location.function_name(), message);
    }
};

/**
 * @brief Exception thrown when a type error occurs
 */
class TypeError : public BrezelError {
public:
    /**
     * @brief Construct a new Type Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit TypeError(const std::string& message,
                       const std::source_location& location = std::source_location::current())
        : BrezelError("TypeError: " + message, location) {}
};

/**
 * @brief Exception thrown when dimensions do not match
 */
class ShapeError : public BrezelError {
public:
    /**
     * @brief Construct a new Shape Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit ShapeError(const std::string& message,
                        const std::source_location& location = std::source_location::current())
        : BrezelError("ShapeError: " + message, location) {}
};

/**
 * @brief Exception thrown when an index is out of bounds
 */
class IndexError : public BrezelError {
public:
    /**
     * @brief Construct a new Index Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit IndexError(const std::string& message,
                        const std::source_location& location = std::source_location::current())
        : BrezelError("IndexError: " + message, location) {}
};

/**
 * @brief Exception thrown when a value is invalid
 */
class ValueError : public BrezelError {
public:
    /**
     * @brief Construct a new Value Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit ValueError(const std::string& message,
                        const std::source_location& location = std::source_location::current())
        : BrezelError("ValueError: " + message, location) {}
};

/**
 * @brief Exception thrown when a device operation fails
 */
class DeviceError : public BrezelError {
public:
    /**
     * @brief Construct a new Device Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit DeviceError(const std::string& message,
                         const std::source_location& location = std::source_location::current())
        : BrezelError("DeviceError: " + message, location) {}
};

/**
 * @brief Exception thrown when a memory operation fails
 */
class MemoryError : public BrezelError {
public:
    /**
     * @brief Construct a new Memory Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit MemoryError(const std::string& message,
                         const std::source_location& location = std::source_location::current())
        : BrezelError("MemoryError: " + message, location) {}
};

/**
 * @brief Exception thrown when an operation is not implemented
 */
class NotImplementedError : public BrezelError {
public:
    /**
     * @brief Construct a new Not Implemented Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit NotImplementedError(const std::string& message, const std::source_location& location =
                                                                 std::source_location::current())
        : BrezelError("NotImplementedError: " + message, location) {}
};

/**
 * @brief Exception thrown when a runtime error occurs
 */
class RuntimeError : public BrezelError {
public:
    /**
     * @brief Construct a new Runtime Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit RuntimeError(const std::string& message,
                          const std::source_location& location = std::source_location::current())
        : BrezelError("RuntimeError: " + message, location) {}
};

/**
 * @brief Exception thrown when a type conversion fails
 */
class TypeConversionError : public TypeError {
public:
    /**
     * @brief Construct a new Type Conversion Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit TypeConversionError(const std::string& message, const std::source_location& location =
                                                                 std::source_location::current())
        : TypeError("Type conversion error: " + message, location) {}
};

/**
 * @brief Exception thrown when broadcasting fails
 */
class BroadcastError : public ShapeError {
public:
    /**
     * @brief Construct a new Broadcast Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit BroadcastError(const std::string& message,
                            const std::source_location& location = std::source_location::current())
        : ShapeError("Broadcasting error: " + message, location) {}
};

/**
 * @brief Exception thrown when CUDA operations fail
 */
class CUDAError : public DeviceError {
public:
    /**
     * @brief Construct a new CUDA Error
     *
     * @param message Error message
     * @param location Source location where the error occurred
     */
    explicit CUDAError(const std::string& message,
                       const std::source_location& location = std::source_location::current())
        : DeviceError("CUDA error: " + message, location) {}
};

}  // namespace brezel