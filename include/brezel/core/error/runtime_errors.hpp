/**
 * @file runtime_errors.hpp
 * @author Carlos Salguero
 * @brief Runtime-related error classes for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines error classes related to runtime operations
 * in the brezel framework, such as invalid arguments, not implemented,
 * precondition failures, and more.
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
 * @brief Runtime errors
 */
class BREZEL_API RuntimeError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    RuntimeError(std::string message, Code code = Code::RuntimeError,
                 const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit RuntimeError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Not implemented error
 */
class BREZEL_API NotImplementedError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit NotImplementedError(
        std::string message = "Feature not implemented",
        const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::NotImplemented, location) {}

    /**
     * @brief Construct with feature name
     *
     * @param feature Feature name
     * @param location Source location
     */
    explicit NotImplementedError(std::string_view feature, const std::source_location& location =
                                                               std::source_location::current())
        : RuntimeError(fmt::format("Feature not implemented: {}", feature), Code::NotImplemented,
                       location) {}
};

/**
 * @brief Invalid argument error
 */
class BREZEL_API InvalidArgumentError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvalidArgumentError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::InvalidArgument, location) {}

    /**
     * @brief Construct with argument name and reason
     *
     * @param arg_name Argument name
     * @param reason Reason for invalidity
     * @param location Source location
     */
    InvalidArgumentError(std::string_view arg_name, std::string reason,
                         const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Invalid argument {}: {}", arg_name, reason),
                       Code::InvalidArgument, location) {}
};

/**
 * @brief Invalid operation error
 */
class BREZEL_API InvalidOperationError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvalidOperationError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::InvalidOperation, location) {}

    /**
     * @brief Construct with operation and reason
     *
     * @param operation Operation description
     * @param reason Reason for invalidity
     * @param location Source location
     */
    InvalidOperationError(std::string_view operation, std::string reason,
                          const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Invalid operation {}: {}", operation, reason),
                       Code::InvalidOperation, location) {}
};

/**
 * @brief Timeout error
 */
class BREZEL_API TimeoutError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit TimeoutError(std::string message,
                          const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::Timeout, location) {}

    /**
     * @brief Construct with operation and timeout
     *
     * @param operation Operation description
     * @param timeout_ms Timeout in milliseconds
     * @param location Source location
     */
    TimeoutError(std::string_view operation, double timeout_ms,
                 const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Operation '{}' timed out after {:.2f} ms", operation,
                                   timeout_ms),
                       Code::Timeout, location) {}
};

/**
 * @brief Cancelled operation error
 */
class BREZEL_API CancellationError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit CancellationError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::Cancelled, location) {}

    /**
     * @brief Construct with operation
     *
     * @param operation Operation description
     * @param location Source location
     */
    explicit CancellationError(std::string_view operation, const std::source_location& location =
                                                               std::source_location::current())
        : RuntimeError(fmt::format("Operation '{}' was cancelled", operation), Code::Cancelled,
                       location) {}
};

/**
 * @brief Assertion failed error
 */
class BREZEL_API AssertionFailedError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit AssertionFailedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::AssertionFailed, location) {}

    /**
     * @brief Construct with expression
     *
     * @param expression Expression that failed
     * @param location Source location
     */
    explicit AssertionFailedError(
        std::string_view expression,
        const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Assertion failed: {}", expression), Code::AssertionFailed,
                       location) {}
};

/**
 * @brief Precondition failed error
 */
class BREZEL_API PreconditionFailedError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit PreconditionFailedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::PreconditionFailed, location) {}

    /**
     * @brief Construct with condition
     *
     * @param condition Description of the failed condition
     * @param location Source location
     */
    explicit PreconditionFailedError(
        std::string_view condition,
        const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Precondition failed: {}", condition), Code::PreconditionFailed,
                       location) {}
};

/**
 * @brief Postcondition failed error
 */
class BREZEL_API PostconditionFailedError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit PostconditionFailedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::PostconditionFailed, location) {}

    /**
     * @brief Construct with condition
     *
     * @param condition Description of the failed condition
     * @param location Source location
     */
    explicit PostconditionFailedError(
        std::string_view condition,
        const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Postcondition failed: {}", condition),
                       Code::PostconditionFailed, location) {}
};

/**
 * @brief Invariant violated error
 */
class BREZEL_API InvariantViolatedError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvariantViolatedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::InvariantViolated, location) {}

    /**
     * @brief Construct with invariant description
     *
     * @param invariant Description of the violated invariant
     * @param location Source location
     */
    explicit InvariantViolatedError(
        std::string_view invariant,
        const std::source_location& location = std::source_location::current())
        : RuntimeError(fmt::format("Invariant violated: {}", invariant), Code::InvariantViolated,
                       location) {}
};

/**
 * @brief Internal error for framework bugs
 */
class BREZEL_API InternalError : public RuntimeError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InternalError(std::string message,
                           const std::source_location& location = std::source_location::current())
        : RuntimeError(std::move(message), Code::InternalError, location) {}
};

/**
 * @brief Math-related errors
 */
class BREZEL_API MathError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    MathError(std::string message, Code code = Code::InvalidValue,
              const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit MathError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Division by zero error
 */
class BREZEL_API DivideByZeroError : public MathError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DivideByZeroError(
        std::string message = "Division by zero",
        const std::source_location& location = std::source_location::current())
        : MathError(std::move(message), Code::DivideByZero, location) {}
};

/**
 * @brief Numerical overflow error
 */
class BREZEL_API OverflowError : public MathError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit OverflowError(std::string message,
                           const std::source_location& location = std::source_location::current())
        : MathError(std::move(message), Code::Overflow, location) {}
};

/**
 * @brief Numerical underflow error
 */
class BREZEL_API UnderflowError : public MathError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit UnderflowError(std::string message,
                            const std::source_location& location = std::source_location::current())
        : MathError(std::move(message), Code::Underflow, location) {}
};

/**
 * @brief Invalid numerical value error (NaN, Inf)
 */
class BREZEL_API InvalidValueError : public MathError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvalidValueError(
        std::string message, const std::source_location& location = std::source_location::current())
        : MathError(std::move(message), Code::InvalidValue, location) {}

    /**
     * @brief Construct with value description
     *
     * @param value_desc Value description
     * @param location Source location
     */
    explicit InvalidValueError(std::string_view value_desc, const std::source_location& location =
                                                                std::source_location::current())
        : MathError(fmt::format("Invalid numerical value: {}", value_desc), Code::InvalidValue,
                    location) {}
};
}