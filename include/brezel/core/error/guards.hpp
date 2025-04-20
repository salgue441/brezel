/**
 * @file guards.hpp
 * @author Carlos Salguero
 * @brief Error handling guards and utilities for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file provides RAII guards and other utilities for error handling
 * in the brezel framework
 */

#pragma once

#include <functional>
#include <source_location>
#include <string>

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>
#include <brezel/core/error/runtime_errors.hpp>
#include <brezel/core/error/tensor_errors.hpp>

#include <spdlog/spdlog.h>

namespace brezel {
namespace error {
/**
 * @brief RAII guard that logs and rethrows exceptions
 *
 * @tparam LoggerT Logger type
 */
template <typename LoggerT = spdlog::logger>
class ExceptionGuard {
public:
    /**
     * @brief Constructor with scope name and logger
     *
     * @param scope_name Name of the guarded scope
     * @param logger Logger to use
     */
    explicit ExceptionGuard(std::string scope_name, LoggerT& logger = *spdlog::default_logger())
        : m_scope_name(std::move(scope_name)), m_logger(logger) {}

    /**
     * @brief Destructor catches and logs exceptions
     */
    ~ExceptionGuard() noexcept(false) {
        if (std::uncaught_exceptions() > 0) {
            try {
                throw;
            } catch (const Error& e) {
                m_logger.error("Exception in {}: {}", m_scope_name, e.what());
                throw;
            } catch (const std::exception& e) {
                m_logger.error("Standard exception in {}: {}", m_scope_name, e.what());
                throw;
            } catch (...) {
                m_logger.error("Unknown exception in {}", m_scope_name);
                throw;
            }
        }
    }

private:
    std::string m_scope_name;
    LoggerT& m_logger;
};

/**
 * @brief RAII guard that ensures a cleanup action runs
 */
class CleanupGuard {
public:
    /**
     * @brief Constructor
     *
     * @param cleanup Action to perform on destruction
     */
    explicit CleanupGuard(std::function<void()> cleanup)
        : m_cleanup(std::move(cleanup)), m_enabled(true) {}

    /**
     * @brief Destructor runs the cleanup action
     */
    ~CleanupGuard() {
        if (m_enabled && m_cleanup) {
            m_cleanup();
        }
    }

    /**
     * @brief Disable the cleanup action
     */
    void disable() noexcept { m_enabled = false; }

    /**
     * @brief Enable the cleanup action
     */
    void enable() noexcept { m_enabled = true; }

    /**
     * @brief Release the guard without running the cleanup
     */
    void release() noexcept { disable(); }

private:
    std::function<void()> m_cleanup;
    bool m_enabled;

    // Non-copyable
    CleanupGuard(const CleanupGuard&) = delete;
    CleanupGuard& operator=(const CleanupGuard&) = delete;
};

/**
 * @brief Assert a condition or throw an error
 *
 * @param condition Condition to check
 * @param message Error message
 * @param location Source location
 * @throws AssertionFailedError if condition is false
 */
inline void Assert(bool condition, const std::string& message = "Assertion failed",
                   const std::source_location& location = std::source_location::current()) {
    if (!condition) {
        throw AssertionFailedError(message, location);
    }
}

/**
 * @brief Verify a precondition or throw an error
 *
 * @param condition Condition to check
 * @param message Error message
 * @param location Source location
 * @throws PreconditionFailedError if condition is false
 */
inline void Require(bool condition, const std::string& message,
                    const std::source_location& location = std::source_location::current()) {
    if (!condition) {
        throw PreconditionFailedError(message, location);
    }
}

/**
 * @brief Verify a postcondition or throw an error
 *
 * @param condition Condition to check
 * @param message Error message
 * @param location Source location
 * @throws PostconditionFailedError if condition is false
 */
inline void Ensure(bool condition, const std::string& message,
                   const std::source_location& location = std::source_location::current()) {
    if (!condition) {
        throw PostconditionFailedError(message, location);
    }
}

/**
 * @brief Throw if feature is not implemented
 *
 * @param feature Feature description
 * @param location Source location
 * @throws NotImplementedError
 */
[[noreturn]] inline void NotImplemented(
    const std::string& feature = "Feature",
    const std::source_location& location = std::source_location::current()) {
    throw NotImplementedError(feature, location);
}

/**
 * @brief Throw an invalid argument error
 *
 * @param argument Argument name
 * @param reason Reason for invalidity
 * @param location Source location
 * @throws InvalidArgumentError
 */
[[noreturn]] inline void InvalidArgument(
    const std::string& argument, const std::string& reason,
    const std::source_location& location = std::source_location::current()) {
    throw InvalidArgumentError(argument, reason, location);
}

/**
 * @brief Throw if an index is out of bounds
 *
 * @param index Actual index
 * @param bounds Maximum allowed value
 * @param dimension Optional dimension name
 * @param location Source location
 * @throws IndexOutOfBoundsError if index >= bounds
 */
inline void CheckBounds(size_t index, size_t bounds, const std::string& dimension = "",
                        const std::source_location& location = std::source_location::current()) {
    if (index >= bounds) {
        throw IndexOutOfBoundsError(index, bounds, dimension, location);
    }
}
}  // namespace error

}  // namespace brezel