/**
 * @file result.hpp
 * @author Carlos Salguero
 * @brief Result monad for error handling in the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This defile defines the Result<T> monad which provides a robust
 * and functional way to handle errors without exceptions
 */

#pragma once

#include <exception>
#include <functional>
#include <memory>
#include <optional>
#include <source_location>
#include <string>
#include <type_traits>
#include <utility>

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>

namespace brezel {
namespace error {
/**
 * @brief Result type for operations that can fail
 *
 * Provides a monadic interface for error handling inspired by Rust's
 * Result<T, E> and Swift's Result<Success, Failure>. It encapsulates either
 * a success value or an error, allowing for safer error propagation without
 * exceptions.
 *
 * @tparam T Value type
 */
template <typename T>
class Result {
public:
    /**
     * @brief Default constructor - creates an error result
     */
    Result() : m_value(), m_has_value(false), m_error("Default-constructed Result has no value") {}

    /**
     * @brief Construct with value
     *
     * @param value Success value
     */
    Result(T value) : m_value(std::move(value)), m_has_value(true), m_error() {}

    /**
     * @brief Construct with error
     *
     * @param error Error object
     */
    Result(std::shared_ptr<Error> error)
        : m_value(), m_has_value(false), m_error(std::make_shared<Error>(std::move(error))) {}

    /**
     * @brief Construct with error message and code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    Result(std::string message, Code code = Code::Unknown,
           const std::source_location& loc = std::source_location::current())
        : m_value(), m_has_value(false),
          m_error(std::make_shared<Error>(std::move(message), code, loc)) {}

    /**
     * @brief Check if result contains value
     *
     * @return true if contains value
     */
    [[nodiscard]] bool has_value() const noexcept { return m_has_value; }

    /**
     * @brief Check if result contains error
     *
     * @return true if contains error
     */
    [[nodiscard]] bool has_error() const noexcept { return !m_has_value; }

    /**
     * @brief Boolean conversion (true if has value)
     */
    explicit operator bool() const noexcept { return m_has_value; }

    /**
     * @brief Get the value (throws if error)
     *
     * @return const T& Value reference
     * @throws Error contained error if result has error
     */
    [[nodiscard]] const T& value() const& {
        if (!m_has_value) {
            throw *m_error;
        }

        return m_value;
    }

    /**
     * @brief Get the value (throws if error)
     *
     * @return T&& Value rvalue reference
     * @throws Error contained error if result has error
     */
    [[nodiscard]] T&& value() && {
        if (!m_has_value) {
            throw *m_error;
        }

        return std::move(m_value);
    }

    /**
     * @brief Get the error (throws if no error)
     *
     * @return const Error& Error reference
     * @throws std::logic_error if result has value
     */
    [[nodiscard]] const Error& error() const {
        if (m_has_value) {
            throw std::logic_error("Result has value, not error");
        }

        return *m_error;
    }

    /**
     * @brief Get value or throw error
     *
     * @return T Value
     * @throws Error contained error if result has error
     */
    T unwrap() const& {
        if (!m_has_value) {
            throw *m_error;
        }

        return m_value;
    }

    /**
     * @brief Get value or throw error (move version)
     *
     * @return T Value
     * @throws Error contained error if result has error
     */
    T unwrap() && {
        if (!m_has_value) {
            throw *m_error;
        }

        return std::move(m_value);
    }

    /**
     * @brief Get value or return default
     *
     * @param default_value Default value
     * @return T Value or default
     */
    T m_valueor(T default_value) const& {
        if (!m_has_value) {
            return default_value;
        }

        return m_value;
    }

    /**
     * @brief Get value or return default (move version)
     *
     * @param default_value Default value
     * @return T Value or default
     */
    T m_valueor(T default_value) && {
        if (!m_has_value) {
            return default_value;
        }

        return std::move(m_value);
    }

    /**
     * @brief Get value or compute default
     *
     * @tparam F Function type
     * @param f Function that returns default value
     * @return T Value or computed default
     */
    template <typename F>
    T m_valueor_else(F&& f) const& {
        if (!m_has_value) {
            return f();
        }

        return m_value;
    }

    /**
     * @brief Get value or compute default (move version)
     *
     * @tparam F Function type
     * @param f Function that returns default value
     * @return T Value or computed default
     */
    template <typename F>
    T m_valueor_else(F&& f) && {
        if (!m_has_value) {
            return f();
        }

        return std::move(m_value);
    }

    /**
     * @brief Map result through a function
     *
     * @tparam F Function type
     * @param f Function to apply
     * @return Result<U> Mapped result
     */
    template <typename F>
    auto map(F&& f) const& -> Result<std::invoke_result_t<F, const T&>> {
        if (m_has_value) {
            return *m_error;
        }

        try {
            return f(m_value);
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Map result through a function (move version)
     *
     * @tparam F Function type
     * @param f Function to apply
     * @return Result<U> Mapped result
     */
    template <typename F>
    auto map(F&& f) && -> Result<std::invoke_result_t<F, T&&>> {
        if (!m_has_value) {
            return *m_error;
        }

        try {
            return f(std::move(m_value));
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Flat map result through a function
     *
     * @tparam F Function type
     * @param f Function that returns Result<U>
     * @return Result<U> Flat-mapped result
     */
    template <typename F>
    auto and_then(F&& f) const& -> std::invoke_result_t<F, const T&> {
        if (!m_has_value) {
            return *m_error;
        }

        try {
            return f(m_value);
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Flat map result through a function (move version)
     *
     * @tparam F Function type
     * @param f Function that returns Result<U>
     * @return Result<U> Flat-mapped result
     */
    template <typename F>
    auto and_then(F&& f) && -> std::invoke_result_t<F, T&&> {
        if (!m_has_value) {
            return *m_error;
        }

        try {
            return f(std::move(m_value));
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Map error through a function
     *
     * @tparam F Function type
     * @param f Function to apply to error
     * @return Result<T> Result with mapped error
     */
    template <typename F>
    Result<T> or_else(F&& f) const& {
        if (m_has_value) {
            return *this;
        }

        try {
            return f(*m_error);
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Map error through a function (move version)
     *
     * @tparam F Function type
     * @param f Function to apply to error
     * @return Result<T> Result with mapped error
     */
    template <typename F>
    Result<T> or_else(F&& f) && {
        if (m_has_value) {
            return std::move(*this);
        }

        try {
            return f(*m_error);
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

private:
    T m_value;
    bool m_has_value;
    std::shared_ptr<Error> m_error;
};

/**
 * @brief Create a success result
 *
 * @tparam T Value type
 * @param value Value
 * @return Result<T> Success result
 */
template <typename T>
Result<T> Ok(T&& value) {
    return Result<T>(std::forward<T>(value));
}

/**
 * @brief Create an error result
 *
 * @tparam T Value type
 * @tparam Args Constructor argument types
 * @param args Error constructor arguments
 * @return Result<T> Error result
 */
template <typename T, typename... Args>
Result<T> Err(Args&&... args) {
    return Result<T>(Error(std::forward<Args>(args)...));
}

/**
 * @brief Specialization of Result for void-returning operations
 */
template <>
class Result<void> {
public:
    /**
     * @brief Default constructor - creates success result
     */
    Result() : m_has_value(true), m_error() {}

    /**
     * @brief Construct with error
     *
     * @param error Error object
     */
    /* implicit */ Result(Error error)
        : m_has_value(false), m_error(std::make_shared<Error>(std::move(error))) {}

    /**
     * @brief Construct with error message and code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    Result(std::string message, Code code = Code::Unknown,
           const std::source_location& location = std::source_location::current())
        : m_has_value(false), m_error(std::make_shared<Error>(std::move(message), code, location)) {
    }

    /**
     * @brief Check if result is success
     *
     * @return true if success
     */
    [[nodiscard]] bool has_value() const noexcept { return m_has_value; }

    /**
     * @brief Check if result contains error
     *
     * @return true if contains error
     */
    [[nodiscard]] bool has_error() const noexcept { return !m_has_value; }

    /**
     * @brief Boolean conversion (true if success)
     */
    explicit operator bool() const noexcept { return m_has_value; }

    /**
     * @brief Get the error (throws if no error)
     *
     * @return const Error& Error reference
     * @throws std::logic_error if result is success
     */
    [[nodiscard]] const Error& error() const {
        if (m_has_value) {
            throw std::logic_error("Result is success, not error");
        }
        return *m_error;
    }

    /**
     * @brief Ensure success or throw error
     *
     * @throws Error contained error if result has error
     */
    void unwrap() const {
        if (!m_has_value) {
            throw *m_error;
        }
    }

    /**
     * @brief Map success result through a function
     *
     * @tparam F Function type
     * @param f Function to apply
     * @return Result<U> Mapped result
     */
    template <typename F>
    auto map(F&& f) const -> Result<std::invoke_result_t<F>> {
        if (!m_has_value) {
            return *m_error;
        }

        try {
            return Ok(f());
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Flat map success result through a function
     *
     * @tparam F Function type
     * @param f Function that returns Result<U>
     * @return Result<U> Flat-mapped result
     */
    template <typename F>
    auto and_then(F&& f) const -> std::invoke_result_t<F> {
        if (!m_has_value) {
            return *m_error;
        }

        try {
            return f();
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

    /**
     * @brief Map error through a function
     *
     * @tparam F Function type
     * @param f Function to apply to error
     * @return Result<void> Result with mapped error
     */
    template <typename F>
    Result<void> or_else(F&& f) const {
        if (m_has_value) {
            return *this;
        }

        try {
            return f(*m_error);
        } catch (const Error& e) {
            return e;
        } catch (const std::exception& e) {
            return Error(e.what());
        }
    }

private:
    bool m_has_value;
    std::shared_ptr<Error> m_error;
};

/**
 * @brief Create a success result for void operations
 *
 * @return Result<void> Success result
 */
inline Result<void> Ok() {
    return Result<void>();
}

/**
 * @brief Create an error result for void operations
 *
 * @tparam Args Constructor argument types
 * @param args Error constructor arguments
 * @return Result<void> Error result
 */
template <typename... Args>
Result<void> Err(Args&&... args) {
    return Result<void>(Error(std::forward<Args>(args)...));
}

/**
 * @brief Try to execute a function, capturing any exceptions into a Result
 *
 * @tparam F Function type
 * @tparam Args Argument types
 * @param f Function to call
 * @param args Arguments to pass
 * @return Result<ReturnType> Result containing the function result or an error
 */
template <typename F, typename... Args>
auto Try(F&& f, Args&&... args) -> Result<std::invoke_result_t<F, Args...>> {
    using ReturnType = std::invoke_result_t<F, Args...>;

    try {
        if constexpr (std::is_void_v<ReturnType>) {
            f(std::forward<Args>(args)...);
            return Ok();
        } else {
            return Ok(f(std::forward<Args>(args)...));
        }
    } catch (const Error& e) {
        return e;
    } catch (const std::exception& e) {
        return Error(e.what());
    } catch (...) {
        return Error("Unknown exception");
    }
}
}  // namespace error
}  // namespace brezel