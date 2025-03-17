#pragma once

#include <brezel/macros.hpp>

#include <cassert>
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>

namespace brezel::utils {
// Forward declaration
class Error;

/**
 * @brief Expected type for error handling (Rust-inspired)
 *
 * This class represents a value that can be either a successful result (T)
 * or an error (E). It provides a monadic interface for composing operations
 * that might fail, similar to Rust's Result type or C++23 std::expected.
 *
 * @tparam T Value type (cannot be void, use Expected<void, E> specialization instead)
 * @tparam E Error type (defaults to Error)
 */
template <typename T, typename E = Error>
class Expected {
    static_assert(!std::is_reference_v<T>, "Expected value type cannot be a reference");
    static_assert(!std::is_reference_v<E>, "Expected error type cannot be a reference");
    static_assert(!std::is_same_v<T, void>,
                  "Expected value type cannot be void, use Expected<void, E> instead");

    static_assert(std::is_copy_constructible_v<E> || std::is_move_constructible_v<E>,
                  "Error type must be copy or move constructible");

public:
    using value_type = T;
    using error_type = E;

    /**
     * @brief Default constructor is deleted
     */
    Expected() = delete;

    /**
     * @brief Construct an Expected with a value
     *
     * @param value The value
     */
    template <typename U, std::enable_if_t<std::is_constructible_v<T, U&&> &&
                                               !std::is_same_v<std::decay_t<U>, Expected<T, E>> &&
                                               !std::is_same_v<std::decay_t<U>, E>,
                                           int> = 0>
    constexpr Expected(U&& value) : m_variant(std::in_place_index<0>, std::forward<U>(value)) {}

    /**
     * @brief Constructs an Expected with an error
     *
     * @param error The error
     */
    template <typename G, std::enable_if_t<std::is_constructible_v<E, G&&> &&
                                               !std::is_same_v<std::decay_t<G>, Expected<T, E>>,
                                           int> = 0>
    constexpr Expected(G&& error) : m_variant(std::in_place_index<1>, std::forward<G>(error)) {}

    // Copy && Move operations
    constexpr Expected(const Expected&) = default;
    constexpr Expected(Expected&&) noexcept = default;
    constexpr Expected& operator=(const Expected&) = default;
    constexpr Expected& operator=(Expected&&) = default;

    // Destructor
    ~Expected() = default;

    // Methods
    /**
     * @brief Checks if the Expected contains a value
     *
     * @return true if it contains a value, false if it contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr bool has_value() const noexcept {
        return m_variant.index() == 0;
    }

    /**
     * @brief Alias for has_value()
     */
    BREZEL_ALWAYS_INLINE constexpr explicit operator bool() const noexcept { return has_value(); }

    /**
     * @brief Returns the contained value
     *
     * @return A const reference to the value
     * @throws std::bad_variant_access if it contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr const T& value() const& { return std::get<0>(m_variant); }

    /**
     * @brief Returns the contained value
     *
     * @return A reference to the value
     * @throws std::bad_variant_access if it contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr T& value() & { return std::get<0>(m_variant); }

    /**
     * @brief Returns the contained value
     *
     * @return An rvalue reference to the value
     * @throws std::bad_variant_access if it contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr T&& value() && { return std::move(std::get<0>(m_variant)); }

    /**
     * @brief Returns the contained value or a default
     *
     * @param default_value The default value to return if it contains an error
     * @return The value or default
     */
    template <typename U>
    BREZEL_ALWAYS_INLINE constexpr T value_or(U&& default_value) const& {
        return has_value() ? value() : static_cast<T>(std::forward<U>(default_value));
    }

    /**
     * @brief Returns the contained value or a default
     *
     * @param default_value The default value to return if it contains an error
     * @return The value or default
     */
    template <typename U>
    BREZEL_ALWAYS_INLINE constexpr T value_or(U&& default_value) && {
        return has_value() ? std::move(value()) : static_cast<T>(std::forward<U>(default_value));
    }

    /**
     * @brief Returns the contained error
     *
     * @return A const reference to the error
     * @throws std::bad_variant_access if contains a value
     */
    BREZEL_ALWAYS_INLINE constexpr const E& error() const& { return std::get<1>(m_variant); }

    /**
     * @brief Returns the contained error
     *
     * @return A reference to the error
     * @throws std::bad_variant_access if contains a value
     */
    BREZEL_ALWAYS_INLINE constexpr E& error() & { return std::get<1>(m_variant); }

    /**
     * @brief Returns the contained error
     *
     * @return An rvalue reference to the error
     * @throws std::bad_variant_access if contains a value
     */
    BREZEL_ALWAYS_INLINE constexpr E&& error() && { return std::move(std::get<1>(m_variant)); }

    /**
     * @brief Transforms the contained value with a function
     *
     * @param fn The function to apply to the value
     * @return A new Expected with the transformed value or the original error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map(F&& fn) const& {
        using ResultType = std::invoke_result_t<F, const T&>;
        using ReturnType = Expected<ResultType, E>;

        if (has_value()) {
            return ReturnType(std::invoke(std::forward<F>(fn), value()));
        } else {
            return ReturnType(error());
        }
    }

    /**
     * @brief Transforms the contained value with a function
     *
     * @param fn The function to apply to the value
     * @return A new Expected with the transformed value or the original error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map(F&& fn) && {
        using ResultType = std::invoke_result_t<F, T&&>;
        using ReturnType = Expected<ResultType, E>;

        if (has_value()) {
            return ReturnType(std::invoke(std::forward<F>(fn), std::move(value())));
        } else {
            return ReturnType(std::move(error()));
        }
    }

    /**
     * @brief Transforms the contained error with a function
     *
     * @param fn The function to apply to the error
     * @return A new Expected with the original value or the transformed error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map_error(F&& fn) const& {
        using ResultType = std::invoke_result_t<F, const E&>;
        using ReturnType = Expected<T, ResultType>;

        if (has_value()) {
            return ReturnType(value());
        } else {
            return ReturnType(std::invoke(std::forward<F>(fn), error()));
        }
    }

    /**
     * @brief Transforms the contained error with a function
     *
     * @param fn The function to apply to the error
     * @return A new Expected with the original value or the transformed error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map_error(F&& fn) && {
        using ResultType = std::invoke_result_t<F, E&&>;
        using ReturnType = Expected<T, ResultType>;

        if (has_value()) {
            return ReturnType(std::move(value()));
        } else {
            return ReturnType(std::invoke(std::forward<F>(fn), std::move(error())));
        }
    }

    /**
     * @brief Chains with another Expected-returning function
     *
     * @param fn The function to apply to the value
     * @return The result of the function or the original error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto and_then(F&& fn) const& {
        using ResultType = std::invoke_result_t<F, const T&>;
        static_assert(is_expected_v<ResultType>, "Function must return an Expected");

        using G = typename ResultType::error_type;
        static_assert(std::is_convertible_v<E, G>, "Error types must be convertible");

        if (has_value()) {
            return std::invoke(std::forward<F>(fn), value());
        } else {
            return ResultType(static_cast<G>(error()));
        }
    }

    /**
     * @brief Chains with another Expected-returning function
     *
     * @param fn The function to apply to the value
     * @return The result of the function or the original error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto and_then(F&& fn) && {
        using ResultType = std::invoke_result_t<F, T&&>;
        static_assert(is_expected_v<ResultType>, "Function must return an Expected");

        using G = typename ResultType::error_type;
        static_assert(std::is_convertible_v<E, G>, "Error types must be convertible");

        if (has_value()) {
            return std::invoke(std::forward<F>(fn), std::move(value()));
        } else {
            return ResultType(static_cast<G>(std::move(error())));
        }
    }

    /**
     * @brief Chains with another Expected-returning function when there's an
     * error
     *
     * @param fn The function to apply to the error
     * @return The original value or the result of the function
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto or_else(F&& fn) const& {
        using ResultType = std::invoke_result_t<F, const E&>;
        static_assert(is_expected_v<ResultType>, "Function must return an Expected");

        static_assert(std::is_same_v<typename ResultType::value_type, T>,
                      "Function must return an Expected with the same value type");

        if (has_value()) {
            return *this;
        } else {
            return std::invoke(std::forward<F>(fn), error());
        }
    }

    /**
     * @brief Chains with another Expected-returning function when there's an
     * error
     *
     * @param fn The function to apply to the error
     * @return The original value or the result of the function
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto or_else(F&& fn) && {
        using ResultType = std::invoke_result_t<F, E&&>;
        static_assert(is_expected_v<ResultType>, "Function must return an Expected");

        static_assert(std::is_same_v<typename ResultType::value_type, T>,
                      "Function must return an Expected with the same value type");

        if (has_value()) {
            return std::move(*this);
        } else {
            return std::invoke(std::forward<F>(fn), std::move(error()));
        }
    }

    /**
     * @brief Performs an action if this contains a value
     *
     * @param fn The function to apply to the value
     * @return A reference to this Expected
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE Expected& on_value(F&& fn) & {
        if (has_value()) {
            std::invoke(std::forward<F>(fn), value());
        }

        return *this;
    }

    /**
     * @brief Performs an action if this contains an error
     *
     * @param fn The function to apply to the error
     * @return A reference to this Expected
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE Expected& on_error(F&& fn) & {
        if (!has_value()) {
            std::invoke(std::forward<F>(fn), error());
        }
        return *this;
    }

    /**
     * @brief Unwraps the Expected, returning the value or calling a handler
     * function with the error
     *
     * @param handler The function to handle the error
     * @return The contained value
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE T unwrap_or_else(F&& handler) const& {
        if (has_value()) {
            return value();
        } else {
            return std::invoke(std::forward<F>(handler), error());
        }
    }

    /**
     * @brief Unwraps the Expected, returning the value or calling a handler
     * function with the error
     *
     * @param handler The function to handle the error
     * @return The contained value
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE T unwrap_or_else(F&& handler) && {
        if (has_value()) {
            return std::move(value());
        } else {
            return std::invoke(std::forward<F>(handler), std::move(error()));
        }
    }

    /**
     * @brief Unwraps the Expected, returning the value or throwing the error
     * if it's an exception
     *
     * @return The contained value
     * @throws E if the Expected contains an error and E is derived from std::exception
     */
    template <typename U = E>
    BREZEL_ALWAYS_INLINE T unwrap_or_throw() const& {
        static_assert(std::is_base_of_v<std::exception, U>,
                      "Error type must be derived from std::exception to use unwrap_or_throw");

        if (has_value()) {
            return value();
        } else {
            throw error();
        }
    }

    /**
     * @brief Unwraps the Expected, returning the value or throwing the error
     * if it's an exception
     *
     * @return The contained value
     * @throws E if the Expected contains an error and E is derived from std::exception
     */
    template <typename U = E>
    BREZEL_ALWAYS_INLINE T unwrap_or_throw() && {
        static_assert(std::is_base_of_v<std::exception, U>,
                      "Error type must be derived from std::exception to use unwrap_or_throw");

        if (has_value()) {
            return std::move(value());
        } else {
            throw std::move(error());
        }
    }

    /**
     * @brief Unwraps the Expected, asserting that it contains a value
     *
     * @return The contained value
     * @note Asserts in debug builds if the Expected contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr const T& unwrap() const& {
        BREZEL_ASSERT(has_value(), "Called unwrap() on an Expected containing an error");
        return value();
    }

    /**
     * @brief Unwraps the Expected, asserting that it contains a value
     *
     * @return The contained value
     * @note Asserts in debug builds if the Expected contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr T& unwrap() & {
        BREZEL_ASSERT(has_value(), "Called unwrap() on an Expected containing an error");
        return value();
    }

    /**
     * @brief Unwraps the Expected, asserting that it contains a value
     *
     * @return The contained value
     * @note Asserts in debug builds if the Expected contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr T&& unwrap() && {
        BREZEL_ASSERT(has_value(), "Called unwrap() on an Expected containing an error");
        return std::move(value());
    }

    // Operators
    friend constexpr bool operator==(const Expected& lhs, const Expected& rhs) {
        if (lhs.has_value() != rhs.has_value()) {
            return false;
        }

        if (lhs.has_value()) {
            return lhs.value() == rhs.value();
        } else {
            return lhs.error() == rhs.error();
        }
    }

    friend constexpr bool operator!=(const Expected& lhs, const Expected& rhs) {
        return !(lhs == rhs);
    }

private:
    template <typename U>
    struct is_expected : std::false_type {};

    template <typename V, typename G>
    struct is_expected<Expected<V, G>> : std::true_type {};

    template <typename U>
    static constexpr bool is_expected_v = is_expected<std::decay_t<U>>::value;

    // Internal storage using std::variant
    std::variant<T, E> m_variant;
};

/**
 * @brief Specialization of Expected for void value type
 *
 * @tparam E Error type
 */
template <typename E>
class Expected<void, E> {
    static_assert(!std::is_reference_v<E>, "Expected error type cannot be a reference");

    static_assert(std::is_copy_constructible_v<E> || std::is_move_constructible_v<E>,
                  "Error type must be a copy or move constructible");

public:
    using value_type = void;
    using error_type = E;

    /**
     * @brief Constructs an Expected with a (void) value
     */
    constexpr Expected() noexcept : m_has_error(false) {}

    /**
     * @brief Constructs an Expected with an error
     *
     * @param error The error to store
     */
    template <typename G, std::enable_if_t<std::is_constructible_v<E, G&&> &&
                                               !std::is_same_v<std::decay_t<G>, Expected<void, E>>,
                                           int> = 0>
    constexpr Expected(G&& error) : m_error(std::forward<G>(error)), m_has_error(true) {}

    // Copy & Move operations
    constexpr Expected(const Expected&) = default;
    constexpr Expected(Expected&&) = default;
    constexpr Expected& operator=(const Expected&) = default;
    constexpr Expected& operator=(Expected&&) = default;

    // Destructor
    ~Expected() = default;

    /**
     * @brief Checks if the Expected contains a value
     *
     * @return true if it contains a value, false if it contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr bool has_value() const noexcept { return !m_has_error; }

    /**
     * @brief Alias for has_value()
     */
    BREZEL_ALWAYS_INLINE constexpr explicit operator bool() const noexcept { return has_value(); }

    /**
     * @brief Returns the contained error
     *
     * @return A const reference to the error
     * @throws std::logic_error if it contains a value
     */
    BREZEL_ALWAYS_INLINE constexpr const E& error() const& {
        if (!m_has_error) {
            throw std::logic_error("Expected does not contain an error");
        }

        return *m_error;
    }

    /**
     * @brief Returns the contained error
     *
     * @return A reference to the error
     * @throws std::logic_error if it contains a value
     */
    BREZEL_ALWAYS_INLINE constexpr E& error() & {
        if (!m_has_error) {
            throw std::logic_error("Expected does not contain an error");
        }

        return *m_error;
    }

    /**
     * @brief Returns the contained error
     *
     * @return An rvalue reference to the error
     * @throws std::logic_error If it contains a value
     */
    BREZEL_ALWAYS_INLINE constexpr E&& error() && {
        if (!m_has_error) {
            throw std::logic_error("Expected does not contain an error");
        }

        return std::move(*m_error);
    }

    /**
     * @brief Transforms this Expected with a function
     * @param fn The function to apply
     * @return A new Expected with the transformed value or the original error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map(F&& fn) const {
        using ResultType = std::invoke_result_t<F>;
        using ReturnType = Expected<ResultType, E>;

        if (has_value()) {
            return ReturnType(std::invoke(std::forward<F>(fn)));
        } else {
            return ReturnType(error());
        }
    }

    /**
     * @brief Maps the contained error with a function
     * @param fn The function to apply to the error
     * @return A new Expected with a void value or the mapped error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map_error(F&& fn) const& {
        using ResultType = std::invoke_result_t<F, const E&>;
        using ReturnType = Expected<void, ResultType>;

        if (has_value()) {
            return ReturnType();
        } else {
            return ReturnType(std::invoke(std::forward<F>(fn), error()));
        }
    }

    /**
     * @brief Maps the contained error with a function
     * @param fn The function to apply to the error
     * @return A new Expected with a void value or the mapped error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto map_error(F&& fn) && {
        using ResultType = std::invoke_result_t<F, E&&>;
        using ReturnType = Expected<void, ResultType>;

        if (has_value()) {
            return ReturnType();
        } else {
            return ReturnType(std::invoke(std::forward<F>(fn), std::move(error())));
        }
    }

    /**
     * @brief Chains with another Expected-returning function
     * @param fn The function to apply
     * @return The result of the function or the original error
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto and_then(F&& fn) const {
        using ResultType = std::invoke_result_t<F>;

        static_assert(is_expected_v<ResultType>, "Function must return an Expected");
        using G = typename ResultType::error_type;
        static_assert(std::is_convertible_v<E, G>, "Error types must be convertible");

        if (has_value()) {
            return std::invoke(std::forward<F>(fn));
        } else {
            return ResultType(static_cast<G>(error()));
        }
    }

    /**
     * @brief Chains with another Expected-returning function when there's an error
     * @param fn The function to apply to the error
     * @return The original value or the result of the function
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto or_else(F&& fn) const& {
        using ResultType = std::invoke_result_t<F, const E&>;

        static_assert(is_expected_v<ResultType>, "Function must return an Expected");
        static_assert(std::is_same_v<typename ResultType::value_type, void>,
                      "Function must return an Expected<void>");

        if (has_value()) {
            return *this;
        } else {
            return std::invoke(std::forward<F>(fn), error());
        }
    }

    /**
     * @brief Chains with another Expected-returning function when there's an error
     * @param fn The function to apply to the error
     * @return The original value or the result of the function
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE constexpr auto or_else(F&& fn) && {
        using ResultType = std::invoke_result_t<F, E&&>;

        static_assert(is_expected_v<ResultType>, "Function must return an Expected");
        static_assert(std::is_same_v<typename ResultType::value_type, void>,
                      "Function must return an Expected<void>");

        if (has_value()) {
            return std::move(*this);
        } else {
            return std::invoke(std::forward<F>(fn), std::move(error()));
        }
    }

    /**
     * @brief Performs an action if this contains a value
     * @param fn The function to apply
     * @return A reference to this Expected
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE Expected& on_value(F&& fn) & {
        if (has_value()) {
            std::invoke(std::forward<F>(fn));
        }

        return *this;
    }

    /**
     * @brief Performs an action if this contains an error
     * @param fn The function to apply to the error
     * @return A reference to this Expected
     */
    template <typename F>
    BREZEL_ALWAYS_INLINE Expected& on_error(F&& fn) & {
        if (!has_value()) {
            std::invoke(std::forward<F>(fn), error());
        }

        return *this;
    }

    /**
     * @brief Unwraps the Expected, throwing the error if it's an exception
     * @throws E if the Expected contains an error and E is derived
     * from std::exception
     */
    template <typename U = E>
    BREZEL_ALWAYS_INLINE void unwrap_or_throw() const& {
        static_assert(std::is_base_of_v<std::exception, U>,
                      "Error type must be derived from std::exception to use unwrap_or_throw");

        if (!has_value()) {
            throw error();
        }
    }

    /**
     * @brief Unwraps the Expected, throwing the error if it's an exception
     * @throws E if the Expected contains an error and E is derived
     * from std::exception
     */
    template <typename U = E>
    BREZEL_ALWAYS_INLINE void unwrap_or_throw() && {
        static_assert(std::is_base_of_v<std::exception, U>,
                      "Error type must be derived from std::exception to use unwrap_or_throw");

        if (!has_value()) {
            throw std::move(error());
        }
    }

    /**
     * @brief Unwraps the Expected, asserting that it contains a value
     * @note Asserts in debug builds if the Expected contains an error
     */
    BREZEL_ALWAYS_INLINE constexpr void unwrap() const {
        BREZEL_ASSERT(has_value(), "Called unwrap() on an Expected<void> containing an error");
    }

    // Operators
    friend constexpr bool operator==(const Expected& lhs, const Expected& rhs) {
        if (lhs.has_value() != rhs.has_value()) {
            return false;
        }

        if (lhs.has_value()) {
            return true;
        } else {
            return lhs.error() == rhs.error();
        }
    }

    friend constexpr bool operator!=(const Expected& lhs, const Expected& rhs) {
        return !(lhs == rhs);
    }

private:
    template <typename U>
    struct is_expected : std::false_type {};

    template <typename V, typename G>
    struct is_expected<Expected<V, G>> : std::true_type {};

    template <typename U>
    static constexpr bool is_expected_v = is_expected<std::decay_t<U>>::value;

    std::optional<E> m_error;
    bool m_has_error;
};

/**
 * @brief Creates an Expected with a value
 *
 * @tparam T The value type
 * @tparam E The error type
 * @param value The value to store
 * @return An Expected with the value
 */
template <typename T, typename E = Error>
BREZEL_ALWAYS_INLINE constexpr Expected<std::decay_t<T>, E> make_ok(T&& value) {
    return Expected<std::decay_t<T>, E>(std::forward<T>(value));
}

/**
 * @brief Creates an Expected with an error
 *
 * @tparam E The error type
 * @tparam T The value type
 * @param error The error to store
 * @return An Expected with the error
 */
template <typename E = Error, typename T>
BREZEL_ALWAYS_INLINE constexpr Expected<T, std::decay_t<E>> make_err(E&& error) {
    return Expected<T, std::decay_t<E>>(std::forward<E>(error));
}

/**
 * @brief Creates an Expected<void> with a value
 *
 * @tparam E The error type
 * @return An Expected<void> with a value
 */
template <typename E = Error>
BREZEL_ALWAYS_INLINE constexpr Expected<void, E> make_ok() {
    return Expected<void, E>();
}

/**
 * @brief Creates an Expected<void> with an error
 *
 * @tparam E The error type
 * @param error The error to store
 * @return An Expected<void> with an error
 */
template <typename E = Error>
BREZEL_ALWAYS_INLINE constexpr Expected<void, std::decay_t<E>> make_err(E&& error) {
    return Expected<void, std::decay_t<E>>(std::forward<E>(error));
}
}  // namespace brezel::utils