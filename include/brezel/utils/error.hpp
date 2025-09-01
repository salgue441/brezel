/**
 * @file error.hpp
 * @author Carlos Salguero
 * @brief Error handling definitions
 * @version 0.1
 * @date 2025-09-01
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>

namespace brezel::error {
/**
 * @brief Error codes for tensor operations
 */
enum class ErrorCode : uint32_t {
    Success = 0,
    OutOfMemory = 1000,
    InvalidAlignment,
    MemoryCorruption,
    ShapeMismatch = 2000,
    InvalidShape,
    DimensionMismatch,
    InvalidIndex,
    TypeMismatch = 3000,
    UnsupportedType,
    InvalidCast,
    InvalidOperation = 4000,
    UnsupportedOperation,
    DivisionByZero,
    MatrixNotSquare,
    SingularMatrix,
    FileNotFound = 5000,
    InvalidFormat,
    ReadError,
    WriteError,
    Unknown = 9999
};

/**
 * @brief A lightweight, value-based error type containing an error and an
 * optional message.
 *
 * This class is designed to be used as the default error type for the
 * `Result` class template. It provides a simple, efficient way to represent
 * errors with canonical code and additional context via message.
 *
 * It is copyable, comparable, and has a minimal footprint.
 *
 * @note The message is stored as `std::string_view`. The user is responsible
 * for ensuring the underlying character data for the message remains valid for
 * the lifetime of the `Error` object. For dynamic messages, use a `std::string`
 * and pass its `c_str()` or `.data()` to the constructor, ensuring the string
 * outlives the `Error` view.
 *
 * @invariant The `code` member always contains a valid `ErrorCode`.
 * @invariant The `message` member is always a valid `std::string_view`. If no
 *            message was provided, it is an empty `std::string_view`.
 */
class Error {
  public:
    /**
     * @brief Constructs an `Error` with `ErrorCode::Unknown` and an empty
     * message.
     *
     * @post `code() == ErrorCode::Unknown`
     * @post `message().empty() == true`
     */
    constexpr Error() noexcept = default;

    /**
     * @brief Constructs an `Error` with the specified code an an empty
     * message.
     *
     * @param code The error code to assign to this error.
     *
     * @post `code() == code`
     * @post `message().empty() == true`
     */
    constexpr explicit Error(ErrorCode code) noexcept : m_code(code) {}

    /**
     * @brief Constructs an `Error` with the specified code and message.
     *
     * @param code The error code to assign to this error.
     * @param message A view of the error message. This referenced data is *not*
     *                copied; the `Error` object will merely store a view. The
     *                caller must ensure the data pointed to by `message`
     *                remains valid.
     *
     * @post `code() == code`
     * @post `message() == message`
     */
    constexpr Error(ErrorCode code, std::string_view message) noexcept : m_code(code), m_message(message) {}

    // Group: Accessors
    /// @{

    /**
     * @brief Returns the error code associated with this error.
     *
     * @return constexpr The `ErrorCode` representing the specific error
     *         condition.
     */
    constexpr ErrorCode code() const noexcept { return m_code; }

    /**
     * @brief Returns a view of the error message associated with this error.
     *
     * @return constexpr A `std::string_view` containing the error message.
     *         If no message was provided, the view is empty.
     */
    constexpr std::string_view message() const noexcept { return m_message; }
    /// @}

    // Group: Comparison Operators
    /// @{

    /**
     * @brief Checks if two `Error` objects represent the same error condition.
     *
     * Equality is based solely on the error code (`ErrorCode`). The message is
     * ignored for comparison purposes.
     *
     * @param other The `Error` object to compare with.
     * @return `true` if this `this->code() == other.code()`, `false` otherwise
     */
    constexpr bool operator==(const Error& other) const noexcept { return m_code == other.m_code; }

    /**
     * @brief Checks if two `Error` objects represents different error
     * conditions.
     *
     * Inequality is based solely on the error code (`ErrorCode`). The message
     * is ignored for comparison purposes.
     *
     * @param other The `Error` object to compare with.
     * @return `true` if `this->code() != other.code()`, `false` otherwise
     */
    constexpr bool operator!=(const Error& other) const noexcept { return !(*this == other); }
    /// @}

    /**
     * @brief Creates a formatted string representation of the error.
     *
     * The format is: `"Error(<code_number>[, <message>])"`
     * For example: `"Error(5, File not found)"`
     *
     * @return A `std::string` containing the formatted representation of the
     *         error.
     *
     * @throws May throw `std::bad_alloc` if memory allocation for the string
     *         fails.
     */
    std::string to_string() const {
        std::string result = "Error(";
        result += std::to_string(static_cast<uint32_t>(m_code));

        if (!m_message.empty()) {
            result += ", ";
            result += m_message;
        }

        result += ")";
        return result;
    }

  private:
    ErrorCode m_code = ErrorCode::Unknown;
    std::string_view m_message;
};

/**
 * @brief A type that represents either a success value or an error.
 *
 * This class template is a direct analog to Rust's Result<T, E> type. It is
 * designed for explicit, type-safe error handling without the use of
 * exceptions.
 *
 * An object of type Result<T, E> always contains either a value of type `T`
 * (indicating success) or a value of type `E` (indicating an error). The user
 * must check the state of the object before accessing the contained value or
 * an error.
 *
 * @tparam T The type of the value contained in a successful result.
 *         Must be nonthrow-destructible.
 * @tparam E The type of error contained in a failed result. Defaults to
 *         `Error`. Must be nonthrow-destructible.
 * @invariant The internal `std::variant` always contains exactly one of `T`
 *            or `E`.
 *
 * @note This class does not use empty state (valueless_by_exception) for
 *       the variant. The destructors of `T` or `E` must not throw.
 */
template <typename T, typename E>
class Result {
  public:
    using value_type = T;
    using m_errortype = E;

  public:
    /**
     * @brief Constructs a successful `Result` containing a value.
     *
     * @param value The success value to be contained
     */
    constexpr Result(const T& value) noexcept(std::is_nothrow_copy_constructible_v<T>) : m_storage(value) {}

    /**
     * @brief Constructs a successful `Result` by moving a value.
     *
     * @param value The success value to be moved in.
     */
    constexpr Result(T&& value) noexcept(std::is_nothrow_copy_constructible_v<T>) : m_storage(std::move(vaue)) {}

    /**
     * @brief Constructs a failed `Result` containing an error.
     *
     * @param error The error value to be contained.
     */
    constexpr Result(const E& error) noexcept(std::is_nothrow_copy_constructible_v<E>) : m_storage(error) {}

    /**
     * @brief Constructs a failed `Result` by moving an error.
     *
     * @param error The error value to be contained.
     */
    constexpr Result(E&& error) noexcept(std::is_nothrow_copy_constructible_v<E>) : m_storage(std::move(error)) {}

    // Copy/move constructors
    constexpr Result(const Result&) = default;
    constexpr Result(Result&&) noexcept = default;
    constexpr Result& operator=(const Result&) = default;
    constexpr Result& operator=(Result&&) noexcept = default;

    /**
     * @brief Checks if the `Result` contains a success value.
     *
     * @return `true` if `*this` contains a value of type `T`, `false` otherwise
     */
    constexpr bool is_ok() const noexcept { return std::holds_alternative<T>(m_storage); }

    /**
     * @brief Checks if the `Result` contains an error.
     *
     * @return `true` if `*this` contains a value of type `E`, `false` otehrwise
     */
    constexpr bool is_err() const noexcept { return std::holds_alternative<E>(m_storage); }

    /**
     * @brief Contextually converts to `true` if the `Result` contains a
     * success value.
     *
     * Allows for usage in boolean contexts like `if (result) { ... }`.
     *
     * @return `is_ok()`
     */
    constexpr explicit operator bool() const noexcept { return is_ok(); }

    // Group: Value accessors
    /// @{

    /**
     * @brief Returns a const lvalue reference to the contained success value.
     *
     * @return a const reference to the contained value of type `T`.
     * @pre `is_ok() == true`. The behavior is undefined if called on a `Result`
     *      containing an error.
     */
    constexpr const T& value() const& noexcept { return std::get<T>(m_storage); }

    /**
     * @brief Returns an lvalue reference to the contained success value.
     *
     * @return a reference to the contained value of type `T`.
     * @pre `is_ok() == true`. The behavior is undefined if called on a `Result`
     *      containing an error.
     */
    constexpr T& value() & noexcept { return std::get<T>(m_storage); }

    /**
     * @brief Returns a const rvalue reference to the contained success value.
     *
     * @return a const reference to the contained value of type `T`.
     * @pre `is_ok() == true`. The behavior is undefined if called on a `Result`
     *      containing an error.
     */
    constexpr const T&& value() const&& noexcept { return std::get<T>(std::move(m_storage)); }

    /**
     * @brief Returns an rvalue reference to the contained success value.
     *
     * @return a reference to the contained value of type `T`.
     * @pre `is_ok() == true`. The behavior is undefined if called on a `Result`
     *      containing an error.
     */
    constexpr T&& value() && noexcept { return std::get<T>(std::move(m_storage)); }
    /// @}

    // Group: Error accessors
    /// @{

    /**
     * @brief Returns a const lvalue reference to the contained error value.
     *
     * @return a const reference to the contained value of type `E`.
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` containing a value.
     */
    constexpr const E& value() const& noexcept { return std::get<E>(m_storage); }

    /**
     * @brief Returns an lvalue reference to the contained error value.
     *
     * @return a reference to the contained value of type `E`.
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` containing a value.
     */
    constexpr E& value() & noexcept { return std::get<E>(m_storage); }

    /**
     * @brief Returns a const rvalue reference to the contained error value.
     *
     * @return a const reference to the contained value of type `E`.
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` containing an error.
     */
    constexpr const E&& value() const&& noexcept { return std::get<E>(std::move(m_storage)); }

    /**
     * @brief Returns an rvalue reference to the contained error value.
     *
     * @return a reference to the contained value of type `E`.
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` containing an error.
     */
    constexpr E&& value() && noexcept { return std::get<E>(std::move(m_storage)); }
    /// @}

    /**
     * @brief Returns the contained value or a provided default.
     *
     * If `*this` contains a value, returns that value. Otherwise, returns
     * `default_value`.
     *
     * @tparam U Type of the default value. Must be convertible to `T`.
     * @param default_value The value to return if `*this` contains an error.
     *
     * @return The contained value if `is_ok()`, otherwise
     *         `static_cast<T>(std::forward<U>(default_value))`.
     *
     * @note The `const&` overload may involve a copy of the contained value or
     * the default value. The `&&` overload is more efficient as it can move
     * from the contained value.
     */
    template <typename U>
    constexpr T value_or(U&& default_value) const& {
        return is_ok() ? value() : static_cast<T>(std::forward<U>(default_value));
    }

    template <typename U>
    constexpr T value_or(U&& default_value) && {
        return is_ok() ? std::move(value()) : static_cast<T>(std::forward<U>(default_value));
    }

    // Group: Functional Transformers
    /// @{

    /**
     * @brief Applies a function to the contained value (if any), transforming
     * the success type.
     *
     * If `*this` is a `Result<T, E>` containing a value, returns a
     * `Result<U, E>` containing the result of applying `func` to the value.
     *
     * If `*this` contains an error, returns a `Result<U, E>` containing the
     * same error.
     *
     * @tparam F A callable type with the signature `U(const T&)` or compatible.
     * @param func The function to apply to the contained value.
     * @return A `Result<U, E>` containing the transformed value or the
     *         original error.
     */
    template <typename F>
    constexpr auto map(F&& func) const& -> Result<std::invoke_result_t<F, const & T>, E> {
        using U = std::invoke_result_t<F, const T&>;
        return is_ok() ? Result<U, E>(func(value())) : Result<U, E>(error());
    }

    template <typename F>
    constexpr auto map(F&& func) && -> Result<std::invoke_result_t<F, T&&>, E> {
        using U = std::invoke_result_t<F, T&&>;
        return is_ok() ? Result<U, E>(func(std::move(value()))) : Result<U, E>(std::move(error()));
    }

    /**
     * @brief Applies a function to the contained error (if any), transforming
     * the error type.
     *
     * If `*this` is a `Result<T, E>` containing an error, returns a
     * `Result<T, F>` containing the result of applying `func` to the error.
     *
     * If `*this` contains a value, returns a `Result<T, F>` containing the
     * same value.
     *
     * @tparam F A callable type with the signature `F(const E&)` or compatible.
     * @param func The function to apply to the contained error.
     * @return A `Result<T, F>` containing the original value or the
     *         transformed error.
     */
    template <typename F>
    constexpr auto map_err(F&& func) const& -> Result<T, std::invoke_result_t<F, const E&>> {
        using U = std::invoke_result_t<F, const E&>;
        return is_ok() ? Result<T, U>(value()) : Result<T, U>(func(error()));
    }

    template <typename F>
    constexpr auto map_err(F&& func) && -> Result<T, std::invoke_result_t<F, E&&>> {
        using U = std::invoke_result_t<F, E&&>;
        return is_ok() ? Result<T, U>(std::move(value())) : Result<T, U>(func(std::move(error())));
    }

    /**
     * @brief Monadic bind operation. Chains operations that may fail.
     *
     * If `*this` contains a value, applies `func` to it. `func` must return
     * a `Result<U, E>` type.
     *
     * If `*this` contains an error, returns a `Result<U, E>` containing that
     * same error.
     *
     * This is used for sequencing operations where each step might fail.
     *
     * @tparam F A callable type with the signature `Result<U, E>(const T&)` or
     *         compatible.
     * @param func The function to apply to the contained value.
     * @return The `Result<U, E>` returned by `func` or a result containing the
     *         original error.
     */
    template <typename F>
    constexpr auto and_then(F&& func) const& -> std::invoke_result_t<F, const T&> {
        return is_ok() ? func(value()) : std::invoke_result_t<F, const T&>(error());
    }

    template <typename F>
    constexpr auto and_then(F&& func) && -> std::invoke_result_t<F, T&&> {
        return is_ok() ? func(std::move(value())) : std::invoke_result_t<F, T&&>(std::move(error()));
    }
    /// @}

  private:
    std::variant<T, E> m_storage;
};

/**
 * @brief Creates a successful `Result` containing a value.
 *
 * This is a factory function for conveniently creating a `Result` object
 * in the success state. It decays the provided value type to handle references
 * and cv-qualifiers appropriately, and uses the default `Error` type.
 *
 * @tparam T The type of the value to be contained. It is decayed to remove
 *         references and cv-qualifiers, so `Ok(5)` returns `Result<int, Error>`
 *         and `Ok(std::string("hello"))` returns `Result<std::string, Error>`.
 * @param value The value to initialize the successful result with.
 *              It is perfectly-forwarded into the `Result` constructor.
 *
 * @return A `Result<std::decay_t<T>, Error>` object containing the
 *         successfully constructed value.
 *
 * @relates Result
 */
template <typename T>
constexpr Result<std::decay<T>, Error> Ok(T&& value) {
    return Result<std::decay_t<T>, Error>(std::forward<T>(value));
}

/**
 * @brief Creates a successful `void` `Result`.
 *
 * This factory function creates a `Result<void, E>`, which signifies a
 * successful operation that does not yield a value, only a potential error.
 * This is useful for functions that perform an operation and may fail but have
 * no value to return on success.
 *
 * @tparam E The type of the error. Defaults to `Error`.
 * @return A default-constructed `Result<void, E>` in the success state.
 * @relates Result
 */
template <typename E = Error>
constexpr Result<void, E> Ok() {
    return Result<void, E>();
}

/**
 * @brief Creates a failed `Result` containing an error for a `void` function.
 *
 * This factory function creates a `Result<void, E>` in the error state. It is
 * used to return an error from a function whose success case would return
 * `void`.
 *
 * @tparam E The type of the error. It is decayed to remove references and
 *         cv-qualifiers.
 *
 * @param error The error value to initialize the failed result with.
 *              It is perfectly-forwarded into the `Result` constructor.
 *
 * @return A `Result<void, std::decay_t<E>>` object containing the error.
 * @relates Result
 */
template <typename E = Error>
constexpr auto Err(E&& error) {
    return Result<void, std::decay_t<E>>(std::forward<E>(error));
}

/**
 * @brief Creates a failed `Result` containing an error for a value-returning
 * function.
 *
 * This factory function creates a `Result<T, E>` in the error state. It is
 * used to return an error from a function whose success case would return a
 * value of type `T`.
 *
 * @tparam T The value type of the `Result` to be returned.
 * @tparam E The type of the error. It is decayed to remove references and
 *         cv-qualifiers.
 * @param error The error value to initialize the failed result with.
 *              It is perfectly-forwarded into the `Result` constructor.
 *
 * @return A `Result<T, std::decay_t<E>>` object containing the error
 */
template <typename T, typename E>
constexpr auto Err(E&& error) {
    return Result<T, std::decay_t<E>>(std::forward<E>(error));
}

/**
 * @brief Specialization of Result for the case where the success type is
 * `void`.
 *
 * This partial specialization handles the common case where an operation
 * can fail but does not produce a value upon success. It represents the outcome
 * of a procedure that either completes successfully or returns an error.
 *
 * @tparam E The type of the error contained in a failed result. Must be
 *         nonthrow-destructible. Defaults to `Error`.
 *
 * @invariant The internal state is exclusively defined by `has_error`.
 *            - If `has_error` is `false`, the object is in the success state
 *              and `error` is not to be accessed.
 *            - If `has_error` is `true`, the object is in the error state and
 *              `error` contains a valid, constructed object of type `E`.
 *
 * @note This specialization does not store a value, only a boolean flag and
 * an error. The destructor of `E` must not throw.
 */
template <typename E>
class Result<void, E> {
  public:
    using value_type = void;
    using m_errortype = E;

  public:
    /**
     * @brief Constructs a successful `Result<void, E>`.
     *
     * Initializes the object to represent a successful outcome of a void
     * operation.
     *
     * @post `is_ok() == true` and `is_err() == false`.
     */
    constexpr Result() noexcept : m_has_error(false) {}

    /**
     * @brief Constructs a failed `Result<void, E>` from an lvalue error.
     *
     * @param error The error value to be copied into the result.
     *
     * @post `is_ok() == false` and `is_err() == true`.
     * @post `error()` returns a reference to a copy of `error`.
     */
    constexpr Result(const E& error) noexcept(std::is_nothrow_copy_constructible_v<E>)
        : m_has_error(true), m_error(error) {}

    /**
     * @brief Constructs a failed `Result<void, E>` from an rvalue error.
     *
     * @param error The error value to be moved into the result.
     *
     * @post `is_ok() == false` and `is_err() == true`.
     * @post `error()` returns a reference to the moved-from `error`.
     */
    constexpr Result(E&& error) noexcept(std::is_nothrow_move_constructible_v<E>)
        : m_has_error(true), m_error(std::move(error)) {}

    /**
     * @brief Checks if the `Result` represents a successful outcome.
     *
     * @return `true` if `*this` represents success (contains no error),
     *         `false` otherwise.
     */
    constexpr bool is_ok() const noexcept { return !m_has_error; }

    /**
     * @brief Checks if the `Result` contains an error.
     *
     * @return `true` if `*this` contains an error, `false` otherwise.
     */
    constexpr bool is_err() const noexcept { return m_has_error; }

    /**
     * @brief Contextually converts to `true` if the `Result` is successful.
     *
     * Allows for usage in boolean contexts like `if (result) { ... }`.
     *
     * @return `is_ok()`.
     */
    constexpr explicit operator bool() const noexcept { return is_ok(); }

    // Group: Error Accessors
    /// @{
    /**
     * @brief Returns a const lvalue reference to the contained error.
     *
     * @return A const reference to the contained error of type `E`.
     *
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` in the success state.
     */
    constexpr const E& error() const& noexcept { return m_error; }

    /**
     * @brief Returns an lvalue reference to the contained error.
     *
     * @return A reference to the contained error of type `E`.
     *
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` in the success state.
     */
    constexpr E& error() & noexcept { return m_error; }

    /**
     * @brief Returns a const rvalue reference to the contained error.
     *
     * @return A const rvalue reference to the contained error of type `E`.
     *
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` in the success state.
     */
    constexpr const E&& error() const&& noexcept { return std::move(m_error); }

    /**
     * @brief Returns an rvalue reference to the contained error.
     *
     * @return An rvalue reference to the contained error of type `E`.
     *
     * @pre `is_err() == true`. The behavior is undefined if called on a
     *      `Result` in the success state.
     */
    constexpr E&& error() && noexcept { return std::move(m_error); }
    /// @}

  private:
    bool m_has_error;
    Error m_error;
};

/**
 * @brief Macro for early return on error.
 */
#define BREZEL_TRY(expr)                   \
    do {                                   \
        auto&& _tensor_result = (expr);    \
        if (!_tensor_result.is_ok()) {     \
            return _tensor_result.error(); \
        }                                  \
    } while (0)

#define BREZEL_TRY_VALUE(expr)                                                 \
    [&]() -> decltype(auto) {                                                  \
        auto&& _tensor_result = (expr);                                        \
        if (!_tensor_result.is_ok()) {                                         \
            return _tensor_result.error();                                     \
        }                                                                      \
        return std::forward<decltype(_tensor_result)>(_tensor_result).value(); \
    }()
}  // namespace brezel::error