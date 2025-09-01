/**
 * @file dtype.hpp
 * @author your name (you@domain.com)
 * @brief
 * @version 0.1
 * @date 2025-09-01
 *
 * @copyright Copyright (c) 2025
 *
 */

#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeinfo>

namespace brezel::core {
/**
 * @brief Enumeration of supported data types for tensor operations.
 *
 * This enum classifies all primitive data types that can be used as elements
 * in tensors. The values are grouped by category and assigned specific
 * numeric ranges for easy categorization.
 */
enum class DType : uint8_t {
    Bool = 0,
    Int8 = 10,
    Int16 = 11,
    Int32 = 12,
    Int64 = 13,
    UInt8 = 20,
    UInt16 = 21,
    UInt32 = 22,
    UInt64 = 23,
    Float16 = 30,
    BFloat16 = 31,
    Float32 = 32,
    Float64 = 33,
    Complex64 = 40,
    Complex128 = 41,
    Unknown = 255
};

/**
 * @brief Type traits for mapping C++ types to DType values.
 *
 * @internal
 * The namespace contains the implementation details for type-DType mapping.
 */
namespace detail {
/**
 * @brief Primary template for dtype traits (unsupported by default).
 *
 * @tparam T The C++ type to query.
 */
template <typename T>
struct dtype_traits {
    static constexpr DType value = DType::Unknown;
    static constexpr bool is_supported = false;

    // Specialization for supported types
    template <>
    struct dtype_traits<bool> {
        static constexpr DType value = DType::Bool;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<int8_t> {
        static constexpr DType value = DType::Int8;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<int16_t> {
        static constexpr DType value = DType::Int16;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<int32_t> {
        static constexpr DType value = DType::Int32;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<int64_t> {
        static constexpr DType value = DType::Int64;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<uint8_t> {
        static constexpr DType value = DType::UInt8;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<uint16_t> {
        static constexpr DType value = DType::UInt16;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<uint32_t> {
        static constexpr DType value = DType::UInt32;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<uint64_t> {
        static constexpr DType value = DType::UInt64;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<float> {
        static constexpr DType value = DType::Float32;
        static constexpr bool is_supported = true;
    };

    template <>
    struct dtype_traits<double> {
        static constexpr DType value = DType::Float64;
        static constexpr bool is_supported = true;
    };
};
}  // namespace detail

/**
 * @brief Compile-time mapping from C++ type to DType.
 *
 * @tparam T The C++ type to query.
 * @return The DType corresponding to the C++ type T, with cv-qualifiers
 *         removed. Returns DType::Unknown for unsupported types.
 *
 * Example:
 * @code {cpp}
 * static_assert(dtype_v<int32_t> == DType::Int32);
 * static_assert(dtype_v<const float> == DType::FLoat32);
 * @endcode
 */
template <typename T>
constexpr DType dtype_v = detail::dtype_traits<std::remove_cv_t<T>>::value;

/**
 * @brief Compile-time check if a C++ type is supported as a tensor element
 * type.
 *
 * @tparam T The C++ type to check.
 * @return true if T is a supported tensor element type, false otherwise.
 *
 * Example:
 * @code
 * static_assert(is_supported_type_v<float> == true);
 * static_assert(is_supported_type_v<std::string> == false);
 * @endcode
 */
template <typename T>
constexpr bool is_supported_type_v = detail::dtype_traits<std::remove_cv_t<T>>::is_supported;

/**
 * @brief Concept for constraining templates to supported tensor element types.
 *
 * This concept can be used to constrain template parameters to only accept
 * types that can are valid for use as tensor elements.
 *
 * @tparam T The type to check against the concept.
 */
template <typename T>
concept SupportedType = is_supported_type_v<T>;

/**
 * @brief Returns the size in bytes of a single element of the specified dtype.
 *
 * @param dtype The data type to query.
 *
 * @return The size in bytes of a single element of type dtype.
 *         Returns 0 for unsupported or future dtypes (e.g., Float16, Complex
 *         types).
 */
constexpr size_t sizeof_dtype(DType dtype) noexcept {
    switch (dtype) {
        case DType::Bool:
            return sizeof(bool);
        case DType::Int8:
            return sizeof(int8_t);
        case DType::Int16:
            return sizeof(int16_t);
        case DType::Int32:
            return sizeof(int32_t);
        case DType::Int64:
            return sizeof(int64_t);
        case DType::UInt8:
            return sizeof(uint8_t);
        case DType::UInt16:
            return sizeof(uint16_t);
        case DType::UInt32:
            return sizeof(uint32_t);
        case DType::UInt64:
            return sizeof(uint64_t);
        case DType::Float32:
            return sizeof(float);
        case DType::Float64:
            return sizeof(double);
        default:
            return 0;
    }
}

/**
 * @brief Returns the alignment requirement of a single element of the
 * specified dtype.
 *
 * @param dtype The data type to query.
 *
 * @return The alignment requirement in bytes for elements of type dtype.
 *         Returns 1 for unsupported or future dtypes.
 */
constexpr size_t alignof_dtype(DType dtype) noexcept {
    switch (dtype) {
        case DType::Bool:
            return alignof(bool);
        case DType::Int8:
            return alignof(int8_t);
        case DType::Int16:
            return alignof(int16_t);
        case DType::Int32:
            return alignof(int32_t);
        case DType::Int64:
            return alignof(int64_t);
        case DType::UInt8:
            return alignof(uint8_t);
        case DType::UInt16:
            return alignof(uint16_t);
        case DType::UInt32:
            return alignof(uint32_t);
        case DType::UInt64:
            return alignof(uint64_t);
        case DType::Float32:
            return alignof(float);
        case DType::Float64:
            return alignof(double);
        default:
            return 1;
    }
}

/**
 * @brief Checks if a dtype represents an integer type (signed or unsigned).
 *
 * @param dtype The data type to check.
 * @return true if dtype is an integer type (Int8-Int64 or UInt8-UInt64),
 *         false otherwise.
 */
constexpr bool is_integer_dtype(DType dtype) noexcept {
    return (dtype >= DType::Int8 && dtype <= DType::Int64) || (dtype >= DType::UInt8 && dtype <= DType::UInt64);
}

/**
 * @brief Checks if a dtype represents a signed integer type.
 *
 * @param dtype The data type to check.
 * @return true if dtype is a signed integer type (Int8-Int64), false otherwise.
 */
constexpr bool is_signed_integer_dtype(DType dtype) noexcept { return dtype >= DType::Int8 && dtype <= DType::Int64; }

/**
 * @brief Checks if a dtype represents an unsigned integer type.
 *
 * @param dtype The data type to check.
 * @return true if dtype is an unsigned integer type (UInt8-UInt64), false
 *         otherwise.
 */
constexpr bool is_unsigned_integer_dtype(DType dtype) noexcept {
    return dtype >= DType::UInt8 && dtype <= DType::UInt64;
}

/**
 * @brief Checks if a dtype represents a floating-point type.
 *
 * @param dtype The data type to check.
 * @return true if dtype is a floating-point type (Float16-Float64), false
 *         otherwise.
 */
constexpr bool is_floating_point_dtype(DType dtype) noexcept {
    return dtype >= DType::Float16 && dtype <= DType::Float64;
}

/**
 * @brief Checks if a dtype represents a boolean type.
 *
 * @param dtype The data type to check.
 * @return true if dtype is DType::Bool, false otherwise.
 */
constexpr bool is_boolean_dtype(DType dtype) noexcept { return dtype == DType::Bool; }

/**
 * @brief Returns a human-readable string representation of a dtype.
 *
 * @param dtype The data type to get the name of.
 * @return A string_view containing the name of the dtype (e.g., "int32",
 *         "float64"). Returns "unknown" for unsupported dtypes.
 */
constexpr std::string_view dtype_name(DType dtype) noexcept {
    switch (dtype) {
        case DType::Bool:
            return "bool";
        case DType::Int8:
            return "int8";
        case DType::Int16:
            return "int16";
        case DType::Int32:
            return "int32";
        case DType::Int64:
            return "int64";
        case DType::UInt8:
            return "uint8";
        case DType::UInt16:
            return "uint16";
        case DType::UInt32:
            return "uint32";
        case DType::UInt64:
            return "uint64";
        case DType::Float16:
            return "float16";
        case DType::BFloat16:
            return "bfloat16";
        case DType::Float32:
            return "float32";
        case DType::Float64:
            return "float64";
        case DType::Complex64:
            return "complex64";
        case DType::Complex128:
            return "complex128";
        default:
            return "unknown";
    }
}

/**
 * @brief Parses a dtype from its string representation.
 *
 * @param name The string name of the dtype (e.g., "int32", "float").
 * @return The DType corresponding to the name, or DType::Unknown if the name
 *         is not recognized.
 *
 * @note Accepts both specific names ("float32") and common aliases ("float").
 */
constexpr DType parse_dtype(std::string_view name) noexcept {
    if (name == "bool") return DType::Bool;
    if (name == "int8") return DType::Int8;
    if (name == "int16") return DType::Int16;
    if (name == "int32") return DType::Int32;
    if (name == "int64") return DType::Int64;
    if (name == "uint8") return DType::UInt8;
    if (name == "uint16") return DType::UInt16;
    if (name == "uint32") return DType::UInt32;
    if (name == "uint64") return DType::UInt64;
    if (name == "float16") return DType::Float16;
    if (name == "bfloat16") return DType::BFloat16;
    if (name == "float32" || name == "float") return DType::Float32;
    if (name == "float64" || name == "double") return DType::Float64;
    if (name == "complex64") return DType::Complex64;
    if (name == "complex128") return DType::Complex128;
    return DType::Unknown;
}

/**
 * @brief Type-erased visitor for performing operations based on dtype.
 *
 * This function enables runtime dispatch to template code based on the
 * actual dtype value. The visitor must be callable that can accept any
 * supported type via std::type_identity<T>.
 *
 * @tparam Visitor The type of the visitor callable. Must be invocable with
 *         std::type_identity<T> for all supported T.
 * @param dtype The data type to visit.
 * @param visitor The callable object to invoke with the appropriate type
 *                identity.
 *
 * @return The result of invoking the visitor with the appropriate type
 *         identity.
 * @throws std::runtime_error if dtype is not supported.
 *
 * Example:
 * @code {cpp}
 * auto type_name = visit_dtype(dtype, [](auto type_id) {
 *  using T = typename decltype(type_id)::type;
 *  return typeid(T).name();
 * })
 * @endcode
 */
template <typename Visitor>
decltype(auto) visit_dtype(DType dtype, Visitor&& visitor) {
    switch (dtype) {
        case DType::Bool:
            return visitor(std::type_identity<bool>{});
        case DType::Int8:
            return visitor(std::type_identity<int8_t>{});
        case DType::Int16:
            return visitor(std::type_identity<int16_t>{});
        case DType::Int32:
            return visitor(std::type_identity<int32_t>{});
        case DType::Int64:
            return visitor(std::type_identity<int64_t>{});
        case DType::UInt8:
            return visitor(std::type_identity<uint8_t>{});
        case DType::UInt16:
            return visitor(std::type_identity<uint16_t>{});
        case DType::UInt32:
            return visitor(std::type_identity<uint32_t>{});
        case DType::UInt64:
            return visitor(std::type_identity<uint64_t>{});
        case DType::Float32:
            return visitor(std::type_identity<float>{});
        case DType::Float64:
            return visitor(std::type_identity<double>{});
        default:
            throw std::runtime_error("Unsupported dtype: " + std::string(dtype_name(dtype)));
    }
}

/**
 * @brief Determines the common promoted type for binary operations.
 *
 * Implements type promotion rules similar to NumPy's type promotion system.
 * When performing operations between two different dtypes, this function
 * determines the appropriate result type.
 *
 * @param lhs The dtype of the left operand.
 * @param rhs The dtype of the right operand.
 *
 * @return The promoted dtype that should be used for the operation result.
 *         Returns DType::Unknown if promotion rules cannot be determined.
 *
 * @noexcept Noexcept guarantee.
 *
 * Promotion rules:
 * - Same types: returns the same type
 * - Bool promotes to the other type
 * - Floating point types dominate (promote to the larger float type)
 * - Integer types promote to the larger type, with signed preferred for equal
 *   sizes
 */
constexpr DType promote_dtypes(DType lhs, DType rhs) noexcept {
    if (lhs == rhs) return lhs;
    if (lhs == DType::Bool) return rhs;
    if (rhs == DType::Bool) return lhs;

    if (is_floating_point_dtype(lhs) || is_floating_point_dtype(rhs)) {
        if (lhs == DType::Float64 || rhs == DType::Float64) return DType::Float64;

        return DType::Float32;
    }

    if (is_integer_dtype(lhs) && is_integer_dtype(rhs)) {
        size_t lhs_size = sizeof_dtype(lhs);
        size_t rhs_size = sizeof_dtype(rhs);

        if (lhs_size > rhs_size) return lhs;
        if (rhs_size > lhs_size) return rhs;

        if (is_signed_integer_dtype(lhs)) return lhs;
        return rhs;
    }

    return DType::Unknown;
}
}  // namespace brezel::core