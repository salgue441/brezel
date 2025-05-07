/**
 * @file dtype.hpp
 * @author Carlos Salguero
 * @brief Defines data types used in the brezel library
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains enum definitions and utilities for handling
 * different data types that can be used for tensor elements
 */

#pragma once

#include <complex>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>

namespace brezel {
/**
 * @brief Enumeration of supported data types
 */
enum class DType {
    Undefined,   ///< Undefined data type
    Bool,        ///< Boolean
    UInt8,       ///< 8-bit unsigned integer
    Int8,        ///< 8-bit signed integer
    UInt16,      ///< 16-bit unsigned integer
    Int16,       ///< 16-bit signed integer
    UInt32,      ///< 32-bit unsigned integer
    Int32,       ///< 32-bit signed integer
    UInt64,      ///< 64-bit unsigned integer
    Int64,       ///< 64-bit signed integer
    Float16,     ///< 16-bit floating point (half precision)
    Float32,     ///< 32-bit floating point (single precision)
    Float64,     ///< 64-bit floating point (double precision)
    Complex32,   ///< 32-bit complex number (2 x 16-bit floats)
    Complex64,   ///< 64-bit complex number (2 x 32-bit floats)
    Complex128,  ///< 128-bit complex number (2 x 64-bit floats)
    BFloat16,    ///< 16-bit brain floating point
    String,      ///< String type (variable length)
};

/**
 * @brief Get the size of a data type in bytes
 *
 * @param dtype The data type
 * @return size_t Size in bytes
 */
inline size_t dtype_size(DType dtype) {
    switch (dtype) {
        case DType::Bool:
            return sizeof(bool);

        case DType::UInt8:
            return sizeof(uint8_t);

        case DType::Int8:
            return sizeof(int8_t);

        case DType::UInt16:
            return sizeof(uint16_t);

        case DType::Int16:
            return sizeof(int16_t);

        case DType::UInt32:
            return sizeof(uint32_t);

        case DType::Int32:
            return sizeof(int32_t);

        case DType::UInt64:
            return sizeof(uint64_t);

        case DType::Int64:
            return sizeof(int64_t);

        case DType::Float16:
            return 2;

        case DType::Float32:
            return sizeof(float);

        case DType::Float64:
            return sizeof(double);

        case DType::Complex32:
            return 4;

        case DType::Complex64:
            return sizeof(std::complex<float>);

        case DType::Complex128:
            return sizeof(std::complex<double>);

        case DType::BFloat16:
            return 2;

        case DType::String:
            return sizeof(void*);

        default:
            throw std::runtime_error("Unknown dtype");
    }
}

/**
 * @brief Check if a data type is floating point
 *
 * @param dtype The data type
 * @return bool Whether the data type is floating point
 */
inline bool is_floating_point(DType dtype) {
    return dtype == DType::Float16 || dtype == DType::Float32 || dtype == DType::Float64 ||
           dtype == DType::BFloat16;
}

/**
 * @brief Check if a data type is integral
 *
 * @param dtype The data type
 * @return bool Whether the data type is integral
 */
inline bool is_integral(DType dtype) {
    return dtype == DType::UInt8 || dtype == DType::Int8 || dtype == DType::UInt16 ||
           dtype == DType::Int16 || dtype == DType::UInt32 || dtype == DType::Int32 ||
           dtype == DType::UInt64 || dtype == DType::Int64;
}

/**
 * @brief Check if a data type is complex
 *
 * @param dtype The data type
 * @return bool Whether the data type is complex
 */
inline bool is_complex(DType dtype) {
    return dtype == DType::Complex32 || dtype == DType::Complex64 || dtype == DType::Complex128;
}

/**
 * @brief Convert a data type to a string
 *
 * @param dtype The data type
 * @return std::string String representation
 */
inline std::string dtype_to_string(DType dtype) {
    switch (dtype) {
        case DType::Undefined:
            return "Undefined";

        case DType::Bool:
            return "Bool";

        case DType::UInt8:
            return "UInt8";

        case DType::Int8:
            return "Int8";

        case DType::UInt16:
            return "UInt16";

        case DType::Int16:
            return "Int16";

        case DType::UInt32:
            return "UInt32";

        case DType::Int32:
            return "Int32";

        case DType::UInt64:
            return "UInt64";

        case DType::Int64:
            return "Int64";

        case DType::Float16:
            return "Float16";

        case DType::Float32:
            return "Float32";

        case DType::Float64:
            return "Float64";

        case DType::Complex32:
            return "Complex32";

        case DType::Complex64:
            return "Complex64";

        case DType::Complex128:
            return "Complex128";

        case DType::BFloat16:
            return "BFloat16";

        case DType::String:
            return "String";

        default:
            return "Unknown";
    }
}

/**
 * @brief Template to map C++ types to brezel DTypes
 * @tparam T The C++ type
 */
template <typename T>
struct TypeToDType;

// Specializations for each supported type
template <>
struct TypeToDType<bool> {
    static constexpr DType value = DType::Bool;
};

template <>
struct TypeToDType<uint8_t> {
    static constexpr DType value = DType::UInt8;
};

template <>
struct TypeToDType<int8_t> {
    static constexpr DType value = DType::Int8;
};

template <>
struct TypeToDType<uint16_t> {
    static constexpr DType value = DType::UInt16;
};

template <>
struct TypeToDType<int16_t> {
    static constexpr DType value = DType::Int16;
};

template <>
struct TypeToDType<uint32_t> {
    static constexpr DType value = DType::UInt32;
};

template <>
struct TypeToDType<int32_t> {
    static constexpr DType value = DType::Int32;
};

template <>
struct TypeToDType<uint64_t> {
    static constexpr DType value = DType::UInt64;
};

template <>
struct TypeToDType<int64_t> {
    static constexpr DType value = DType::Int64;
};

template <>
struct TypeToDType<float> {
    static constexpr DType value = DType::Float32;
};

template <>
struct TypeToDType<double> {
    static constexpr DType value = DType::Float64;
};

template <>
struct TypeToDType<std::complex<float>> {
    static constexpr DType value = DType::Complex64;
};

template <>
struct TypeToDType<std::complex<double>> {
    static constexpr DType value = DType::Complex128;
};

template <>
struct TypeToDType<std::string> {
    static constexpr DType value = DType::String;
};

/**
 * @brief Get the DType for a C++ type
 * @tparam T The C++ type
 * @return DType The corresponding DType
 */
template <typename T>
inline DType get_dtype() {
    return TypeToDType<T>::value;
}

/**
 * @brief Exception thrown when type conversion fails
 */
class TypeConversionError : public std::runtime_error {
public:
    /**
     * @brief Construct a new Type Conversion error
     *
     * @param message Error message
     */
    explicit TypeConversionError(const std::string& message) : std::runtime_error(message) {}
};
}  // namespace brezel