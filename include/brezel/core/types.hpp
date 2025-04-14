#pragma once

#include <complex>
#include <concepts>
#include <cstdint>
#include <string>
#include <type_traits>
#include <vector>

namespace brezel {
/**
 * @brief Common types
 */
using index_t = std::int64_t;           ///< Type used for tensor indices
using size_t = std::size_t;             ///< Type used for sizes and dimensions
using shape_t = std::vector<index_t>;   ///< Type representing tensor shapes
using stride_t = std::vector<index_t>;  ///< Type representing tensor strides
using byte_t = std::uint8_t;            ///< Type used for raw byte data

/**
 * @brief Device types supported by the framework
 */
enum class DeviceType {
    CPU,    ///< CPU device (default)
    CUDA,   ///< NVIDIA CUDA device
    MPS,    ///< Apple Metal Performance Shaders device
    OpenCL  ///< OpenCL device
};

/**
 * @brief Data types supported by the framework
 */
enum class DataType {
    Bool,       ///< Boolean type
    Uint8,      ///< 8-bit unsigned integer
    Int8,       ///< 8-bit signed integer
    Int16,      ///< 16-bit signed integer
    Int32,      ///< 32-bit signed integer
    Int64,      ///< 64-bit signed integer
    Float16,    ///< 16-bit floating point (half precision)
    Float32,    ///< 32-bit floating point (single precision)
    Float64,    ///< 64-bit floating point (double precision)
    Complex32,  ///< Complex number with single precision components
    Complex64   ///< Complex number with double precision components
};

/**
 * @brief Layout types for tensor memory
 */
enum class Layout {
    Strided,  ///< Standard strided memory layout (default)
    Sparse,   ///< Sparse memory layout
    COO,      ///< Coordinate format sparse layout
    CSR       ///< Compressed sparse row format
};

/**
 * @brief Mapping from DataType to C++ type
 *
 * @tparam dtype The DataType enum value
 */
template <DataType dtype>
struct DataTypeToNative {};

// Specialization for each supported data type
template <>
struct DataTypeToNative<DataType::Bool> {
    using type = bool;
};

template <>
struct DataTypeToNative<DataType::Uint8> {
    using type = std::uint8_t;
};

template <>
struct DataTypeToNative<DataType::Int8> {
    using type = std::int8_t;
};

template <>
struct DataTypeToNative<DataType::Int16> {
    using type = std::int16_t;
};

template <>
struct DataTypeToNative<DataType::Int32> {
    using type = std::int32_t;
};

template <>
struct DataTypeToNative<DataType::Int64> {
    using type = std::int64_t;
};

template <>
struct DataTypeToNative<DataType::Float32> {
    using type = float;
};

template <>
struct DataTypeToNative<DataType::Float64> {
    using type = double;
};

template <>
struct DataTypeToNative<DataType::Complex32> {
    using type = std::complex<float>;
};

template <>
struct DataTypeToNative<DataType::Complex64> {
    using type = std::complex<double>;
};

/**
 * @brief Helper alias to get the C++ type from a DataType
 */
template <DataType dtype>
using dtype_t = typename DataTypeToNative<dtype>::type;

/**
 * @brief Concept for numeric types
 */
template <typename T>
concept Numeric = std::is_arithmetic_v<T> || std::is_same_v<T, std::complex<float>> ||
                  std::is_same_v<T, std::complex<double>>;

/**
 * @brief Concept for tensor-compatible scalar types
 */
template <typename T>
concept Scalar = Numeric<T>;

/**
 * @brief Concept for floating point types
 */
template <typename T>
concept FloatingPoint = std::is_floating_point_v<T>;

/**
 * @brief Concept for integer types
 */
template <typename T>
concept Integer = std::is_integral_v<T>;

/**
 * @brief Concept for complex number types
 */
template <typename T>
concept Complex = std::is_same_v<T, std::complex<float>> || std::is_same_v<T, std::complex<double>>;

/**
 * @brief Get the DataType enum value from a C++ type
 *
 * @tparam T The C++ type
 * @return constexpr DataType The corresponding DataType enum value
 */
template <typename T>
constexpr DataType get_data_type() {
    if constexpr (std::is_same_v<T, bool>)
        return DataType::Bool;

    else if constexpr (std::is_same_v<T, std::uint8_t>)
        return DataType::Uint8;

    else if constexpr (std::is_same_v<T, std::int8_t>)
        return DataType::Uint8;

    else if constexpr (std::is_same_v<T, std::int16_t>)
        return DataType::Int16;

    else if constexpr (std::is_same_v<T, std::int32_t>)
        return DataType::Int32;

    else if constexpr (std::is_same_v<T, std::int64_t>)
        return DataType::Int64;

    else if constexpr (std::is_same_v<T, float>)
        return DataType::Float32;

    else if constexpr (std::is_same_v<T, double>)
        return DataType::Float64;

    else if constexpr (std::is_same_v<T, std::complex<float>>)
        return DataType::Complex32;

    else if constexpr (std::is_same_v<T, std::complex<double>>)
        return DataType::Complex64;

    else
        static_assert(always_false<T>, "Unsupported type for get_data_type");
}

// Helper for static assertions
template <typename T>
inline constexpr bool always_false = false;

/**
 * @brief Get the size in bytes of a DataType
 *
 * @param dtype The DataType enum value
 * @return size_t Size in bytes
 */
constexpr size_t data_type_size(DataType dtype) {
    switch (dtype) {
        case DataType::Bool:
            return sizeof(bool);

        case DataType::Uint8:
            return sizeof(std::uint8_t);

        case DataType::Int8:
            return sizeof(std::int8_t);

        case DataType::Int16:
            return sizeof(std::int16_t);

        case DataType::Int32:
            return sizeof(std::int32_t);

        case DataType::Int64:
            return sizeof(std::int64_t);

        case DataType::Float16:
            return 2;

        case DataType::Float32:
            return sizeof(float);

        case DataType::Float64:
            return sizeof(double);

        case DataType::Complex32:
            return sizeof(std::complex<float>);

        case DataType::Complex64:
            return sizeof(std::complex<double>);

        default:
            return 0;
    }
}

/**
 * @brief Convert a DataType to a human-readable string
 *
 * @param dtype The DataType enum value
 * @return std::string String representation
 */
inline std::string data_type_to_string(DataType dtype) {
    switch (dtype) {
        case DataType::Bool:
            return "bool";

        case DataType::Uint8:
            return "uint8";

        case DataType::Int8:
            return "int8";

        case DataType::Int16:
            return "int16";

        case DataType::Int32:
            return "int32";

        case DataType::Int64:
            return "int64";

        case DataType::Float16:
            return "float16";

        case DataType::Float32:
            return "float32";

        case DataType::Float64:
            return "float64";

        case DataType::Complex32:
            return "complex32";

        case DataType::Complex64:
            return "complex64";
            
        default:
            return "unknown";
    }
}
}  // namespace brezel