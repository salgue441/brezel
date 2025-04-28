/**
 * @file scalar.hpp
 * @author Carlos Salguero
 * @brief Defines the Scalar class for representing scalar values
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains the Scalar class, which represents a type-erased
 * value that can interact with tensors
 */

#pragma once

#include <complex>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <variant>

#include <brezel/core/dtype.hpp>

namespace brezel {
/**
 * @brief Class representing a scalar value of various types
 *
 * The Scalar class is a type-erased wrapper for scalar values that can
 * interact with tensors. It supports various data types and automatic
 * conversions between them.
 */
class Scalar {
public:
    using ValueType =
        std::variant<bool, uint8_t, int8_t, uint16_t, int16_t, uint32_t, int32_t, uint64_t, int64_t,
                     float, double, std::complex<float>, std::complex<double>, std::string>;

    /**
     * @brief Default constructor (creates scalar with value 0)
     */
    Scalar() : m_value(0.0) {}

    /**
     * @brief Construct from a boolean
     *
     * @param value Boolean value
     */
    Scalar(bool value) : m_value(value) {}

    /**
     * @brief Construct from a character
     *
     * @param value Character value
     */
    Scalar(char value) : m_value(static_cast<int8_t>(value)) {}

    /**
     * @brief Construct from an unsigned character
     *
     * @param value Unsigned character value
     */
    Scalar(unsigned char value) : m_value(static_cast<uint8_t>(value)) {}

    /**
     * @brief Construct from a signed integer
     *
     * @param value Integer value
     */
    Scalar(int value) : m_value(static_cast<int32_t>(value)) {}

    /**
     * @brief Construct from an unsigned integer
     *
     * @param value Unsigned integer value
     */
    Scalar(unsigned int value) : m_value(static_cast<uint32_t>(value)) {}

    /**
     * @brief Construct from a long integer
     *
     * @param value Long integer value
     */
    Scalar(long value) : m_value(static_cast<int64_t>(value)) {}

    /**
     * @brief Construct from an unsigned long integer
     *
     * @param value Unsigned long integer value
     */
    Scalar(unsigned long value) : m_value(static_cast<uint64_t>(value)) {}

    /**
     * @brief Construct from a long long integer
     *
     * @param value Long long integer value
     */
    Scalar(long long value) : m_value(static_cast<int64_t>(value)) {}

    /**
     * @brief Construct from an unsigned long long integer
     *
     * @param value Unsigned long long integer value
     */
    Scalar(unsigned long long value) : m_value(static_cast<uint64_t>(value)) {}

    /**
     * @brief Construct from a float
     *
     * @param value Float value
     */
    Scalar(float value) : m_value(value) {}

    /**
     * @brief Construct from a double
     *
     * @param value Double value
     */
    Scalar(double value) : m_value(value) {}

    /**
     * @brief Construct from a complex float
     *
     * @param value Complex float value
     */
    Scalar(const std::complex<float>& value) : m_value(value) {}

    /**
     * @brief Construct from a complex double
     *
     * @param value Complex double value
     */
    Scalar(const std::complex<double>& value) : m_value(value) {}

    /**
     * @brief Construct from a string
     *
     * @param value String value
     */
    Scalar(const std::string& value) : m_value(value) {}

    /**
     * @brief Construct from a C-string
     *
     * @param value C-string value
     */
    Scalar(const char* value) : m_value(std::string(value)) {}

    /**
     * @brief Get the stored value as the specified type
     *
     * @tparam T Target type
     * @return T Value as the target type
     * @throws std::bad_variant_access If the conversion is not possible
     */
    template <typename T>
    T to() const {
        if constexpr (std::is_same_v<T, bool>) {
            return to_bool();
        } else if constexpr (std::is_same_v<T, uint8_t>) {
            return to_uint8();
        } else if constexpr (std::is_same_v<T, int8_t>) {
            return to_int8();
        } else if constexpr (std::is_same_v<T, uint16_t>) {
            return to_uint16();
        } else if constexpr (std::is_same_v<T, int16_t>) {
            return to_int16();
        } else if constexpr (std::is_same_v<T, uint32_t>) {
            return to_uint32();
        } else if constexpr (std::is_same_v<T, int32_t>) {
            return to_int32();
        } else if constexpr (std::is_same_v<T, uint64_t>) {
            return to_uint64();
        } else if constexpr (std::is_same_v<T, int64_t>) {
            return to_int64();
        } else if constexpr (std::is_same_v<T, float>) {
            return to_float();
        } else if constexpr (std::is_same_v<T, double>) {
            return to_double();
        } else if constexpr (std::is_same_v<T, std::complex<float>>) {
            return to_complex_float();
        } else if constexpr (std::is_same_v<T, std::complex<double>>) {
            return to_complex_double();
        } else if constexpr (std::is_same_v<T, std::string>) {
            return to_string();
        } else {
            static_assert(std::is_void_v<T>, "Unsupported type for Scalar::to()");
            return T();
        }
    }

    /**
     * @brief Get the data type of the stored value
     *
     * @return DType Data type
     */
    DType dtype() const {
        return std::visit(
            [](const auto& v) -> DType {
                using T = std::decay_t<decltype(v)>;

                if constexpr (std::is_same_v<T, bool>) {
                    return DType::Bool;
                } else if constexpr (std::is_same_v<T, uint8_t>) {
                    return DType::UInt8;
                } else if constexpr (std::is_same_v<T, int8_t>) {
                    return DType::Int8;
                } else if constexpr (std::is_same_v<T, uint16_t>) {
                    return DType::UInt16;
                } else if constexpr (std::is_same_v<T, int16_t>) {
                    return DType::Int16;
                } else if constexpr (std::is_same_v<T, uint32_t>) {
                    return DType::UInt32;
                } else if constexpr (std::is_same_v<T, int32_t>) {
                    return DType::Int32;
                } else if constexpr (std::is_same_v<T, uint64_t>) {
                    return DType::UInt64;
                } else if constexpr (std::is_same_v<T, int64_t>) {
                    return DType::Int64;
                } else if constexpr (std::is_same_v<T, float>) {
                    return DType::Float32;
                } else if constexpr (std::is_same_v<T, double>) {
                    return DType::Float64;
                } else if constexpr (std::is_same_v<T, std::complex<float>>) {
                    return DType::Complex64;
                } else if constexpr (std::is_same_v<T, std::complex<double>>) {
                    return DType::Complex128;
                } else if constexpr (std::is_same_v<T, std::string>) {
                    return DType::String;
                } else {
                    return DType::Undefined;
                }
            },
            m_value);
    }

    /**
     * @brief Check if the stored value is a boolean
     *
     * @return bool Whether the value is a boolean
     */
    bool is_bool() const { return std::holds_alternative<bool>(m_value); }

    /**
     * @brief Check if the stored value is an integer
     *
     * @return bool Whether the value is an integer
     */
    bool is_integral() const {
        return std::holds_alternative<uint8_t>(m_value) ||
               std::holds_alternative<int8_t>(m_value) ||
               std::holds_alternative<uint16_t>(m_value) ||
               std::holds_alternative<int16_t>(m_value) ||
               std::holds_alternative<uint32_t>(m_value) ||
               std::holds_alternative<int32_t>(m_value) ||
               std::holds_alternative<uint64_t>(m_value) ||
               std::holds_alternative<int64_t>(m_value);
    }

    /**
     * @brief Check if the stored value is a floating point number
     *
     * @return bool Whether the value is a floating point number
     */
    bool is_floating_point() const {
        return std::holds_alternative<float>(m_value) || std::holds_alternative<double>(m_value);
    }

    /**
     * @brief Check if the stored value is a complex number
     *
     * @return bool Whether the value is a complex number
     */
    bool is_complex() const {
        return std::holds_alternative<std::complex<float>>(m_value) ||
               std::holds_alternative<std::complex<double>>(m_value);
    }

    /**
     * @brief Check if the stored value is a string
     *
     * @return bool Whether the value is a string
     */
    bool is_string() const { return std::holds_alternative<std::string>(m_value); }

    /**
     * @brief Check if the stored value is numeric (integral, floating point, or complex)
     *
     * @return bool Whether the value is numeric
     */
    bool is_numeric() const { return is_integral() || is_floating_point() || is_complex(); }

    /**
     * @brief Convert the stored value to a boolean
     *
     * @return bool Boolean value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    bool to_bool() const {
        return std::visit(
            [](const auto& v) -> bool {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v;
                } else if constexpr (std::is_integral_v<T>) {
                    return v != 0;
                } else if constexpr (std::is_floating_point_v<T>) {
                    return v != 0.0;
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return v.real() != 0.0 || v.imag() != 0.0;
                } else if constexpr (std::is_same_v<T, std::string>) {
                    return !v.empty();
                } else {
                    return false;
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to an unsigned 8-bit integer
     *
     * @return uint8_t Unsigned 8-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    uint8_t to_uint8() const {
        return std::visit(
            [](const auto& v) -> uint8_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<uint8_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<uint8_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<uint8_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to uint8_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a signed 8-bit integer
     *
     * @return int8_t Signed 8-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    int8_t to_int8() const {
        return std::visit(
            [](const auto& v) -> int8_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<int8_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<int8_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<int8_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to int8_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to an unsigned 16-bit integer
     *
     * @return uint16_t Unsigned 16-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    uint16_t to_uint16() const {
        return std::visit(
            [](const auto& v) -> uint16_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<uint16_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<uint16_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<uint16_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to uint16_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a signed 16-bit integer
     *
     * @return int16_t Signed 16-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    int16_t to_int16() const {
        return std::visit(
            [](const auto& v) -> int16_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<int16_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<int16_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<int16_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to int16_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to an unsigned 32-bit integer
     *
     * @return uint32_t Unsigned 32-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    uint32_t to_uint32() const {
        return std::visit(
            [](const auto& v) -> uint32_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<uint32_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<uint32_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<uint32_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to uint32_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a signed 32-bit integer
     *
     * @return int32_t Signed 32-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    int32_t to_int32() const {
        return std::visit(
            [](const auto& v) -> int32_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<int32_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<int32_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<int32_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to int32_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to an unsigned 64-bit integer
     *
     * @return uint64_t Unsigned 64-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    uint64_t to_uint64() const {
        return std::visit(
            [](const auto& v) -> uint64_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<uint64_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<uint64_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<uint64_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to uint64_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a signed 64-bit integer
     *
     * @return int64_t Signed 64-bit integer value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    int64_t to_int64() const {
        return std::visit(
            [](const auto& v) -> int64_t {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1 : 0;
                } else if constexpr (std::is_integral_v<T>) {
                    return static_cast<int64_t>(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return static_cast<int64_t>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<int64_t>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to int64_t");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a single-precision floating point number
     *
     * @return float Float value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    float to_float() const {
        return std::visit(
            [](const auto& v) -> float {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1.0f : 0.0f;
                } else if constexpr (std::is_integral_v<T> || std::is_floating_point_v<T>) {
                    return static_cast<float>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<float>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to float");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a double-precision floating point number
     *
     * @return double Double value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    double to_double() const {
        return std::visit(
            [](const auto& v) -> double {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? 1.0 : 0.0;
                } else if constexpr (std::is_integral_v<T> || std::is_floating_point_v<T>) {
                    return static_cast<double>(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return static_cast<double>(v.real());
                } else {
                    throw TypeConversionError("Cannot convert to double");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a complex float
     *
     * @return std::complex<float> Complex float value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    std::complex<float> to_complex_float() const {
        return std::visit(
            [](const auto& v) -> std::complex<float> {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return std::complex<float>(v ? 1.0f : 0.0f, 0.0f);
                } else if constexpr (std::is_integral_v<T> || std::is_floating_point_v<T>) {
                    return std::complex<float>(static_cast<float>(v), 0.0f);
                } else if constexpr (std::is_same_v<T, std::complex<float>>) {
                    return v;
                } else if constexpr (std::is_same_v<T, std::complex<double>>) {
                    return std::complex<float>(static_cast<float>(v.real()),
                                               static_cast<float>(v.imag()));
                } else {
                    throw TypeConversionError("Cannot convert to complex<float>");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a complex double
     *
     * @return std::complex<double> Complex double value
     * @throws std::bad_variant_access If the conversion is not possible
     */
    std::complex<double> to_complex_double() const {
        return std::visit(
            [](const auto& v) -> std::complex<double> {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return std::complex<double>(v ? 1.0 : 0.0, 0.0);
                } else if constexpr (std::is_integral_v<T> || std::is_floating_point_v<T>) {
                    return std::complex<double>(static_cast<double>(v), 0.0);
                } else if constexpr (std::is_same_v<T, std::complex<float>>) {
                    return std::complex<double>(static_cast<double>(v.real()),
                                                static_cast<double>(v.imag()));
                } else if constexpr (std::is_same_v<T, std::complex<double>>) {
                    return v;
                } else {
                    throw TypeConversionError("Cannot convert to complex<double>");
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to a string
     *
     * @return std::string String value
     */
    std::string to_string() const {
        return std::visit(
            [](const auto& v) -> std::string {
                using T = std::decay_t<decltype(v)>;
                if constexpr (std::is_same_v<T, bool>) {
                    return v ? "true" : "false";
                } else if constexpr (std::is_integral_v<T>) {
                    return std::to_string(v);
                } else if constexpr (std::is_floating_point_v<T>) {
                    return std::to_string(v);
                } else if constexpr (std::is_same_v<T, std::complex<float>> ||
                                     std::is_same_v<T, std::complex<double>>) {
                    return "(" + std::to_string(v.real()) + "," + std::to_string(v.imag()) + ")";
                } else if constexpr (std::is_same_v<T, std::string>) {
                    return v;
                } else {
                    return "";
                }
            },
            m_value);
    }

    /**
     * @brief Convert the stored value to the specified data type
     *
     * @param dtype Target data type
     * @return Scalar New scalar with the specified data type
     * @throws TypeConversionError If the conversion is not possible
     */
    Scalar to(DType dtype) const {
        switch (dtype) {
            case DType::Bool:
                return Scalar(to_bool());

            case DType::UInt8:
                return Scalar(to_uint8());

            case DType::Int8:
                return Scalar(to_int8());

            case DType::UInt16:
                return Scalar(to_uint16());

            case DType::Int16:
                return Scalar(to_int16());

            case DType::UInt32:
                return Scalar(to_uint32());

            case DType::Int32:
                return Scalar(to_int32());

            case DType::UInt64:
                return Scalar(to_uint64());

            case DType::Int64:
                return Scalar(to_int64());

            case DType::Float32:
                return Scalar(to_float());

            case DType::Float64:
                return Scalar(to_double());

            case DType::Complex64:
                return Scalar(to_complex_float());

            case DType::Complex128:
                return Scalar(to_complex_double());

            case DType::String:
                return Scalar(to_string());

            default:
                throw TypeConversionError("Unsupported conversion to " + dtype_to_string(dtype));
        }
    }

    /**
     * @brief Equality operator
     *
     * @param other Scalar to compare with
     * @return bool Whether the scalars are equal
     */
    bool operator==(const Scalar& other) const {
        if (m_value.index() == other.m_value.index()) {
            return m_value == other.m_value;
        }

        if (is_complex() || other.is_complex()) {
            return to_complex_double() == other.to_complex_double();
        } else if (is_floating_point() || other.is_floating_point()) {
            return to_double() == other.to_double();
        } else if (is_integral() || other.is_integral()) {
            return to_int64() == other.to_int64();
        } else if (is_bool() || other.is_bool()) {
            return to_bool() == other.to_bool();
        } else if (is_string() || other.is_string()) {
            return to_string() == other.to_string();
        }

        return false;
    }

    /**
     * @brief Inequality operator
     *
     * @param other Scalar to compare with
     * @return bool Whether the scalars are not equal
     */
    bool operator!=(const Scalar& other) const { return !(*this == other); }

    /**
     * @brief Less than operator
     *
     * @param other Scalar to compare with
     * @return bool Whether this scalar is less than the other
     * @throws TypeConversionError If the comparison is not possible
     */
    bool operator<(const Scalar& other) const {
        if (is_string() && other.is_string()) {
            return to_string() < other.to_string();
        } else if (is_numeric() && other.is_numeric()) {
            if (is_complex() || other.is_complex()) {
                throw TypeConversionError("Cannot compare complex numbers with <");
            } else if (is_floating_point() || other.is_floating_point()) {
                return to_double() < other.to_double();
            } else {
                return to_int64() < other.to_int64();
            }
        } else {
            throw TypeConversionError("Cannot compare different types with <");
        }
    }

    /**
     * @brief Greater than operator
     *
     * @param other Scalar to compare with
     * @return bool Whether this scalar is greater than the other
     * @throws TypeConversionError If the comparison is not possible
     */
    bool operator>(const Scalar& other) const { return other < *this; }

    /**
     * @brief Less than or equal operator
     *
     * @param other Scalar to compare with
     * @return bool Whether this scalar is less than or equal to the other
     * @throws TypeConversionError If the comparison is not possible
     */
    bool operator<=(const Scalar& other) const { return !(other < *this); }

    /**
     * @brief Greater than or equal operator
     *
     * @param other Scalar to compare with
     * @return bool Whether this scalar is greater than or equal to the other
     * @throws TypeConversionError If the comparison is not possible
     */
    bool operator>=(const Scalar& other) const { return !(*this < other); }

    /**
     * @brief Addition operator
     *
     * @param other Scalar to add
     * @return Scalar Sum of the scalars
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar operator+(const Scalar& other) const {
        if (is_string() && other.is_string()) {
            return Scalar(to_string() + other.to_string());
        } else if (is_numeric() && other.is_numeric()) {
            if (is_complex() || other.is_complex()) {
                return Scalar(to_complex_double() + other.to_complex_double());
            } else if (is_floating_point() || other.is_floating_point()) {
                return Scalar(to_double() + other.to_double());
            } else {
                return Scalar(to_int64() + other.to_int64());
            }
        } else {
            throw TypeConversionError("Cannot add different types");
        }
    }

    /**
     * @brief Subtraction operator
     *
     * @param other Scalar to subtract
     * @return Scalar Difference of the scalars
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar operator-(const Scalar& other) const {
        if (is_numeric() && other.is_numeric()) {
            if (is_complex() || other.is_complex()) {
                return Scalar(to_complex_double() - other.to_complex_double());
            } else if (is_floating_point() || other.is_floating_point()) {
                return Scalar(to_double() - other.to_double());
            } else {
                return Scalar(to_int64() - other.to_int64());
            }
        } else {
            throw TypeConversionError("Cannot subtract different types");
        }
    }

    /**
     * @brief Multiplication operator
     *
     * @param other Scalar to multiply by
     * @return Scalar Product of the scalars
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar operator*(const Scalar& other) const {
        if (is_numeric() && other.is_numeric()) {
            if (is_complex() || other.is_complex()) {
                return Scalar(to_complex_double() * other.to_complex_double());
            } else if (is_floating_point() || other.is_floating_point()) {
                return Scalar(to_double() * other.to_double());
            } else {
                return Scalar(to_int64() * other.to_int64());
            }
        } else {
            throw TypeConversionError("Cannot multiply different types");
        }
    }

    /**
     * @brief Division operator
     *
     * @param other Scalar to divide by
     * @return Scalar Quotient of the scalars
     * @throws TypeConversionError If the operation is not possible
     * @throws std::domain_error If division by zero
     */
    Scalar operator/(const Scalar& other) const {
        if (is_numeric() && other.is_numeric()) {
            if (other.is_complex()) {
                auto denom = other.to_complex_double();
                if (denom.real() == 0.0 && denom.imag() == 0.0) {
                    throw std::domain_error("Division by zero");
                }

                return Scalar(to_complex_double() / denom);
            } else if (other.is_floating_point()) {
                auto denom = other.to_double();
                if (denom == 0.0) {
                    throw std::domain_error("Division by zero");
                }

                return Scalar(to_double() / denom);
            } else {
                auto denom = other.to_int64();
                if (denom == 0) {
                    throw std::domain_error("Division by zero");
                }

                if (is_complex()) {
                    return Scalar(to_complex_double() / static_cast<double>(denom));
                } else if (is_floating_point()) {
                    return Scalar(to_double() / static_cast<double>(denom));
                } else {
                    return Scalar(to_int64() / denom);
                }
            }
        } else {
            throw TypeConversionError("Cannot divide different types");
        }
    }

    /**
     * @brief Unary minus operator
     *
     * @return Scalar Negated scalar
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar operator-() const {
        if (is_numeric()) {
            if (is_complex()) {
                return Scalar(-to_complex_double());
            } else if (is_floating_point()) {
                return Scalar(-to_double());
            } else {
                return Scalar(-to_int64());
            }
        } else {
            throw TypeConversionError("Cannot negate non-numeric type");
        }
    }

    /**
     * @brief Addition assignment operator
     *
     * @param other Scalar to add
     * @return Scalar& Reference to this
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar& operator+=(const Scalar& other) {
        *this = *this + other;
        return *this;
    }

    /**
     * @brief Subtraction assignment operator
     *
     * @param other Scalar to subtract
     * @return Scalar& Reference to this
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar& operator-=(const Scalar& other) {
        *this = *this - other;
        return *this;
    }

    /**
     * @brief Multiplication assignment operator
     *
     * @param other Scalar to multiply by
     * @return Scalar& Reference to this
     * @throws TypeConversionError If the operation is not possible
     */
    Scalar& operator*=(const Scalar& other) {
        *this = *this * other;
        return *this;
    }

    /**
     * @brief Division assignment operator
     *
     * @param other Scalar to divide by
     * @return Scalar& Reference to this
     * @throws TypeConversionError If the operation is not possible
     * @throws std::domain_error If division by zero
     */
    Scalar& operator/=(const Scalar& other) {
        *this = *this / other;
        return *this;
    }

    /**
     * @brief Convert to string representation with type information
     *
     * @return std::string String representation with type information
     */
    std::string to_string_with_type() const {
        std::string value_str = to_string();
        std::string type_str = dtype_to_string(dtype());

        return value_str + " (" + type_str + ")";
    }

private:
    ValueType m_value;
};

/**
 * @brief Output stream operator for Scalar
 *
 * @param os Output stream
 * @param scalar Scalar to output
 * @return std::ostream& Reference to the output stream
 */
inline std::ostream& operator<<(std::ostream& os, const Scalar& scalar) {
    return os << scalar.to_string();
}

/**
 * @brief Addition operator with scalar on the right
 *
 * @param value Value on the left
 * @param scalar Scalar on the right
 * @return Scalar Result of the addition
 */
template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
Scalar operator+(T value, const Scalar& scalar) {
    return Scalar(value) + scalar;
}

/**
 * @brief Subtraction operator with scalar on the right
 *
 * @param value Value on the left
 * @param scalar Scalar on the right
 * @return Scalar Result of the subtraction
 */
template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
Scalar operator-(T value, const Scalar& scalar) {
    return Scalar(value) - scalar;
}

/**
 * @brief Multiplication operator with scalar on the right
 *
 * @param value Value on the left
 * @param scalar Scalar on the right
 * @return Scalar Result of the multiplication
 */
template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
Scalar operator*(T value, const Scalar& scalar) {
    return Scalar(value) * scalar;
}

/**
 * @brief Division operator with scalar on the right
 *
 * @param value Value on the left
 * @param scalar Scalar on the right
 * @return Scalar Result of the division
 */
template <typename T, typename = std::enable_if_t<std::is_arithmetic_v<T>>>
Scalar operator/(T value, const Scalar& scalar) {
    return Scalar(value) / scalar;
}
}  // namespace brezel