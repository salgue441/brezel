/**
 * @file tensor_errors.hpp
 * @author Carlos Salguero
 * @brief Tensor-related error classes for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines error classes related to tensor operations
 * in the brezel framework
 */

#pragma once

#include <source_location>
#include <string>
#include <vector>

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>

#include <fmt/core.h>
#include <fmt/format.h>

namespace brezel::error {
/**
 * @brief Tensor operation errors
 */
class BREZEL_API TensorError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    TensorError(std::string message, Code code = Code::InvalidOperation,
                const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit TensorError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Invalid tensor shape error
 */
class BREZEL_API InvalidShapeError : public TensorError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvalidShapeError(
        std::string message, const std::source_location& location = std::source_location::current())
        : TensorError(std::move(message), Code::InvalidShape, location) {}

    /**
     * @brief Construct with invalid shape
     *
     * @param shape Invalid shape
     * @param reason Reason for invalidity
     * @param location Source location
     */
    template <typename Container>
    InvalidShapeError(const Container& shape, std::string reason,
                      const std::source_location& location = std::source_location::current())
        : TensorError(format_shape_error(shape, std::move(reason)), Code::InvalidShape, location) {}

private:
    template <typename Container>
    static std::string format_shape_error(const Container& shape, std::string reason) {
        std::string shape_str = "[";
        bool first = true;

        for (const auto& dim : shape) {
            if (!first) {
                shape_str += ", ";
            }

            shape_str += std::to_string(dim);
            first = false;
        }

        shape_str = "]";
        return fmt::format("Invalid shape {}: {}", shape_str, reason);
    }
};

/**
 * @brief Shape mismatch error
 */
class BREZEL_API ShapeMismatchError : public TensorError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit ShapeMismatchError(
        std::string message, const std::source_location& location = std::source_location::current())
        : TensorError(std::move(message), Code::ShapeMismatch, location) {}

    /**
     * @brief Construct with mismatched shapes
     *
     * @param shape1 First shape
     * @param shape2 Second shape
     * @param operation Operation description
     * @param location Source location
     */
    template <typename Container>
    ShapeMismatchError(const Container& shape1, const Container& shape2, std::string operation,
                       const std::source_location& location = std::source_location::current())
        : TensorError(format_shape_mismatch(shape1, shape2, std::move(operation)),
                      Code::ShapeMismatch, location) {}

private:
    template <typename Container>
    static std::string format_shape_mismatch(const Container& shape1, const Container& shape2,
                                             std::string operation) {
        std::string shape1_str = "[";
        bool first = true;

        for (const auto& dim : shape1) {
            if (!first)
                shape1_str += ", ";
            shape1_str += std::to_string(dim);
            first = false;
        }
        shape1_str += "]";

        std::string shape2_str = "[";
        first = true;
        
        for (const auto& dim : shape2) {
            if (!first)
                shape2_str += ", ";
            shape2_str += std::to_string(dim);
            first = false;
        }
        shape2_str += "]";

        return fmt::format("Shape mismatch for {}: {} vs {}", operation, shape1_str, shape2_str);
    }
};

/**
 * @brief Dimension mismatch error
 */
class BREZEL_API DimensionMismatchError : public TensorError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DimensionMismatchError(
        std::string message, const std::source_location& location = std::source_location::current())
        : TensorError(std::move(message), Code::DimensionMismatch, location) {}

    /**
     * @brief Construct with mismatched dimensions
     *
     * @param expected Expected dimension count
     * @param actual Actual dimension count
     * @param operation Operation description
     * @param location Source location
     */
    DimensionMismatchError(size_t expected, size_t actual, std::string operation,
                           const std::source_location& location = std::source_location::current())
        : TensorError(fmt::format("Dimension mismatch for {}: expected {} dimensions, got {}",
                                  operation, expected, actual),
                      Code::DimensionMismatch, location) {}
};

/**
 * @brief Invalid data type error
 */
class BREZEL_API InvalidDtypeError : public TensorError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit InvalidDtypeError(
        std::string message, const std::source_location& location = std::source_location::current())
        : TensorError(std::move(message), Code::InvalidDtype, location) {}

    /**
     * @brief Construct with invalid dtype
     *
     * @param dtype Invalid dtype
     * @param operation Operation description
     * @param location Source location
     */
    InvalidDtypeError(std::string_view dtype, std::string operation,
                      const std::source_location& location = std::source_location::current())
        : TensorError(fmt::format("Invalid data type '{}' for {}", dtype, operation),
                      Code::InvalidDtype, location) {}
};

/**
 * @brief Data type mismatch error
 */
class BREZEL_API DtypeMismatchError : public TensorError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DtypeMismatchError(
        std::string message, const std::source_location& location = std::source_location::current())
        : TensorError(std::move(message), Code::DtypeMismatch, location) {}

    /**
     * @brief Construct with mismatched dtypes
     *
     * @param dtype1 First dtype
     * @param dtype2 Second dtype
     * @param operation Operation description
     * @param location Source location
     */
    DtypeMismatchError(std::string_view dtype1, std::string_view dtype2, std::string operation,
                       const std::source_location& location = std::source_location::current())
        : TensorError(fmt::format("Data type mismatch for {}: {} vs {}", operation, dtype1, dtype2),
                      Code::DtypeMismatch, location) {}
};

/**
 * @brief Index out of bounds error
 */
class BREZEL_API IndexOutOfBoundsError : public TensorError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit IndexOutOfBoundsError(
        std::string message, const std::source_location& location = std::source_location::current())
        : TensorError(std::move(message), Code::IndexOutOfBounds, location) {}

    /**
     * @brief Construct with index and bounds information
     *
     * @param index Actual index
     * @param bounds Maximum allowed value
     * @param dimension_name Optional dimension name
     * @param location Source location
     */
    IndexOutOfBoundsError(size_t index, size_t bounds, std::string_view dimension_name = "",
                          const std::source_location& location = std::source_location::current())
        : TensorError(dimension_name.empty()
                          ? fmt::format("Index {} out of bounds [0, {})", index, bounds)
                          : fmt::format("Index {} out of bounds [0, {}) for dimension '{}'", index,
                                        bounds, dimension_name),
                      Code::IndexOutOfBounds, location) {}
};
}  // namespace brezel::error