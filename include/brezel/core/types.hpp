#pragma once

/**
 * @file types.hpp
 * @brief Core type definitions for the Brezel tensor framework
 * @version 0.1
 * @date 2025-04-18
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains fundamental type definitions used throughout the
 * Brezel framework, including shape, strides, data types, and device types.
 */

#include <array>
#include <cstdint>
#include <initializer_list>
#include <ostream>
#include <string>
#include <vector>

namespace brezel {
/**
 * @brief Index type used for tensor dimensions and indexing
 *
 * Using int64_t allows addressing large tensors and avoids
 * the need for casting between index types
 */
using index_t = int64_t;

/**
 * @brief Shape type representing tensor dimensions
 *
 * A shape is a vector of indices defining the size of each dimension.
 * For example, a 2x3x4 tensor has a shape of {2, 3, 4}
 */
using shape_t = std::vector<index_t>;

/**
 * @brief Stride type representing memory layout
 *
 * Strides define the memory step size along each dimension.
 * For example, a 2x3 row-major tensor has strides of {3, 1}.
 */
using stride_t = std::vector<index_t>;

/**
 * @brief Enumeration for supported data types
 */
enum class dtype_t {
    Float32,   ///< 32-bit floating point (float)
    Float64,   ///< 64-bit floating point (double)
    Int32,     ///< 32-bit signed integer (int32_t)
    Int64,     ///< 64-bit signed integer (int64_t)
    Uint8,     ///< 8-bit unsigned integer (uint8_t)
    Bool,      ///< Boolean,
    Complex64  ///< Complex number with float32 real and imaginary parts
};

/**
 * @brief Enumeration of supported device types
 */
enum class DeviceType {
    CPU,  ///< CPU device
    CUDA  ///< CUDA device
};

/**
 * @brief Memory layout ordering for tensors
 */
enum class MemoryLayout {
    RowMajor,    ///< Row-major (C-style) layout
    ColumnMajor  ///< Column-major (Fortran-style) layout
};

/**
 * @brief Convert dtype to string representation
 *
 * @param dtype Data type to convert
 * @return std::string String representation of the data type
 */
std::string dtype_to_string(dtype_t dtype);

/**
 * @brief Get size in bytes for a given data type
 *
 * @param dtype Data type
 * @return size_t Size in bytes
 */
size_t dtype_size(dtype_t dtype);

/**
 * @brief Get the C++ type name for a given data type
 *
 * @param dtype Data type
 * @return std::string C++ type name
 */
std::string dtype_to_type_name(dtype_t dtype);

/**
 * @brief Convert device type to string representation
 *
 * @param device_type Device type to convert
 * @return std::string String representation of the device type
 */
std::string device_type_to_string(DeviceType device_type);

/**
 * @brief Check if two shapes are equal
 *
 * @param lhs First shape
 * @param rhs Second shape
 * @return bool True if shapes are equal
 */
bool shapes_equal(const shape_t& lhs, const shape_t& rhs);

/**
 * @brief Check if two strides are equal
 *
 * @param lhs First stride
 * @param rhs Second stride
 * @return bool True if strides are equal
 */
bool strides_equal(const stride_t& lhs, const stride_t& rhs);

/**
 * @brief Calculate the total number of elements from a shape
 *
 * @param shape Tensor shape
 * @return size_t Total number of elements
 */
size_t calculate_size(const shape_t& shape);

/**
 * @brief Calculate strides for a given shape using the specified memory layout
 *
 * @param shape Tensor shape
 * @param layout Memory layout (default: RowMajor)
 * @return stride_t Calculated strides
 */
stride_t calculate_strides(const shape_t& shape, MemoryLayout layout = MemoryLayout::RowMajor);

/**
 * @brief Convert a linear index to multi-dimensional indices
 *
 * @param linear_index Linear (flat) index
 * @param shape Tensor shape
 * @param strides Tensor strides
 * @return std::vector<index_t> Multi-dimensional indices
 */
std::vector<index_t> linear_index_to_indices(size_t linear_index, const shape_t& shape,
                                             const stride_t& strides);

/**
 * @brief Convert multi-dimensional indices to a linear index
 *
 * @param indices Multi-dimensional indices
 * @param strides Tensor strides
 * @return size_t Linear (flat) index
 */
size_t indices_to_linear_index(const std::vector<index_t>& indices, const stride_t& strides);

/**
 * @brief Check if a shape is valid (all dimensions > 0)
 *
 * @param shape Shape to check
 * @return bool True if shape is valid
 */
bool is_valid_shape(const shape_t& shape);

/**
 * @brief Check if a tensor with the given shape and strides is contiguous
 * in memory
 *
 * @param shape Tensor shape
 * @param strides Tensor strides
 * @return bool True if tensor is contiguous
 */
bool is_contiguous(const shape_t& shape, const stride_t& strides);

/**
 * @brief Create a shape from an initializer list
 *
 * @param dims Dimensions
 * @return shape_t Shape object
 */
inline shape_t make_shape(std::initializer_list<index_t> dims) {
    return shape_t(dims);
}

/**
 * @brief Variadic function to create a shape
 *
 * @tparam Args Argument types
 * @param first First dimension
 * @param args Additional dimensions
 * @return shape_t Shape object
 */
template <typename... Args>
inline shape_t make_shape(index_t first, Args... args) {
    shape_t result = {first};
    shape_t rest = make_shape({static_cast<index_t>(args)...});

    result.insert(result.end(), rest.begin(), rest.end());
    return result;
}

/**
 * @brief Base case for the variadic makeShape function
 *
 * @return shape_t Empty shape
 */
inline shape_t make_shape() {
    return {};
}

/**
 * @brief Template function to write a vector of indices to an output stream
 *
 * @tparam VecT Vector type (should be a vector of index_t)
 * @param os Output stream
 * @param vec Vector to write
 * @return std::ostream& Modified output stream
 */
template <typename VecT>
std::ostream& print_index_vector(std::ostream& os, const VecT& vec) {
    os << "[";

    for (size_t i = 0; i < vec.size(); ++i) {
        os << vec[i];
        if (i < vec.size() - 1) {
            os << ", ";
        }
    }

    os << "]";
    return os;
}

/**
 * @brief Write a shape to an output stream
 *
 * @param os Output stream
 * @param shape Shape to write
 * @return std::ostream& Modified output stream
 */
std::ostream& operator<<(std::ostream& os, const shape_t& shape);

void print_strides(std::ostream& os, const stride_t& strides);

}  // namespace brezel