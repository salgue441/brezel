/**
 * @file shape.hpp
 * @author Carlos Salguero
 * @brief Defines the Shape class for representing tensor dimensions
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains the Shape class, which represents the dimensions of a
 * tensor and provides utilities for dimension manipulation and checking
 */

#pragma once

#include <algorithm>
#include <cstdint>
#include <initializer_list>
#include <numeric>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace brezel {
/**
 * @brief Class representing the shape of a tensor
 */
class Shape {
public:
    using SizeType = int64_t;
    using DimVector = std::vector<SizeType>;
    using Iterator = DimVector::iterator;
    using ConstIterator = DimVector::const_iterator;

    /**
     * @brief Default constructor (creates empty shape)
     */
    Shape() = default;

    /**
     * @brief Construct from an initializer list
     *
     * @param dims Initializer list of dimensions
     */
    Shape(std::initializer_list<SizeType> dims) : m_dims(dims) { validate(); }

    /**
     * @brief Construct from a vector of dimensions
     *
     * @param dims Vector of dimensions
     */
    explicit Shape(const DimVector& dims) : m_dims(dims) { validate(); }

    /**
     * @brief Construct from iterators
     *
     * @tparam InputIt Iterator type
     * @param first Iterator to the first dimension
     * @param last Iterator past the last dimension
     */
    template <typename InputIt>
    Shape(InputIt first, InputIt last) : m_dims(first, last) {
        validate();
    }

    /**
     * @brief Get the number of dimensions
     *
     * @return size_t Number of dimensions
     */
    size_t size() const noexcept { return m_dims.size(); }

    /**
     * @brief Check if the shape is empty
     *
     * @return bool Whether the shape is empty
     */
    bool empty() const noexcept { return m_dims.empty(); }

    /**
     * @brief Get the total number of elements
     *
     * @return SizeType Total number of elements
     */
    SizeType numel() const noexcept {
        return empty() ? 0
                       : std::accumulate(m_dims.begin(), m_dims.end(), SizeType(1),
                                         std::multiplies<SizeType>());
    }

    /**
     * @brief Access a dimension by index
     *
     * @param index Dimension index
     * @return SizeType& Reference to the dimension
     * @throws std::out_of_range If the index is out of range
     */
    SizeType& operator[](size_t index) { return m_dims.at(index); }

    /**
     * @brief Access a dimension by index (const version)
     *
     * @param index Dimension index
     * @return const SizeType& Reference to the dimension
     * @throws std::out_of_range If the index is out of range
     */
    const SizeType& operator[](size_t index) const { return m_dims.at(index); }

    /**
     * @brief Get an iterator to the first dimension
     *
     * @return Iterator Iterator to the first dimension
     */
    Iterator begin() noexcept { return m_dims.begin(); }

    /**
     * @brief Get an iterator past the last dimension
     *
     * @return Iterator Iterator past the last dimension
     */
    Iterator end() noexcept { return m_dims.end(); }

    /**
     * @brief Get a const iterator to the first dimension
     *
     * @return ConstIterator Const iterator to the first dimension
     */
    ConstIterator begin() const noexcept { return m_dims.begin(); }

    /**
     * @brief Get a const iterator past the last dimension
     *
     * @return ConstIterator Const iterator past the last dimension
     */
    ConstIterator end() const noexcept { return m_dims.end(); }

    /**
     * @brief Get a const iterator to the first dimension
     *
     * @return ConstIterator Const iterator to the first dimension
     */
    ConstIterator cbegin() const noexcept { return m_dims.cbegin(); }

    /**
     * @brief Get a const iterator past the last dimension
     *
     * @return ConstIterator Const iterator past the last dimension
     */
    ConstIterator cend() const noexcept { return m_dims.cend(); }

    /**
     * @brief Equality comparison
     *
     * @param other Shape to compare with
     * @return bool Whether the shapes are equal
     */
    bool operator==(const Shape& other) const { return m_dims == other.m_dims; }

    /**
     * @brief Inequality comparison
     *
     * @param other Shape to compare with
     * @return bool Whether the shapes are not equal
     */
    bool operator!=(const Shape& other) const { return !(*this == other); }

    /**
     * @brief Convert to string representation
     *
     * @return std::string String representation
     */
    std::string to_string() const {
        if (empty()) {
            return "[]";
        }

        std::ostringstream oss;

        oss << "[";
        for (size_t i = 0; i < m_dims.size(); ++i) {
            if (i > 0) {
                oss << ", ";
            }

            oss << m_dims[i];
        }

        oss << "]";
        return oss.str();
    }

    /**
     * @brief Reshape to match the target shape
     *
     * The new shape must have the same number of elements as the original
     * shape.
     *
     * @param target_shape Target shape
     * @return Shape Reshaped shape
     * @throws std::invalid_argument If the shapes have different numbers
     *         of elements
     */
    Shape reshape(const Shape& target_shape) const {
        SizeType original_numel = numel();
        SizeType target_numel = target_shape.numel();

        if (original_numel != target_numel) {
            throw std::invalid_argument(
                "Cannot reshape tensor of shape " + to_string() + " to shape " +
                target_shape.to_string() + ": different number of elements (" +
                std::to_string(original_numel) + " vs " + std::to_string(target_numel) + ")");
        }

        return target_shape;
    }

    /**
     * @brief Compute the strides for this shape
     *
     * Strides represent the number of elements to skip to move one position
     * along each dimension.
     *
     * @param row_major Whether to use row-major (C-style) ordering
     * @return DimVector Strides for each dimension
     */
    DimVector strides(bool row_major = true) const {
        if (empty()) {
            return {};
        }

        DimVector result(m_dims.size());
        if (row_major) {
            result.back() = 1;
            for (int i = static_cast<int>(m_dims.size()) - 2; i >= 0; --i) {
                result[i] = result[i + 1] * m_dims[i + 1];
            }
        } else {
            result.front() = 1;
            for (size_t i = 1; i < m_dims.size(); ++i) {
                result[i] = result[i - 1] * m_dims[i - 1];
            }
        }
        return result;
    }

    /**
     * @brief Check if two shapes are broadcastable
     *
     * Shapes are broadcastable if for each dimension pair, either
     * the dimensions are equal or one of them is 1.
     *
     * @param other Shape to check against
     * @return bool Whether the shapes are broadcastable
     */
    bool is_broadcastable_with(const Shape& other) const {
        const auto& shape1 = m_dims;
        const auto& shape2 = other.m_dims;
        const size_t ndim1 = shape1.size();
        const size_t ndim2 = shape2.size();
        const size_t min_ndim = std::min(ndim1, ndim2);

        for (size_t i = 0; i < min_ndim; ++i) {
            const SizeType dim1 = shape1[ndim1 - 1 - i];
            const SizeType dim2 = shape2[ndim2 - 1 - i];

            if (dim1 != dim2 && dim1 != 1 && dim2 != 1) {
                return false;
            }
        }

        return true;
    }

    /**
     * @brief Compute the result shape of broadcasting two shapes
     *
     * @param other Shape to broadcast with
     * @return Shape Resulting shape after broadcasting
     * @throws std::invalid_argument If the shapes are not broadcastable
     */
    Shape broadcast_with(const Shape& other) const {
        if (!is_broadcastable_with(other)) {
            throw std::invalid_argument("Cannot broadcast shapes " + to_string() + " and " +
                                        other.to_string());
        }

        const auto& shape1 = m_dims;
        const auto& shape2 = other.m_dims;
        const size_t ndim1 = shape1.size();
        const size_t ndim2 = shape2.size();
        const size_t result_ndim = std::max(ndim1, ndim2);

        DimVector result_shape(result_ndim);

        for (size_t i = 0; i < result_ndim; ++i) {
            const size_t pos = result_ndim - 1 - i;
            const SizeType dim1 = (i < ndim1) ? shape1[ndim1 - 1 - i] : 1;
            const SizeType dim2 = (i < ndim2) ? shape2[ndim2 - 1 - i] : 1;

            result_shape[pos] = std::max(dim1, dim2);
        }

        return Shape(result_shape);
    }

    /**
     * @brief Create a shape with a subset of dimensions
     *
     * @param start_dim Start dimension index (inclusive)
     * @param end_dim End dimension index (exclusive)
     * @return Shape Shape with the specified dimensions
     * @throws std::out_of_range If indices are out of range
     */
    Shape slice(size_t start_dim, size_t end_dim) const {
        if (start_dim > end_dim || end_dim > m_dims.size()) {
            throw std::out_of_range("Invalid dimension range");
        }

        return Shape(m_dims.begin() + start_dim, m_dims.begin() + end_dim);
    }

    /**
     * @brief Insert a dimension of size 1 at the specified index
     *
     * @param index Index at which to insert the dimension
     * @return Shape Shape with the new dimension
     * @throws std::out_of_range If the index is out of range
     */
    Shape unsqueeze(size_t index) const {
        if (index > m_dims.size()) {
            throw std::out_of_range("Index out of range for unsqueeze");
        }

        DimVector new_dims = m_dims;
        new_dims.insert(new_dims.begin() + index, 1);

        return Shape(new_dims);
    }

    /**
     * @brief Remove a dimension of size 1
     *
     * @param index Index of the dimension to remove
     * @return Shape Shape with the dimension removed
     * @throws std::out_of_range If the index is out of range
     * @throws std::invalid_argument If the dimension at index is not 1
     */
    Shape squeeze(size_t index) const {
        if (index >= m_dims.size()) {
            throw std::out_of_range("Index out of range for squeeze");
        }

        if (m_dims[index] != 1) {
            throw std::invalid_argument("Can only squeeze dimensions of size 1");
        }

        DimVector new_dims = m_dims;
        new_dims.erase(new_dims.begin() + index);

        return Shape(new_dims);
    }

    /**
     * @brief Remove all dimensions of size 1
     *
     * @return Shape Shape with all dimensions of size 1 removed
     */
    Shape squeeze_all() const {
        DimVector new_dims;
        for (const auto& dim : m_dims) {
            if (dim != 1) {
                new_dims.push_back(dim);
            }
        }

        return new_dims.empty() ? Shape({1}) : Shape(new_dims);
    }

    /**
     * @brief Permute the dimensions according to the given indices
     *
     * @param indices Indices defining the new order of dimensions
     * @return Shape Shape with permuted dimensions
     * @throws std::invalid_argument If the number of indices doesn't match
     * @throws std::out_of_range If any index is out of range
     */

    Shape permute(const std::vector<size_t>& indices) const {
        if (indices.size() != m_dims.size()) {
            throw std::invalid_argument(
                "Number of permutation indices must match shape dimensions");
        }

        for (const auto& idx : indices) {
            if (idx >= m_dims.size()) {
                throw std::out_of_range("Permutation index out of range");
            }
        }

        std::vector<size_t> sorted_indices = indices;
        std::sort(sorted_indices.begin(), sorted_indices.end());
        if (std::adjacent_find(sorted_indices.begin(), sorted_indices.end()) !=
            sorted_indices.end()) {
            throw std::invalid_argument("Permutation indices must be unique");
        }

        DimVector new_dims(m_dims.size());
        for (size_t i = 0; i < m_dims.size(); ++i) {
            new_dims[i] = m_dims[indices[i]];
        }

        return Shape(new_dims);
    }

    /**
     * @brief Transpose the shape (swap the last two dimensions)
     *
     * @return Shape Transposed shape
     * @throws std::invalid_argument If the shape has fewer than 2 dimensions
     */
    Shape transpose() const {
        if (m_dims.size() < 2) {
            throw std::invalid_argument("Transpose requires at least 2 dimensions");
        }

        std::vector<size_t> indices(m_dims.size());
        std::iota(indices.begin(), indices.end(), 0);
        std::swap(indices[m_dims.size() - 2], indices[m_dims.size() - 1]);

        return permute(indices);
    }

    /**
     * @brief Get the underlying dimensions vector
     *
     * @return const DimVector& Vector of dimensions
     */
    const DimVector& dims() const noexcept { return m_dims; }

private:
    DimVector m_dims;

    /**
     * @brief Validate dimensions
     *
     * @throws std::invalid_argument If any dimension is negative
     */
    void validate() {
        if (std::any_of(m_dims.begin(), m_dims.end(), [](SizeType dim) { return dim < 0; })) {
            throw std::invalid_argument("Shape dimensions must be non-negative");
        }
    }
};

/**
 * @brief Output stream operator for Shape
 *
 * @param os Output stream
 * @param shape Shape to output
 * @return std::ostream& Reference to the output stream
 */
inline std::ostream& operator<<(std::ostream& os, const Shape& shape) {
    return os << shape.to_string();
}
}  // namespace brezel