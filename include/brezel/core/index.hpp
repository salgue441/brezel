/**
 * @file index.hpp
 * @author Carlos Salguero
 * @brief Defines indexing utilities for tensors
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains classes and utilities for advanced indexing and slicing
 * of tensors, supporting operations similar to NumPy/PyTorch.
 */

#pragma once

#include <cassert>
#include <cstdint>
#include <optional>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

#include <brezel/core/exception.hpp>

namespace brezel {

/**
 * @brief Represents the "..." operator in slice notation
 *
 * This class represents the ellipsis notation (equivalent to Python's "...").
 */
class Ellipsis {
public:
    /**
     * @brief Convert to string representation
     *
     * @return std::string String representation
     */
    std::string to_string() const { return "..."; }

    /**
     * @brief Equality operator
     *
     * @param other Ellipsis to compare with
     * @return bool Always true (all Ellipsis objects are equal)
     */
    bool operator==(const Ellipsis&) const { return true; }

    /**
     * @brief Inequality operator
     *
     * @param other Ellipsis to compare with
     * @return bool Always false (all Ellipsis objects are equal)
     */
    bool operator!=(const Ellipsis&) const { return false; }
};

/**
 * @brief Represents a slice index
 *
 * This class represents a slice index (equivalent to Python's "start:stop:step").
 */
class Slice {
public:
    using IndexType = int64_t;

    /**
     * @brief Default constructor (creates a full slice)
     */
    Slice() : m_start(std::nullopt), m_stop(std::nullopt), m_step(1) {}

    /**
     * @brief Construct with stop (equivalent to ":stop")
     *
     * @param stop Stop index (exclusive)
     */
    explicit Slice(IndexType stop) : m_start(0), m_stop(stop), m_step(1) {}

    /**
     * @brief Construct with start and stop (equivalent to "start:stop")
     *
     * @param start Start index (inclusive)
     * @param stop Stop index (exclusive)
     */
    Slice(IndexType start, IndexType stop) : m_start(start), m_stop(stop), m_step(1) {}

    /**
     * @brief Construct with start, stop, and step (equivalent to "start:stop:step")
     *
     * @param start Start index (inclusive)
     * @param stop Stop index (exclusive)
     * @param step Step size
     * @throws ValueError If step is zero
     */
    Slice(IndexType start, IndexType stop, IndexType step)
        : m_start(start), m_stop(stop), m_step(step) {
        if (step == 0) {
            throw ValueError("Slice step cannot be zero");
        }
    }

    /**
     * @brief Construct with optional indices
     *
     * @param start Start index (inclusive)
     * @param stop Stop index (exclusive)
     * @param step Step size
     * @throws ValueError If step is zero
     */
    Slice(std::optional<IndexType> start, std::optional<IndexType> stop, IndexType step = 1)
        : m_start(start), m_stop(stop), m_step(step) {
        if (step == 0) {
            throw ValueError("Slice step cannot be zero");
        }
    }

    /**
     * @brief Get the start index
     *
     * @return const std::optional<IndexType>& Start index
     */
    const std::optional<IndexType>& start() const { return m_start; }

    /**
     * @brief Get the stop index
     *
     * @return const std::optional<IndexType>& Stop index
     */
    const std::optional<IndexType>& stop() const { return m_stop; }

    /**
     * @brief Get the step size
     *
     * @return IndexType Step size
     */
    IndexType step() const { return m_step; }

    /**
     * @brief Convert to string representation
     *
     * @return std::string String representation
     */
    std::string to_string() const {
        std::string result;
        if (m_start) {
            result += std::to_string(*m_start);
        }

        result += ":";
        if (m_stop) {
            result += std::to_string(*m_stop);
        }

        if (m_step != 1) {
            result += ":" + std::to_string(m_step);
        }

        return result;
    }

    /**
     * @brief Compute the indices covered by this slice
     *
     * @param size Size of the dimension being sliced
     * @return std::vector<IndexType> Vector of indices
     */
    std::vector<IndexType> indices(IndexType size) const {
        IndexType normalized_start =
            m_start ? normalize_index(*m_start, size) : (m_step > 0 ? 0 : size - 1);
            
        IndexType normalized_stop =
            m_stop ? normalize_index(*m_stop, size) : (m_step > 0 ? size : -1);

        std::vector<IndexType> result;
        if (m_step > 0) {
            for (IndexType i = normalized_start; i < normalized_stop; i += m_step) {
                result.push_back(i);
            }
        } else {
            for (IndexType i = normalized_start; i > normalized_stop; i += m_step) {
                result.push_back(i);
            }
        }

        return result;
    }

    /**
     * @brief Compute the number of elements in the slice
     *
     * @param dim_size Size of the dimension being sliced
     * @return IndexType Number of elements
     */
    IndexType size(IndexType dim_size) const {
        IndexType normalized_start =
            m_start ? normalize_index(*m_start, dim_size) : (m_step > 0 ? 0 : dim_size - 1);
        IndexType normalized_stop =
            m_stop ? normalize_index(*m_stop, dim_size) : (m_step > 0 ? dim_size : -1);

        if (m_step > 0) {
            if (normalized_start >= normalized_stop) {
                return 0;
            }

            return (normalized_stop - normalized_start + m_step - 1) / m_step;
        } else {
            if (normalized_start <= normalized_stop) {
                return 0;
            }

            return (normalized_start - normalized_stop - m_step - 1) / -m_step;
        }
    }

    /**
     * @brief Equality operator
     *
     * @param other Slice to compare with
     * @return bool Whether the slices are equal
     */
    bool operator==(const Slice& other) const {
        return m_start == other.m_start && m_stop == other.m_stop && m_step == other.m_step;
    }

    /**
     * @brief Inequality operator
     *
     * @param other Slice to compare with
     * @return bool Whether the slices are not equal
     */
    bool operator!=(const Slice& other) const { return !(*this == other); }

private:
    std::optional<IndexType> m_start;  ///< Start index (inclusive)
    std::optional<IndexType> m_stop;   ///< Stop index (exclusive)
    IndexType m_step;                  ///< Step size

    /**
     * @brief Normalize a negative index
     *
     * @param idx Index to normalize
     * @param size Size of the dimension
     * @return IndexType Normalized index
     */
    static IndexType normalize_index(IndexType idx, IndexType size) {
        if (idx < 0) {
            idx += size;
        }

        return std::max(IndexType(0), std::min(idx, size));
    }
};

/**
 * @brief Represents a new axis (equivalent to Python's None in indexing)
 *
 * This class represents a new axis, which adds a dimension of size 1.
 */
class NewAxis {
public:
    /**
     * @brief Convert to string representation
     *
     * @return std::string String representation
     */
    std::string to_string() const { return "None"; }

    /**
     * @brief Equality operator
     *
     * @param other NewAxis to compare with
     * @return bool Always true (all NewAxis objects are equal)
     */
    bool operator==(const NewAxis&) const { return true; }

    /**
     * @brief Inequality operator
     *
     * @param other NewAxis to compare with
     * @return bool Always false (all NewAxis objects are equal)
     */
    bool operator!=(const NewAxis&) const { return false; }
};

/**
 * @brief Represents a boolean mask for indexing
 *
 * This class represents a boolean mask for advanced indexing.
 */
class BooleanMask {
public:
    using IndexType = int64_t;

    /**
     * @brief Construct from a vector of booleans
     *
     * @param mask Vector of booleans
     */
    explicit BooleanMask(const std::vector<bool>& mask) : m_mask(mask) {}

    /**
     * @brief Get the mask
     *
     * @return const std::vector<bool>& Mask
     */
    const std::vector<bool>& mask() const { return m_mask; }

    /**
     * @brief Convert to string representation
     *
     * @return std::string String representation
     */
    std::string to_string() const {
        std::string result = "[";
        for (size_t i = 0; i < m_mask.size(); ++i) {
            if (i > 0) {
                result += ",";
            }

            result += m_mask[i] ? "True" : "False";
        }
        
        result += "]";
        return result;
    }

    /**
     * @brief Get the indices where the mask is true
     *
     * @return std::vector<IndexType> Vector of indices
     */
    std::vector<IndexType> indices() const {
        std::vector<IndexType> result;
        for (size_t i = 0; i < m_mask.size(); ++i) {
            if (m_mask[i]) {
                result.push_back(static_cast<IndexType>(i));
            }
        }

        return result;
    }

    /**
     * @brief Get the size of the mask
     *
     * @return size_t Size of the mask
     */
    size_t size() const { return m_mask.size(); }

    /**
     * @brief Equality operator
     *
     * @param other BooleanMask to compare with
     * @return bool Whether the masks are equal
     */
    bool operator==(const BooleanMask& other) const { return m_mask == other.m_mask; }

    /**
     * @brief Inequality operator
     *
     * @param other BooleanMask to compare with
     * @return bool Whether the masks are not equal
     */
    bool operator!=(const BooleanMask& other) const { return !(*this == other); }

private:
    std::vector<bool> m_mask;  ///< Boolean mask
};

/**
 * @brief Type for tensor indices
 *
 * This variant type represents the different kinds of indices that can be used
 * for tensor indexing.
 */
using Index = std::variant<int64_t,              // Single index
                           Slice,                // Slice index (start:stop:step)
                           Ellipsis,             // Ellipsis index (...)
                           NewAxis,              // New axis index (None)
                           BooleanMask,          // Boolean mask
                           std::vector<int64_t>  // Integer array
                           >;

/**
 * @brief Convert an index to a string representation
 *
 * @param idx Index to convert
 * @return std::string String representation
 */
inline std::string index_to_string(const Index& idx) {
    return std::visit(
        [](const auto& v) -> std::string {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, int64_t>) {
                return std::to_string(v);
            } else if constexpr (std::is_same_v<T, Slice> || std::is_same_v<T, Ellipsis> ||
                                 std::is_same_v<T, NewAxis> || std::is_same_v<T, BooleanMask>) {
                return v.to_string();
            } else if constexpr (std::is_same_v<T, std::vector<int64_t>>) {
                std::string result = "[";
                for (size_t i = 0; i < v.size(); ++i) {
                    if (i > 0) {
                        result += ",";
                    }

                    result += std::to_string(v[i]);
                }

                result += "]";
                return result;
            } else {
                return "unknown";
            }
        },
        idx);
}

/**
 * @brief Check if an index is a full slice
 *
 * @param idx Index to check
 * @return bool Whether the index is a full slice
 */
inline bool is_full_slice(const Index& idx) {
    if (std::holds_alternative<Slice>(idx)) {
        const auto& slice = std::get<Slice>(idx);
        return !slice.start() && !slice.stop() && slice.step() == 1;
    }

    return false;
}

/**
 * @brief Check if an index will create a view without copying
 *
 * @param idx Index to check
 * @return bool Whether the index will create a view
 */
inline bool is_view_index(const Index& idx) {
    return std::holds_alternative<Slice>(idx) || std::holds_alternative<Ellipsis>(idx) ||
           std::holds_alternative<NewAxis>(idx) || is_full_slice(idx);
}

/**
 * @brief Type for tensor index tuple
 *
 * This type represents a tuple of indices for tensor indexing.
 */
using IndexTuple = std::vector<Index>;

/**
 * @brief Convert an index tuple to a string representation
 *
 * @param indices Index tuple to convert
 * @return std::string String representation
 */
inline std::string index_tuple_to_string(const IndexTuple& indices) {
    std::string result = "(";
    for (size_t i = 0; i < indices.size(); ++i) {
        if (i > 0) {
            result += ", ";
        }

        result += index_to_string(indices[i]);
    }
    
    result += ")";
    return result;
}

/**
 * @brief Create a full slice index
 *
 * @return Slice Full slice
 */
inline Slice full_slice() {
    return Slice();
}

/**
 * @brief Create an ellipsis index
 *
 * @return Ellipsis Ellipsis
 */
inline Ellipsis ellipsis() {
    return Ellipsis();
}

/**
 * @brief Create a new axis index
 *
 * @return NewAxis New axis
 */
inline NewAxis new_axis() {
    return NewAxis();
}

/**
 * @brief Normalization result for an index tuple
 *
 * This struct represents the result of normalizing an index tuple.
 */
struct NormalizedIndexTuple {
    IndexTuple indices;                 ///< Normalized indices
    bool is_view = false;               ///< Whether the indexing creates a view
    bool is_advanced_indexing = false;  ///< Whether advanced indexing is used
    size_t num_ellipsis = 0;            ///< Number of ellipsis indices
    size_t num_new_axes = 0;            ///< Number of new axis indices
};

/**
 * @brief Normalize an index tuple for a tensor with the given shape
 *
 * @param indices Index tuple to normalize
 * @param ndim Number of dimensions in the tensor
 * @return NormalizedIndexTuple Normalized index tuple
 * @throws IndexError If the index tuple is invalid
 */
NormalizedIndexTuple normalize_index_tuple(const IndexTuple& indices, size_t ndim);

/**
 * @brief Compute the output shape of an indexing operation
 *
 * @param shape Input shape
 * @param indices Index tuple
 * @return std::vector<int64_t> Output shape
 * @throws IndexError If the index tuple is invalid
 */
std::vector<int64_t> compute_indexed_shape(const std::vector<int64_t>& shape,
                                           const IndexTuple& indices);

}  // namespace brezel