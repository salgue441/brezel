#pragma once

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>

#include <brezel/core/types.hpp>
#include <brezel/utils/exception.hpp>
#include <brezel/utils/macros.hpp>

#include <algorithm>
#include <array>
#include <cmath>
#include <concepts>
#include <execution>
#include <functional>
#include <future>
#include <mutex>
#include <numeric>
#include <optional>
#include <random>
#include <span>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

namespace brezel {
namespace utils {

/**
 * @brief Check if a shape is valid (all dimensions > 0)
 *
 * @param shape The shape to check
 * @return true If the shape is valid
 * @return false If any dimension is <= 0
 */
inline bool is_valid_shape(const shape_t& shape) {
    return std::all_of(shape.begin(), shape.end(), [](index_t dim) { return dim > 0; });
}

/**
 * @brief Compute the total number of elements in a tensor with the given shape
 *
 * @param shape The shape of the tensor
 * @return size_t Total number of elements
 */
inline size_t compute_size(const shape_t& shape) {
    if (shape.empty()) {
        return 0;
    }

    return std::accumulate(shape.begin(), shape.end(), static_cast<size_t>(1),
                           std::multiplies<size_t>());
}

/**
 * @brief Compute strides for a tensor with the given shape
 *
 * Strides represent the number of elements to skip to move by 1 in each dimension.
 * For a contiguous tensor, this is computed in row-major order (C-style).
 *
 * @param shape The shape of the tensor
 * @return stride_t The computed strides
 */
inline stride_t compute_strides(const shape_t& shape) {
    stride_t strides(shape.size());
    if (shape.empty()) {
        return strides;
    }

    index_t stride = 1;
    for (int i = static_cast<int>(shape.size()) - 1; i >= 0; --i) {
        strides[i] = stride;
        stride *= shape[i];
    }

    return strides;
}

/**
 * @brief Compute the linear (flattened) index from multi-dimensional indices
 *
 * @param indices Multi-dimensional indices
 * @param strides Strides of the tensor
 * @return size_t Linear index
 */
inline size_t compute_linear_index(const std::vector<index_t>& indices, const stride_t& strides) {
    BREZEL_ASSERT(indices.size() == strides.size(), "Indices and strides must have the same size");

    size_t linear_index = 0;
    for (size_t i = 0; i < indices.size(); ++i) {
        linear_index += indices[i] * strides[i];
    }

    return linear_index;
}

/**
 * @brief Convert a linear index to multi-dimensional indices
 *
 * @param linear_index Linear index
 * @param shape Shape of the tensor
 * @param strides Strides of the tensor
 * @return std::vector<index_t> Multi-dimensional indices
 */
inline std::vector<index_t> compute_indices(size_t linear_index, const shape_t& shape,
                                            const stride_t& strides) {
    std::vector<index_t> indices(shape.size());
    for (size_t i = 0; i < shape.size(); ++i) {
        indices[i] = (linear_index / strides[i]) % shape[i];
    }

    return indices;
}

/**
 * @brief Check if two shapes are compatible for broadcasting
 *
 * Broadcasting follows NumPy/PyTorch rules:
 * 1. If arrays don't have the same rank, prepend ones to the smaller rank
 * 2. For each dimension:
 *    a. If sizes are equal, continue
 *    b. If one size is 1, it's stretched to match the other
 *    c. If sizes differ and neither is 1, error
 *
 * @param shape1 First shape
 * @param shape2 Second shape
 * @return true If shapes can be broadcast
 * @return false If shapes cannot be broadcast
 */
inline bool are_broadcastable(const shape_t& shape1, const shape_t& shape2) {
    const auto max_dims = std::max(shape1.size(), shape2.size());
    auto padded1 = shape1;
    auto padded2 = shape2;

    padded1.insert(padded1.begin(), max_dims - padded1.size(), 1);
    padded2.insert(padded2.begin(), max_dims - padded2.size(), 1);

    for (size_t i = 0; i < max_dims; ++i) {
        if (padded1[i] != padded2[i] && padded1[i] != 1 && padded2[i] != 1) {
            return false;
        }
    }

    return true;
}

/**
 * @brief Compute the result shape after broadcasting two shapes
 *
 * @param shape1 First shape
 * @param shape2 Second shape
 * @return shape_t Broadcast result shape
 * @throws ShapeError If shapes cannot be broadcast
 */
inline shape_t broadcast_shapes(const shape_t& shape1, const shape_t& shape2) {
    if (!are_broadcastable(shape1, shape2)) {
        std::string msg = fmt::format("Shapes [{}] and [{}] cannot be broadcast together",
                                      fmt::join(shape1, ","), fmt::join(shape2, ","));
        throw brezel::utils::ShapeError(msg);
    }

    const auto max_dims = std::max(shape1.size(), shape2.size());
    auto padded1 = shape1;
    auto padded2 = shape2;

    padded1.insert(padded1.begin(), max_dims - padded1.size(), 1);
    padded2.insert(padded2.begin(), max_dims - padded2.size(), 1);

    shape_t result(max_dims);
    for (size_t i = 0; i < max_dims; ++i) {
        result[i] = std::max(padded1[i], padded2[i]);
    }

    return result;
}

/**
 * @brief Determine the number of parallel threads to use for an operation
 *
 * Takes into account the size of the data and available hardware parallelism.
 *
 * @param size Size of the data to process
 * @param min_elements_per_thread Minimum elements each thread should process
 * @return size_t Number of threads to use
 */
inline size_t compute_num_threads(size_t size, size_t min_elements_per_thread = 1024) {
    const size_t hardware_threads = std::thread::hardware_concurrency();
    const size_t max_threads = size / min_elements_per_thread;

    return std::max(size_t(1), std::min(hardware_threads, max_threads));
}

/**
 * @brief Parallel for loop implementation
 *
 * @tparam Func Function type
 * @param begin Start index
 * @param end End index (exclusive)
 * @param func Function to apply to each index
 * @param min_elements_per_thread Minimum elements per thread
 */
template <typename Func>
inline void parallel_for(size_t begin, size_t end, Func&& func,
                         size_t min_elements_per_thread = 1024) {
    const size_t size = end - begin;
    if (size == 0)
        return;

    if (size <= min_elements_per_thread) {
        for (size_t i = begin; i < end; ++i) {
            func(i);
        }

        return;
    }

    const size_t num_threads = compute_num_threads(size, min_elements_per_thread);

    if (num_threads <= 1) {
        for (size_t i = begin; i < end; ++i) {
            func(i);
        }
    } else {
        std::vector<std::future<void>> futures;
        futures.reserve(num_threads);

        const size_t chunk_size = (size + num_threads - 1) / num_threads;

        for (size_t t = 0; t < num_threads; ++t) {
            const size_t chunk_begin = begin + t * chunk_size;
            const size_t chunk_end = std::min(begin + (t + 1) * chunk_size, end);

            if (chunk_begin >= chunk_end)
                break;

            futures.push_back(std::async(std::launch::async, [=, &func]() {
                for (size_t i = chunk_begin; i < chunk_end; ++i) {
                    func(i);
                }
            }));
        }

        for (auto& future : futures) {
            future.wait();
        }
    }
}

/**
 * @brief Checks if a tensor is contiguous in memory
 *
 * @param shape The shape of the tensor
 * @param strides The strides of the tensor
 * @return true If the tensor is contiguous
 * @return false If the tensor is not contiguous
 */
inline bool is_contiguous(const shape_t& shape, const stride_t& strides) {
    if (shape.empty() || strides.empty()) {
        return true;
    }

    const auto expected_strides = compute_strides(shape);
    return expected_strides == strides;
}

/**
 * @brief Generate a string representation of a shape
 *
 * @param shape The shape to format
 * @return std::string Formatted shape string
 */
inline std::string shape_to_string(const shape_t& shape) {
    return fmt::format("[{}]", fmt::join(shape, ", "));
}

}  // namespace utils
}  // namespace brezel