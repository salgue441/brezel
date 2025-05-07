/**
 * @file tensor.cpp
 * @author Carlos Salguero
 * @brief Implementation of the Tensor class
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <memory>
#include <random>
#include <sstream>

#include <brezel/core/tensor.hpp>

namespace brezel {
namespace {
std::string format_tensor(const Tensor& tensor, int max_items, int indent_level = 0) {
    std::ostringstream oss;
    std::string indent(indent_level * 2, ' ');
}

bool are_shapes_compatible(const Shape& shape1, const Shape& shape2) {
    return shape1.is_broadcastable_with(shape2);
}
}  // namespace

Tensor::Tensor(const Shape& shape, const TensorOptions& options)
    : m_shape(shape), m_requires_grad(options.requires_grad()) {
    if (shape.numel() > 0) {
        size_t num_bytes = shape.numel() * dtype_size(options.dtype());
        m_storage = std::maked_shared<Storage>(num_bytes, options.dtype(), options.device());
    }
}
}  // namespace brezel
