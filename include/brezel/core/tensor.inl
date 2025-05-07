/**
 * @file tensor.inl
 * @author Carlos Salguero
 * @brief Inline implementation of template methods of the Tensor class
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#pragma once

#include <algorithm>
#include <random>

namespace brezel {

namespace {
template <typename T>
void fill_value(T* data, size_t size, const Scalar& value) {
    T typed_value = value.to<T>();

    for (size_t i = 0; i < size; ++i) {
        data[i] = typed_value;
    }
}

template <typename T>
void fill_uniform(T* data, size_t size, double low, double high) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dist(low, high);

    for (size_t i = 0; i < size; ++i) {
        data[i] = static_cast<T>(dist(gen));
    }
}

template <typename T>
void fill_normal(T* data, size_t size, double mean, double stddev) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<double> dist(mean, stddev);

    for (size_t i = 0; i < size; ++i) {
        data[i] = static_cast<T>(dist(gen));
    }
}
}  // namespace

template <typename T>
Tensor::Tensor(const Shape& shape, const T* data, const TensorOptions& options)
    : m_shape(shape), m_requires_grad(options.requires_grad()) {
    if (shape.numel() > 0) {
        if (data == nullptr) {
            throw ValueError("Cannot create tensor from nullptr data");
        }

        m_storage = Storage::from_cpu_data(data, shape.numel());

        // Convert storage to requested options if different
        if (m_storage->dtype() != options.dtype() || m_storage->device() != options.device()) {
            m_storage = m_storage->to(options.dtype())->to(options.device());
        }
    }
}

template <typename T>
Tensor::Tensor(const std::vector<T>& data, std::optional<Shape> shape, const TensorOptions& options)
    : m_requires_grad(options.requires_grad()) {
    if (shape.has_value()) {
        m_shape = shape.value();
        if (m_shape.numel() != data.size()) {
            throw ShapeError("Vector size " + std::to_string(data.size()) +
                             " doesn't match shape size " + std::to_string(m_shape.numel()));
        }
    } else {
        m_shape = Shape({static_cast<Shape::SizeType>(data.size())});
    }

    if (!data.empty()) {
        m_storage = Storage::from_cpu_data(data.data(), data.size());

        if (m_storage->dtype() != options.dtype() || m_storage->device() != options.device()) {
            m_storage = m_storage->to(options.dtype())->to(options.device());
        }
    }
}

template <typename T>
T* Tensor::data() {
    if (!m_storage) {
        throw ValueError("Cannot access data of an empty tensor");
    }

    if (get_dtype<T>() != m_storage->dtype()) {
        throw TypeError("Requested data type does not match tensor dtype");
    }

    return static_cast<T*>(m_storage->data());
}

template <typename T>
const T* Tensor::data() const {
    if (!m_storage) {
        throw ValueError("Cannot access data of an empty tensor");
    }

    if (get_dtype<T>() != m_storage->dtype()) {
        throw TypeError("Requested data type does not match tensor dtype");
    }

    return static_cast<const T*>(m_storage->data());
}

template <typename... Indices>
Scalar Tensor::item(Indices... indices) const {
    if (!m_storage) {
        throw ValueError("Cannot access item from an empty tensor");
    }

    // Convert variadic indices to vector and check bounds
    std::vector<SizeType> idx = {static_cast<SizeType>(indices)...};

    if (idx.size() != m_shape.size()) {
        throw IndexError("Number of indices (" + std::to_string(idx.size()) +
                         ") doesn't match tensor dimensions (" + std::to_string(m_shape.size()) +
                         ")");
    }

    // Check bounds
    for (size_t i = 0; i < idx.size(); ++i) {
        if (idx[i] < 0 || idx[i] >= m_shape[i]) {
            throw IndexError("Index " + std::to_string(idx[i]) + " out of bounds for dimension " +
                             std::to_string(i) + " with size " + std::to_string(m_shape[i]));
        }
    }

    // Calculate linear index
    auto strides = m_shape.strides();
    size_t linearIndex = 0;
    for (size_t i = 0; i < idx.size(); ++i) {
        linearIndex += idx[i] * strides[i];
    }

    // Return element as Scalar
    switch (m_storage->dtype()) {
        case DType::Bool:
            return Scalar(data<bool>()[linearIndex]);
        case DType::UInt8:
            return Scalar(data<uint8_t>()[linearIndex]);
        case DType::Int8:
            return Scalar(data<int8_t>()[linearIndex]);
        case DType::UInt16:
            return Scalar(data<uint16_t>()[linearIndex]);
        case DType::Int16:
            return Scalar(data<int16_t>()[linearIndex]);
        case DType::UInt32:
            return Scalar(data<uint32_t>()[linearIndex]);
        case DType::Int32:
            return Scalar(data<int32_t>()[linearIndex]);
        case DType::UInt64:
            return Scalar(data<uint64_t>()[linearIndex]);
        case DType::Int64:
            return Scalar(data<int64_t>()[linearIndex]);
        case DType::Float32:
            return Scalar(data<float>()[linearIndex]);
        case DType::Float64:
            return Scalar(data<double>()[linearIndex]);
        case DType::Complex64:
            return Scalar(data<std::complex<float>>()[linearIndex]);
        case DType::Complex128:
            return Scalar(data<std::complex<double>>()[linearIndex]);
        default:
            throw TypeError("Unsupported data type for item access");
    }
}

template <typename... Indices>
Tensor& Tensor::set(const Scalar& value, Indices... indices) {
    if (!m_storage) {
        throw ValueError("Cannot set item in an empty tensor");
    }

    // Convert variadic indices to vector and check bounds
    std::vector<SizeType> idx = {static_cast<SizeType>(indices)...};

    if (idx.size() != m_shape.size()) {
        throw IndexError("Number of indices (" + std::to_string(idx.size()) +
                         ") doesn't match tensor dimensions (" + std::to_string(m_shape.size()) +
                         ")");
    }

    // Check bounds
    for (size_t i = 0; i < idx.size(); ++i) {
        if (idx[i] < 0 || idx[i] >= m_shape[i]) {
            throw IndexError("Index " + std::to_string(idx[i]) + " out of bounds for dimension " +
                             std::to_string(i) + " with size " + std::to_string(m_shape[i]));
        }
    }

    // Calculate linear index
    auto strides = m_shape.strides();
    size_t linearIndex = 0;
    for (size_t i = 0; i < idx.size(); ++i) {
        linearIndex += idx[i] * strides[i];
    }

    // Set element value
    switch (m_storage->dtype()) {
        case DType::Bool:
            data<bool>()[linearIndex] = value.to<bool>();
            break;
        case DType::UInt8:
            data<uint8_t>()[linearIndex] = value.to<uint8_t>();
            break;
        case DType::Int8:
            data<int8_t>()[linearIndex] = value.to<int8_t>();
            break;
        case DType::UInt16:
            data<uint16_t>()[linearIndex] = value.to<uint16_t>();
            break;
        case DType::Int16:
            data<int16_t>()[linearIndex] = value.to<int16_t>();
            break;
        case DType::UInt32:
            data<uint32_t>()[linearIndex] = value.to<uint32_t>();
            break;
        case DType::Int32:
            data<int32_t>()[linearIndex] = value.to<int32_t>();
            break;
        case DType::UInt64:
            data<uint64_t>()[linearIndex] = value.to<uint64_t>();
            break;
        case DType::Int64:
            data<int64_t>()[linearIndex] = value.to<int64_t>();
            break;
        case DType::Float32:
            data<float>()[linearIndex] = value.to<float>();
            break;
        case DType::Float64:
            data<double>()[linearIndex] = value.to<double>();
            break;
        case DType::Complex64:
            data<std::complex<float>>()[linearIndex] = value.to<std::complex<float>>();
            break;
        case DType::Complex128:
            data<std::complex<double>>()[linearIndex] = value.to<std::complex<double>>();
            break;
        default:
            throw TypeError("Unsupported data type for set operation");
    }

    return *this;
}

template <typename T>
std::vector<T> Tensor::to_vector() const {
    if (!m_storage) {
        return std::vector<T>();
    }

    std::vector<T> result(m_shape.numel());

    switch (m_storage->dtype()) {
        case DType::Bool:
            std::transform(data<bool>(), data<bool>() + m_shape.numel(), result.begin(),
                           [](bool val) { return static_cast<T>(val); });
            break;
        case DType::UInt8:
            std::transform(data<uint8_t>(), data<uint8_t>() + m_shape.numel(), result.begin(),
                           [](uint8_t val) { return static_cast<T>(val); });
            break;
        case DType::Int8:
            std::transform(data<int8_t>(), data<int8_t>() + m_shape.numel(), result.begin(),
                           [](int8_t val) { return static_cast<T>(val); });
            break;
        case DType::UInt16:
            std::transform(data<uint16_t>(), data<uint16_t>() + m_shape.numel(), result.begin(),
                           [](uint16_t val) { return static_cast<T>(val); });
            break;
        case DType::Int16:
            std::transform(data<int16_t>(), data<int16_t>() + m_shape.numel(), result.begin(),
                           [](int16_t val) { return static_cast<T>(val); });
            break;
        case DType::UInt32:
            std::transform(data<uint32_t>(), data<uint32_t>() + m_shape.numel(), result.begin(),
                           [](uint32_t val) { return static_cast<T>(val); });
            break;
        case DType::Int32:
            std::transform(data<int32_t>(), data<int32_t>() + m_shape.numel(), result.begin(),
                           [](int32_t val) { return static_cast<T>(val); });
            break;
        case DType::UInt64:
            std::transform(data<uint64_t>(), data<uint64_t>() + m_shape.numel(), result.begin(),
                           [](uint64_t val) { return static_cast<T>(val); });
            break;
        case DType::Int64:
            std::transform(data<int64_t>(), data<int64_t>() + m_shape.numel(), result.begin(),
                           [](int64_t val) { return static_cast<T>(val); });
            break;
        case DType::Float32:
            std::transform(data<float>(), data<float>() + m_shape.numel(), result.begin(),
                           [](float val) { return static_cast<T>(val); });
            break;
        case DType::Float64:
            std::transform(data<double>(), data<double>() + m_shape.numel(), result.begin(),
                           [](double val) { return static_cast<T>(val); });
            break;
        default:
            throw TypeError("Unsupported data type for conversion to vector");
    }

    return result;
}

}  // namespace brezel