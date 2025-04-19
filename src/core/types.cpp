#include <algorithm>
#include <complex>
#include <functional>
#include <numeric>
#include <sstream>
#include <stdexcept>

#include <brezel/core/types.hpp>

namespace brezel {
std::string dtype_to_string(dtype_t dtype) {
    switch (dtype) {
        case dtype_t::Float32:
            return "Float32";

        case dtype_t::Float64:
            return "Float64";

        case dtype_t::Int32:
            return "Int32";

        case dtype_t::Int64:
            return "Int64";

        case dtype_t::Uint8:
            return "Uint8";

        case dtype_t::Bool:
            return "Bool";

        case dtype_t::Complex64:
            return "Complex64";

        default:
            return "Unknown";
    }
}

size_t dtype_size(dtype_t dtype) {
    switch (dtype) {
        case dtype_t::Float32:
            return sizeof(float);

        case dtype_t::Float64:
            return sizeof(double);

        case dtype_t::Int32:
            return sizeof(int32_t);

        case dtype_t::Int64:
            return sizeof(int64_t);

        case dtype_t::Uint8:
            return sizeof(uint8_t);

        case dtype_t::Bool:
            return sizeof(bool);

        case dtype_t::Complex64:
            return sizeof(std::complex<float>);

        default:
            return 0;
    }
}

std::string dtype_to_type_name(dtype_t dtype) {
    switch (dtype) {
        case dtype_t::Float32:
            return "float";

        case dtype_t::Float64:
            return "double";

        case dtype_t::Int32:
            return "int32_t";

        case dtype_t::Int64:
            return "int64_t";

        case dtype_t::Uint8:
            return "uint8_t";

        case dtype_t::Bool:
            return "bool";

        case dtype_t::Complex64:
            return "std::complex<float>";

        default:
            return "unknown";
    }
}

std::string device_type_to_string(DeviceType deviceType) {
    switch (deviceType) {
        case DeviceType::CPU:
            return "CPU";

        case DeviceType::CUDA:
            return "CUDA";

        default:
            return "Unknown";
    }
}

bool shapes_equal(const shape_t& lhs, const shape_t& rhs) {
    if (lhs.size() != rhs.size()) {
        return false;
    }

    return std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

bool strides_equal(const stride_t& lhs, const stride_t& rhs) {
    if (lhs.size() != rhs.size()) {
        return false;
    }

    return std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

size_t calculate_size(const shape_t& shape) {
    if (shape.empty()) {
        return 0;
    }

    return std::accumulate(shape.begin(), shape.end(), static_cast<size_t>(1),
                           std::multiplies<size_t>());
}

stride_t calculate_strides(const shape_t& shape, MemoryLayout layout) {
    stride_t strides(shape.size());

    if (shape.empty()) {
        return strides;
    }

    if (layout == MemoryLayout::RowMajor) {
        index_t stride = 1;
        for (int i = static_cast<int>(shape.size()) - 1; i >= 0; --i) {
            strides[i] = stride;
            stride *= shape[i];
        }
    } else {
        index_t stride = 1;
        for (size_t i = 0; i < shape.size(); ++i) {
            strides[i] = stride;
            stride *= shape[i];
        }
    }

    return strides;
}

std::vector<index_t> linear_index_to_indices(size_t linearIndex, const shape_t& shape,
                                             const stride_t& strides) {
    std::vector<index_t> indices(shape.size());

    for (size_t i = 0; i < shape.size(); ++i) {
        indices[i] = (linearIndex / strides[i]) % shape[i];
    }

    return indices;
}

size_t indices_to_linear_index(const std::vector<index_t>& indices, const stride_t& strides) {
    if (indices.size() != strides.size()) {
        throw std::invalid_argument("Indices and strides must have the same size");
    }

    size_t linear_index = 0;
    for (size_t i = 0; i < indices.size(); ++i) {
        linear_index += indices[i] * strides[i];
    }

    return linear_index;
}

bool is_valid_shape(const shape_t& shape) {
    return std::all_of(shape.begin(), shape.end(), [](index_t dim) { return dim > 0; });
}

bool is_contiguous(const shape_t& shape, const stride_t& strides) {
    if (shape.empty() || strides.empty()) {
        return true;
    }

    const auto expected_strides = calculate_strides(shape);
    return strides_equal(strides, expected_strides);
}

std::ostream& operator<<(std::ostream& os, const shape_t& shape) {
    os << "[";
    for (size_t i = 0; i < shape.size(); ++i) {
        os << shape[i];
        if (i < shape.size() - 1) {
            os << ", ";
        }
    }

    os << "]";
    return os;
}

void print_strides(std::ostream& os, const stride_t& strides) {
    os << "[";
    for (size_t i = 0; i < strides.size(); ++i) {
        os << strides[i];

        if (i < strides.size() - 1) {
            os << ", ";
        }
    }

    os << "]";
}
}  // namespace brezel