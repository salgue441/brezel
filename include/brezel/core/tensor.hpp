/**
 * @file tensor.hpp
 * @author Carlos Salguero
 * @brief Defines the Tensor class, the fundamental data structure
 *        of the brezel library
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * The Tensor class represents a multi-dimensional array with automatic
 * differentiation capabilities, similar to PyTorch's tensor. It provides
 * a comprehensive set of operations and integrates with the autograd system
 * for gradient-based optimization.
 */

#pragma once

#include <algorithm>
#include <functional>
#include <iosfwd>
#include <memory>
#include <numeric>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include <brezel/core/dtype.hpp>
#include <brezel/core/exception.hpp>
#include <brezel/core/scalar.hpp>
#include <brezel/core/shape.hpp>
#include <brezel/core/storage.hpp>
#include <brezel/core/tensor_options.hpp>

namespace brezel {
// Forward declaration
namespace autograd {
class GradientContext;
}

/**
 * @brief The main Tensor class for n-dimensional data operations
 *
 * This class represents an n-dimensional tensor with support for various
 * mathematical operations, autograd, and device placement.
 */
class Tensor {
public:
    using SizeType = int64_t;

    /**
     * @brief Create an empty tensor
     */
    Tensor() = default;

    /**
     * @brief Create a tensor with the specified shape and options
     *
     * @param shape The shape of the tensor
     * @param options The tensor options (dtype, device, etc.)
     */
    explicit Tensor(const Shape& shape, const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor with the specified shape and data
     *
     * @tparam T The data type of the input
     * @param shape The shape of the tensor
     * @param data Pointer to the data
     * @param options The tensor options (dtype, device, etc.)
     */
    template <typename T>
    Tensor(const Shape& shape, const T* data, const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor from a vector of data
     *
     * @tparam T The data type of the input
     * @param data Vector of data values
     * @param shape Optional shape (if not specified, a 1D tensor is created)
     * @param options The tensor options (dtype, device, etc.)
     */
    template <typename T>
    explicit Tensor(const std::vector<T>& data, std::optional<Shape> shape = std::nullopt,
                    const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor with the specified shape and fill with a scalar value
     *
     * @param shape The shape of the tensor
     * @param fill_value The scalar value to fill the tensor with
     * @param options The tensor options (dtype, device, etc.)
     */
    Tensor(const Shape& shape, const Scalar& fill_value,
           const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor filled with zeros
     *
     * @param shape The shape of the tensor
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A tensor filled with zeros
     */
    static Tensor zeros(const Shape& shape, const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor filled with ones
     *
     * @param shape The shape of the tensor
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A tensor filled with ones
     */
    static Tensor ones(const Shape& shape, const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor filled with random values from a uniform distribution
     *
     * @param shape The shape of the tensor
     * @param low Lower bound of the distribution
     * @param high Upper bound of the distribution
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A tensor filled with random values
     */
    static Tensor rand(const Shape& shape, double low = 0.0, double high = 1.0,
                       const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor filled with random values from a normal distribution
     *
     * @param shape The shape of the tensor
     * @param mean Mean of the distribution
     * @param std Standard deviation of the distribution
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A tensor filled with random values
     */
    static Tensor randn(const Shape& shape, double mean = 0.0, double std = 1.0,
                        const TensorOptions& options = TensorOptions());

    /**
     * @brief Create an identity matrix
     *
     * @param n The size of the matrix
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor An identity matrix of size n x n
     */
    static Tensor eye(SizeType n, const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor with evenly spaced values
     *
     * @param start Starting value
     * @param end Ending value (inclusive if step divides end-start exactly)
     * @param step Step size
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A 1D tensor with evenly spaced values
     */
    static Tensor arange(double start, double end, double step = 1.0,
                         const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor with evenly spaced values
     *
     * @param end Ending value (exclusive)
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A 1D tensor with evenly spaced values from 0 to end
     */
    static Tensor arange(double end, const TensorOptions& options = TensorOptions());

    /**
     * @brief Create a tensor with linearly spaced values
     *
     * @param start Starting value
     * @param end Ending value (inclusive)
     * @param steps Number of steps
     * @param options The tensor options (dtype, device, etc.)
     * @return Tensor A 1D tensor with linearly spaced values
     */
    static Tensor linspace(double start, double end, SizeType steps,
                           const TensorOptions& options = TensorOptions());

    /**
     * @brief Get the shape of the tensor
     *
     * @return const Shape& The shape of the tensor
     */
    const Shape& shape() const noexcept { return m_shape; }

    /**
     * @brief Get the number of dimensions
     *
     * @return SizeType The number of dimensions
     */
    SizeType ndim() const noexcept { return m_shape.size(); }

    /**
     * @brief Get the total number of elements
     *
     * @return SizeType The total number of elements
     */
    SizeType numel() const noexcept { return m_shape.numel(); }

    /**
     * @brief Get the data type of the tensor
     *
     * @return DType The data type
     */
    DType dtype() const noexcept { return m_storage ? m_storage->dtype() : DType::Undefined; }

    /**
     * @brief Get the device of the tensor
     *
     * @return Device The device
     */
    Device device() const noexcept { return m_storage ? m_storage->device() : Device::CPU; }

    /**
     * @brief Check if the tensor requires gradients
     *
     * @return bool True if the tensor requires gradients
     */
    bool requires_grad() const noexcept { return m_requires_grad; }

    /**
     * @brief Set whether the tensor requires gradients
     *
     * @param requires_grad Whether the tensor requires gradients
     * @return Tensor& Reference to the tensor
     */
    Tensor& set_requires_grad(bool requires_grad = true);

    /**
     * @brief Get the gradient of the tensor
     *
     * @return const Tensor& The gradient
     */
    const Tensor& grad() const;

    /**
     * @brief Check if the tensor has a gradient
     *
     * @return bool True if the tensor has a gradient
     */
    bool has_grad() const noexcept { return m_grad.has_value(); }

    /**
     * @brief Get a pointer to the data
     *
     * @tparam T The data type to cast to
     * @return T* Pointer to the data
     */
    template <typename T>
    T* data();

    /**
     * @brief Get a pointer to the data (const version)
     *
     * @tparam T The data type to cast to
     * @return const T* Pointer to the data
     */
    template <typename T>
    const T* data() const;

    /**
     * @brief Get the value at the specified indices
     *
     * @tparam Indices Types of the indices
     * @param indices The indices
     * @return Scalar The value at the specified indices
     */
    template <typename... Indices>
    Scalar item(Indices... indices) const;

    /**
     * @brief Set the value at the specified indices
     *
     * @tparam Indices Types of the indices
     * @param value The value to set
     * @param indices The indices
     * @return Tensor& Reference to the tensor
     */
    template <typename... Indices>
    Tensor& set(const Scalar& value, Indices... indices);

    /**
     * @brief Reshape the tensor
     *
     * @param new_shape The new shape
     * @return Tensor A reshaped tensor
     */
    Tensor reshape(const Shape& new_shape) const;

    /**
     * @brief Flatten the tensor to a 1D tensor
     *
     * @return Tensor A flattened tensor
     */
    Tensor flatten() const;

    /**
     * @brief Transpose the tensor
     *
     * @param dim0 First dimension to swap
     * @param dim1 Second dimension to swap
     * @return Tensor A transposed tensor
     */
    Tensor transpose(SizeType dim0, SizeType dim1) const;

    /**
     * @brief Permute the dimensions of the tensor
     *
     * @param dims The new order of dimensions
     * @return Tensor A permuted tensor
     */
    Tensor permute(const std::vector<SizeType>& dims) const;

    /**
     * @brief Squeeze the tensor by removing dimensions of size 1
     *
     * @param dim Optional dimension to squeeze
     * @return Tensor A squeezed tensor
     */
    Tensor squeeze(std::optional<SizeType> dim = std::nullopt) const;

    /**
     * @brief Unsqueeze the tensor by adding a dimension of size 1
     *
     * @param dim The dimension to add
     * @return Tensor An unsqueezed tensor
     */
    Tensor unsqueeze(SizeType dim) const;

    /**
     * @brief Add a tensor or scalar to this tensor
     *
     * @param other The tensor or scalar to add
     * @return Tensor The result of the addition
     */
    Tensor add(const Tensor& other) const;
    Tensor add(const Scalar& other) const;

    /**
     * @brief Subtract a tensor or scalar from this tensor
     *
     * @param other The tensor or scalar to subtract
     * @return Tensor The result of the subtraction
     */
    Tensor sub(const Tensor& other) const;
    Tensor sub(const Scalar& other) const;

    /**
     * @brief Multiply this tensor by a tensor or scalar
     *
     * @param other The tensor or scalar to multiply by
     * @return Tensor The result of the multiplication
     */
    Tensor mul(const Tensor& other) const;
    Tensor mul(const Scalar& other) const;

    /**
     * @brief Divide this tensor by a tensor or scalar
     *
     * @param other The tensor or scalar to divide by
     * @return Tensor The result of the division
     */
    Tensor div(const Tensor& other) const;
    Tensor div(const Scalar& other) const;

    /**
     * @brief Matrix multiplication with another tensor
     *
     * @param other The tensor to multiply with
     * @return Tensor The result of the matrix multiplication
     */
    Tensor matmul(const Tensor& other) const;

    /**
     * @brief Element-wise power operation
     *
     * @param exponent The exponent
     * @return Tensor The result of the power operation
     */
    Tensor pow(double exponent) const;

    /**
     * @brief Sum the tensor over the specified dimensions
     *
     * @param dims Dimensions to reduce over
     * @param keepdim Whether to keep the reduced dimensions
     * @return Tensor The result of the sum
     */
    Tensor sum(const std::vector<SizeType>& dims = {}, bool keepdim = false) const;

    /**
     * @brief Mean of the tensor over the specified dimensions
     *
     * @param dims Dimensions to reduce over
     * @param keepdim Whether to keep the reduced dimensions
     * @return Tensor The result of the mean
     */
    Tensor mean(const std::vector<SizeType>& dims = {}, bool keepdim = false) const;

    /**
     * @brief Apply a function element-wise
     *
     * @param func The function to apply
     * @return Tensor The result of applying the function
     */
    Tensor apply(const std::function<Scalar(const Scalar&)>& func) const;

    /**
     * @brief Check if the tensor is equal to another tensor
     *
     * @param other The other tensor
     * @return Tensor A boolean tensor with the result of the comparison
     */
    Tensor eq(const Tensor& other) const;

    /**
     * @brief Check if the tensor is not equal to another tensor
     *
     * @param other The other tensor
     * @return Tensor A boolean tensor with the result of the comparison
     */
    Tensor ne(const Tensor& other) const;

    /**
     * @brief Check if the tensor is greater than another tensor
     *
     * @param other The other tensor
     * @return Tensor A boolean tensor with the result of the comparison
     */
    Tensor gt(const Tensor& other) const;

    /**
     * @brief Check if the tensor is greater than or equal to another tensor
     *
     * @param other The other tensor
     * @return Tensor A boolean tensor with the result of the comparison
     */
    Tensor ge(const Tensor& other) const;

    /**
     * @brief Check if the tensor is less than another tensor
     *
     * @param other The other tensor
     * @return Tensor A boolean tensor with the result of the comparison
     */
    Tensor lt(const Tensor& other) const;

    /**
     * @brief Check if the tensor is less than or equal to another tensor
     *
     * @param other The other tensor
     * @return Tensor A boolean tensor with the result of the comparison
     */
    Tensor le(const Tensor& other) const;

    /**
     * @brief Convert the tensor to the specified data type
     *
     * @param dtype The target data type
     * @return Tensor The converted tensor
     */
    Tensor to(DType dtype) const;

    /**
     * @brief Convert the tensor to the specified device
     *
     * @param device The target device
     * @return Tensor The converted tensor
     */
    Tensor to(Device device) const;

    /**
     * @brief Convert the tensor to the specified options
     *
     * @param options The target options
     * @return Tensor The converted tensor
     */
    Tensor to(const TensorOptions& options) const;

    /**
     * @brief Convert the tensor to a std::vector
     *
     * @tparam T The target data type
     * @return std::vector<T> A vector containing the tensor's data
     */
    template <typename T>
    std::vector<T> to_vector() const;

    Tensor operator+(const Tensor& other) const { return add(other); }
    Tensor operator+(const Scalar& other) const { return add(other); }

    Tensor operator-(const Tensor& other) const { return sub(other); }
    Tensor operator-(const Scalar& other) const { return sub(other); }

    Tensor operator*(const Tensor& other) const { return mul(other); }
    Tensor operator*(const Scalar& other) const { return mul(other); }

    Tensor operator/(const Tensor& other) const { return div(other); }
    Tensor operator/(const Scalar& other) const { return div(other); }

    Tensor& operator+=(const Tensor& other);
    Tensor& operator+=(const Scalar& other);

    Tensor& operator-=(const Tensor& other);
    Tensor& operator-=(const Scalar& other);

    Tensor& operator*=(const Tensor& other);
    Tensor& operator*=(const Scalar& other);

    Tensor& operator/=(const Tensor& other);
    Tensor& operator/=(const Scalar& other);

    bool operator==(const Tensor& other) const;
    bool operator!=(const Tensor& other) const { return !(*this == other); }

    /**
     * @brief Index into the tensor with a single index
     *
     * @param index The index
     * @return Tensor A view of the indexed tensor
     */
    Tensor operator[](SizeType index) const;

    /**
     * @brief Index into the tensor with an index tuple
     *
     * @param indices The index tuple
     * @return Tensor A view of the indexed tensor
     */
    Tensor operator[](const std::vector<SizeType>& indices) const;

    /**
     * @brief Compute gradients through the computation graph
     */
    void backward();

    /**
     * @brief Zero the gradients of the tensor
     */
    void zero_grad();

    /**
     * @brief Detach the tensor from the computation graph
     *
     * @return Tensor A detached tensor
     */
    Tensor detach() const;

    /**
     * @brief Convert the tensor to a printable string representation
     *
     * @param max_items Maximum number of elements to print per dimension
     * @return std::string String representation of the tensor
     */
    std::string to_string(int max_items = 6) const;

    /**
     * @brief Get a debug string with metadata about the tensor
     *
     * @return std::string Debug string with tensor metadata
     */
    std::string debug_string() const;

private:
    Shape m_shape{};                       ///< The shape of the tensor
    std::shared_ptr<Storage> m_storage{};  ///< The storage for the tensor data
    bool m_requires_grad{false};           ///< Whether gradients are required
    std::optional<Tensor> m_grad{};        ///< The gradient of the tensor
    std::weak_ptr<autograd::GradientContext> m_grad_ctx_{};

    Tensor(const Shape& shape, std::shared_ptr<Storage> storage, bool requires_grad = false);

    // Helper functions
    void create_grad_tensor();
    void set_grad_ctx(std::shared_ptr<autograd::GradientContext> ctx);

    // Friend functions
    friend std::ostream& operator<<(std::ostream& os, const Tensor& tensor);
    friend class autograd::GradientContext;
};

/**
 * @brief Output stream operator for tensors
 *
 * @param os The output stream
 * @param tensor The tensor to output
 * @return std::ostream& The output stream
 */
std::ostream& operator<<(std::ostream& os, const Tensor& tensor);

// Scalar-tensor operations (free functions)
Tensor operator+(const Scalar& scalar, const Tensor& tensor);
Tensor operator-(const Scalar& scalar, const Tensor& tensor);
Tensor operator*(const Scalar& scalar, const Tensor& tensor);
Tensor operator/(const Scalar& scalar, const Tensor& tensor);
}  // namespace brezel