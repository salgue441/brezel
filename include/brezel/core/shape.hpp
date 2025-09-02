/**
 * @file shape.hpp
 * @author Carlos Salguero
 * @brief Multi-dimensional shape implementation for tensor operations.
 *
 * @version 0.1
 * @date 2025-09-01
 *
 * @copyright Copyright (c) 2025
 *
 * This header defines the `Shape` class template and related utilities for
 * representing and manipulating the dimensions of multi-dimensional arrays
 * (tensors). It provides both compile-time fixed-size shapes and runtime
 * dynamic shapes with a unified interface.
 *
 * @defgroup shape Shape utilities
 * @ingroup tensor_core
 */

#pragma once

#include <algorithm>
#include <array>
#include <cassert>
#include <initializer_list>
#include <numeric>
#include <span>
#include <vector>

#include "brezel/core/concepts.hpp"
#include "brezel/utils/error.hpp"

namespace brezel::core {
/**
 * @brief Fixed-size shape for compile-time known dimensions
 *
 * The `Shape` class template represents the dimensions of a multi-dimensional
 * array. When `N != 0`, it provides compile-time known dimensions with stack
 * storage. When `N = 0`, it uses dynamic storage for runtime dimension
 * specification.
 *
 * @tparam N Number of dimensions (0 means dynamic)
 * @par Example:
 * @code {cpp}
 * Shape<3> fixed_shape{2, 3, 4}; // Compile-time known 3D shape
 * Shape<0> dynamic_shape{2, 3, 4}; // Runtime dynamic shape
 * @endcode
 *
 * @par Thread Safety:
 * - Const operations: thread-safe
 * - Non-const operations: not thread-safe (synchronization required)
 *
 * @par Exception Safety:
 * - Most operations provide strong exception guarantee
 * - Element access with `at()` throws `std::out_of_range` on invalid index
 * - Operations marked `noexcept` will not throw exceptions
 */
template <std::size_t N = 0>
class Shape {
  public:
    using value_type = std::size_t;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;
    using iterator = pointer;
    using const_iterator = const pointer;

    static constexpr size_type static_ndim = N;
    static constexpr bool is_dynamic = (N == 0);

  public:
    /**
     * @brief Default constructor
     * @post All dimensions are value-initialized (typically 0)
     */
    constexpr Shape() noexcept : m_dims{} {}

    /**
     * @brief Construct from initializer list
     *
     * @param dims Initilaizer list of dimension sizes
     * @pre `dims.size() <= N`
     * @throws std::out_of_range if assertion fails (in debug builds)
     *
     * @note Only available for static shapes
     */
    constexpr Shape(std::initializer_list<value_type> dims)
        requires(!is_dynamic)
        : m_dims{} {
        assert(dims.size() <= N);
        std::copy(dims.begin(), dims.end(), m_dims.begin);
    }

    /**
     * @brief Construct from std::array
     * @param dims Array containing dimension sizes
     */
    constexpr explicit Shape(const std::array<value_type, N> &dims)
        requires(!is_dynamic)
        : m_dims{} {}

    /**
     * @brief Construct from span
     * @param dims Span containing dimension sizes
     *
     * @pre dims.size() <= N
     * @throws std::out_of_range if assertion fails (in debug builds)
     */
    constexpr explicit Shape(std::span<const value_type> dims)
        requires(!is_dynamic)
        : m_dims{} {
        assert(dims.size() <= N);
        std::copy(dims.begin(), dims.end(), m_dims.begin());
    }

    /**
     * @brief Construct from range.
     *
     * @tparam R Range type satisfying IntegralRange concept
     * @param range Range of integral values
     *
     * @pre std::ranges::size(range) <= N
     * @throws std::out_of_range if assertion fails (in debug builds)
     */
    template <IntegralRange R>
    constexpr explicit Shape(const &R range)
        requires(!is_dynamic)
        : m_dims{} {
        assert(std::ranges::size(range) <= N);
        std::ranges::copy(range, m_dims.begin());
    }

    /**
     * @brief Variadic constructor
     *
     * @tparam Dims Integral types for dimension values
     * @param dims Dimension values as variadic arguments
     * @pre sizeof...(Dims) <= N
     */
    template <Integral... Dims>
    constexpr Shape(Dims... dims)
        requires(!is_dynamic && sizeof...(Dims) <= N)
        : m_dims{static_cast<value_type>(dims)...} {}

    // Copy & move constructors
    constexpr Shape(const Shape &) = default;
    constexpr Shape(Shape &&) noexcept = default;
    constexpr Shape &operator=(const Shape &) = default;
    constexpr Shape &operator=(Shape &&) noexcept = default;

    // Group: Element Access
    /// @{

    /**
     * @brief Subscript access (const)
     *
     * @param index Dimension index [0, N-1]
     * @return Const reference to dimension value
     *
     * @pre index < N
     */
    constexpr const_reference operator[](size_type index) const noexcept {
        assert(index < ndim());
        return m_dims[index];
    }

    /**
     * @brief Subscript access (non-const)
     *
     * @param index Dimension index [0, N-1]
     * @return Const reference to dimension value
     *
     * @pre index < N
     */
    constexpr reference operator[](size_type index) noexcept {
        assert(index < ndim());
        return m_dims[index];
    }

    /**
     * @brief Bounds-checked element access (const)
     *
     * @param index Dimension index [0, N-1]
     * @return Const reference to dimension value
     * @throws std::out_of_range if index >= N
     */
    constexpr const_reference at(size_type index) const {
        if (index >= ndim()) {
            throw std::out_of_range("Shape index out of range");
        }

        return m_dims[index];
    }

    /**
     * @brief Bounds-checked element access (non-const)
     *
     * @param index Dimension index [0, N-1]
     * @return Const reference to dimension value
     * @throws std::out_of_range if index >= N
     */
    constexpr const_reference at(size_type index) {
        if (index >= ndim()) {
            throw std::out_of_range("Shape index out of range");
        }

        return m_dims[index];
    }
    /// @}

    // Group: Size and Capacity
    /// @{

    /// @brief Returns number of dimensions (N)
    constexpr size_type ndim() const noexcept { return N; }

    /// @brief Returns number of dimensions (N)
    constexpr size_type size() const noexcept { return N; }

    /// @brief Returns maximum size (N)
    constexpr size_type max_size() const noexcept { return N; }

    /// @brief Returns true if no dimensions, false otherwise
    constexpr bool empty() const noexcept { return N == 0; }

    /**
     * @brief Compute total number of elements
     *
     * @return Product of all dimension sizes
     */
    constexpr size_type numel() const noexcept {
        return std::accumulate(m_dims.begin(), m_dims.end(), size_type{1}, std::multiplies{});
    }
    /// @}

    // Group: Iterators
    /// @{

    /// @brief Begin iterator (const)
    constexpr const_iterator begin() const noexcept { return m_dims.data(); }

    /// @brief End iterator (const)
    constexpr const_iterator end() const noexcept { return m_dims.data() + N; }

    /// @brief Begin iterator
    constexpr iterator begin() noexcept { return m_data(); }

    /// @brief End iterator
    constexpr iterator end() noexcept { return m_data() + N; }

    /// @brief Const begin iterator
    constexpr const_iterator cbegin() const noexcept { return begin(); }

    /// @brief Const end iterator
    constexpr const_iterator cend() const noexcept { return end(); }
    /// @}

    // Group: Data Access
    /// @{

    /// @brief Direct data access (const)
    constexpr const_pointer data() const noexcept { return m_dims.data(); }

    /// @brief Direct data access (non-const)
    const pointer data() noexcept { return m_dims.data(); }
    /// @}

    // Group: Operators
    /// @{

    /**
     * @brief Equality comparison
     *
     * @param other Shape to compare with
     * @return true if all dimensions are equal
     */
    constexpr bool operator==(const Shape &shape) const noexcept { return std::equal(begin(), end(), other.begin()); }

    /**
     * @brief Inequality operator
     *
     * @param other Shape to compare with
     * @return true if any dimension differs
     */
    constexpr bool operator!=(const Shape &shape) const noexcept { return !(*this == other); }

    /**
     * @brief Three-way comparison (lexicographical ordering)
     *
     * @param other Shape to compare with
     * @return std::strong_ordering result
     */
    constexpr auto operator<=>(const Shape &other) const noexcept {
        return std::lexicographical_compare_three_way(begin(), end(), other.begin(), other.end());
    }
    /// @}

    // Group: Utility methods
    /// @{

    /// @brief Returns true for 0-dimensional shape
    constexpr bool is_scalar() const noexcept { return N == 0; }

    /// @brief Returns true for 1-dimensional shape
    constexpr bool is_vector() const noexcept { return N == 1; }

    /// @brief Returns true for 2-dimensional shape
    constexpr bool is_matrix() const noexcept { return N == 2; }

    /**
     * @brief Reshape to different static size
     *
     * Creates a new shape with M dimensions while preserving the total
     * number of elements. The product of the new dimensions must equal the
     * current total number of elements.
     *
     * @tparam M Target number of dimensions
     * @return New shape with M dimensions
     * @throws std::invalid_argument if the total number of elements cannot
     *         be preserved.
     *
     * @par Example:
     * @code {cpp}
     * brezel::Shape<4> original{2, 3, 4, 5}; // 120 elements
     * auto reshaped = original.reshape<3>(); // Returns Shape<3>{6, 4, 5} (120 dimensions)
     * @endcode
     */
    template <std::size_t M>
    constexpr Shape<M> reshape() const {
        static_assert(M != 0, "Cannot reshape to dynamic shape");
        constexpr size_type total_elements = numel();

        if constexpr (N == 0) {
            static_assert(M == 1, "Scalar can only be reshaped to 1D shape");
            return Shape<M>{1};
        }

        Shape<M> result;
        if constexpr (M < N) {
            constexpr size_type flatten_dims = N - M + 1;
            size_type flattened_size = 1;

            for (size_type i = 0; i < flatten_dims; ++i) {
                flattened_size *= m_dims[i];
            }

            result[0] = flattened_size;
            for (size_type i = 1; i < M; ++i) {
                result[i] = m_dims[flatten_dims + i - 1];
            }
        } else if constexpr (M > N) {
            constexpr size_type extra_dims = M - N;
            for (size_type i = 0; i < extra_dims; ++i) {
                result[i] = 1;
            }

            for (size_type i = 0; i < N; ++i) {
                result[extra_dims + 1] = m_dims[i];
            }
        }

        if constexpr (M > 0) {
            constexpr size_type new_total = result.numel();
            if (new_total != total_elements) {
                throw std::invalid_argument("Reshape would change total number of elements: " +
                                            std::to_string(total_elements) + " -> " + std::to_string(new_total));
            }
        }

        return result;
    }

    /**
     * @brief Reshape with specific dimension values.
     *
     * @tparam M Target number of dimensions.
     * @tparam Dims Integral types for dimension values.
     * @param dims Dimension values for the new shape
     *
     * @return New shape with specified dimensions
     * @throws std::invalid_argument if the total number of elements doesn't
     *         match
     *
     * @par Example:
     * @code {cpp}
     * brezel::Shape<4> original{2, 3, 4, 5}; // 120 elements
     * auto reshaped = original.reshape<3>(6, 4, 5); // Returns Shape<3>{6, 4, 5}
     * @endcode
     */
    template <std::size_t M, Integral... Dims>
    constexpr Shape<M> reshape(Dims... dims) const {
        static_assert(M != 0, "Cannot reshape to dynamic shape");
        static_assert(sizeof...(Dims) == M, "Number of arguments must match target dimension count");

        Shape<M> result{dims...};
        constexpr size_type total_elements = numel();
        constexpr size_type new_total = result.numel();

        if (new_total != total_elements) {
            throw std::invalid_argument("Reshape would change total number of elements: " +
                                        std::to_string(total_elements) + " -> " + std::to_string(new_total));
        }

        return result;
    }

    /**
     * @brief Reshape with automatic dimension inference (-1)
     *
     * One dimension can be -1 to be automatically calculated.
     *
     * @tparam M Target number of dimensions
     * @tparam Dims Integral types for dimension values
     * @param dims Dimension values for the new shape (one can be -1)
     * @return New shape with specified dimensions
     * @throws std::invalid_argument if the total number of elements doesn't
     *         match or multiple -1 values are provided
     *
     * @par Example:
     * @code
     * tensor::Shape<4> original{2, 3, 4, 5};  // 120 elements
     * auto reshaped = original.reshape<3>(6, -1, 5);  // Returns Shape<3>{6, 4, 5}
     * @endcode
     */
    template <std::size_t M, Integral... Dims>
    constexpr Shape<M> reshape_with_inference(Dims... dims) const {
        static_assert(M != 0, "Cannot reshape to dynamic scope");
        static_assert(sizeof...(Dims) == M, "Number of arguments must match target dimension count");

        std::array<value_type, M> new_dims = {static_cast<value_type>(dims)...};
        size_type inferred_index = M;
        size_type known_product = 1;

        for (size_type i = 0; i < M; ++i) {
            if (new_dims[i] == static_cast<value_type>(-1)) {
                if (inferred_index != M) {
                    throw std::invalid_argument("Only one dimension can be inferred (-1)");
                }

                inferred_index = i;
            } else {
                known_product *= new_dims[i];
            }
        }

        constexpr size_type total_elements = numel();
        if (inferred_index != M) {
            if (known_product == 0 || total_elements % known_product != 0) {
                throw std::invalid_argument("Cannot infer dimension: total elements " + std::to_string(total_elements) +
                                            " not divisible by product of known dimensions " +
                                            std::to_string(known_product));
            }

            new_dims[inferred_index] = total_elements / known_product;
        } else if (known_product != total_elements) {
            throw std::invalid_argument("Reshape would change total number of elements: " +
                                        std::to_string(total_elements) + " -> " + std::to_string(known_product));
        }

        return Shape<M>(new_dims);
    }

    /**
     * @brief Convert to dynamic shape
     * @return Dynamic shape with same dimensions
     */
    Shape<0> to_dynamic const();

    /**
     * @brief Convert to different static size
     *
     * @tparam M Target number of dimensions (must be >= N)
     * @return New shape with M dimensions
     * @pre M >= N (compile-time check)
     * @note Extra dimensions are value initialized
     */
    template <std::size_t M>
    constexpr Shape<M> to_static() const
        requires(M != 0)
    {
        static_assert(M >= N, "Target shape must have at least as many dimensions");

        Shape<M> result;

        std::copy(begin(), end(), result.begin());
        return result;
    }

  private:
    std::array<value_type, N> m_dims;
};

/**
 * @brief Dynamic shape specialization for runtime-determined dimensions.
 *
 * Specialization of `Shape` for dynamic dimension counts (N = 0). Uses heap
 * storage and provides additional capacity management and modification
 * operations.
 *
 * @par Example:
 * @code {cpp}
 * brezel::Shape<0> dynamic_shape{2, 3, 4}; // 3D shape created at runtime
 * dynamic_shape.push_back(5);              // Now 4D shape: {2, 3, 4, 5}
 * @endcode
 *
 * @par Thread Safety:
 * - Const operations: thread-safe
 * - Non-const operations: not thread-safe (synchronization required)
 *
 * @par Exception Safety:
 * - Most operations provide strong exception guarantee
 * - at() methods throw std::out_of_range on invalid index
 * - Memory allocation may throw std::bad_alloc
 */
template <>
class Shape<0> {
  public:
    using value_type = std::size_t;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;
    using iterator = std::vector<value_type>::iterator;
    using const_iterator = std::vector<value_type>::const_iterator;

    static constexpr size_type static_ndim = 0;
    static constexpr bool is_dynamic = true;

  public:
    /**
     * @brief Default constructor
     * @post Creates an empty shape (scalar)
     */
    Shape() = default;

    /**
     * @brief Constructor with dimension count
     *
     * @param ndim Number of dimensions
     * @post All dimensions are valued-initialized (typically 0)
     */
    explicit Shape(size_type ndim) : m_dims(ndim) {}

    /**
     * @brief Construct from initializer list
     * @param dims Initializer list of dimension sizes
     */
    Shape(std::initializer_list<value_type> dims) : m_dims(dims) {}

    /**
     * @brief Construct from std::vector (copy)
     * @param dims Vector containing dimension sizes
     */
    explicit Shape(const std::vector<value_type> &dims) : m_dims(dims) {}

    /**
     * @brief Constructo from std::vector (move)
     * @param dims Vector containing dimension sizes
     */
    explicit Shape(std::vector<value_type> &&dims) noexcept : m_dims(std::move(dims)) {}

    /**
     * @brief Construct from span
     * @param dims Span containing dimension sizes
     */
    explicit Shape(std::span<const value_type> dims) : m_dims(dims.begin(), dims.end()) {}

    /**
     * @brief Construct from range
     * @tparam R Range type satisfying IntegralRange concept
     * @param range Range of integral values
     */
    template <IntegralRange R>
    explicit Shape(const R &range) : m_dims(std::ranges::begin(range), std::ranges::end(range)) {}

    /**
     * @brief Variadic constructor
     * @tparam Dims Integral types for dimension values
     * @param dims Dimension values as variadic arguments
     */
    template <Integral... Dims>
    Shape(Dims... dims) : m_dims{static_cast<value_type>(dims)...} {}

    // Copy & move constructors/assignment
    Shape(const Shape &) = default;
    Shape(Shape &&) noexcept = default;
    Shape &operator=(const Shape &) = default;
    Shape &operator=(Shape &&) noexcept = default;

    // Group: Element Access
    /// @{

    /**
     * @brief Subscript access (const)
     * @param index Dimension index [0, size()-1]
     * @return Const reference to dimension value
     * @pre index < size()
     */
    const_reference operator[](size_type index) const noexcept {
        assert(index < m_dims.size());
        return m_dims[index];
    }

    /**
     * @brief Subscript access (non-const)
     * @param index Dimension index [0, size()-1]
     * @return Reference to dimension value
     * @pre index < size()
     */
    reference operator[](size_type index) noexcept {
        assert(index < m_dims.size());
        return m_dims[index];
    }

    /**
     * @brief Bounds-checked element access (const)
     * @param index Dimension index [0, size()-1]
     * @return Const reference to dimension value
     * @throws std::out_of_range if index >= size()
     */
    const_reference at(size_type index) const { return m_dims.at(index); }

    /**
     * @brief Bounds-checked element access (non-const)
     * @param index Dimension index [0, size()-1]
     * @return Reference to dimension value
     * @throws std::out_of_range if index >= size()
     */
    reference at(size_type index) { return m_dims.at(index); }

    /// @brief First dimension value (const)
    const_reference front() const noexcept { return m_dims.front(); }

    /// @brief First dimension value
    reference front() noexcept { return m_dims.front(); }

    /// @brief Last dimension value (const)
    const_reference back() const noexcept { return m_dims.back(); }

    /// @brief Last dimension value
    reference back() noexcept { return m_dims.back(); }
    /// @}

    // Group: Size and Capacity
    /// {@

    /// @brief Returns number of dimensions
    size_type ndim() const noexcept { return m_dims.size(); }

    /// @brief Returns number of dimensions
    size_type size() const noexcept { return m_dims.size(); }

    /// @brief Returns maximum possible size
    size_type max_size() const noexcept { return m_dims.max_size(); }

    /// @brief Returns true if no dimensions
    bool empty() const noexcept { return m_dims.empty(); }

    /// @brief Returns current capacity
    size_type capacity() const noexcept { return m_dims.capacity(); }

    /**
     * @brief Reserve storage for dimensions
     * @param new_cap New capacity
     */
    void reserve(size_type new_cap) { m_dims.reserve(new_cap); }

    /// @brief Reduce capacity to fit current size
    void shrink_to_fit() { m_dims.shrink_to_fit(); }

    /**
     * @brief Compute total number of elements
     * @return Product of all dimension sizes, 0 if empty
     */
    size_type numel() const noexcept {
        if (m_dims.empty()) return 0;
        return std::accumulate(m_dims.begin(), m_dims.end(), size_type{1}, std::multiplies{});
    }
    /// @}

    // Group: Modifiers
    /// @{

    /// @brief Remove all dimensions
    void clear() noexcept { m_dims.clear(); }

    /**
     * @brief Add dimension to the end (copy)
     * @param value Dimension size to add
     */
    void push_back(const value_type &value) { m_dims.push_back(value); }

    /**
     * @brief Add dimension to the end (move)
     * @param value Dimension size to add
     */
    void push_back(value_type &&value) { m_dims.push_back(std::move(value)); }

    /**
     * @brief Construct dimension in-place at the end
     * @tparam Args Argument types for construction
     * @param args Arguments for dimension construction
     * @return Reference to the new dimension
     */
    template <typename... Args>
    reference emplace_back(Args &&...args) {
        return m_dims.emplace_back(std::forward<Args>(args)...);
    }

    /// @brief Remove last dimension
    void pop_back() { m_dims.pop_back(); }

    /**
     * @brief Resize the number of dimensions
     * @param count New number of dimensions
     */
    void resize(size_type count) { m_dims.resize(count); }

    /**
     * @brief Resize the number of dimensions with value
     * @param count New number of dimensions
     * @param value Value for new dimensions
     */
    void resize(size_type count, const value_type &value) { m_dims.resize(count, value); }

    /**
     * @brief Remove dimension at position
     * @param pos Iterator to dimension to remove
     * @return Iterator following the removed dimension
     */
    iterator erase(const_iterator pos) { return m_dims.erase(pos); }

    /**
     * @brief Remove range of dimensions
     * @param first First dimension to remove
     * @param last One past last dimension to remove
     * @return Iterator following the last removed dimension
     */
    iterator erase(const_iterator first, const_iterator last) { return m_dims.erase(first, last); }
    /// @}

    // Group: Iterators
    /// @{

    /// @brief Begin iterator (const)
    const_iterator begin() const noexcept { return m_dims.begin(); }

    /// @brief End iterator (const)
    const_iterator end() const noexcept { return m_dims.end(); }

    /// @brief Begin iterator
    iterator begin() noexcept { return m_dims.begin(); }

    /// @brief End iterator
    iterator end() noexcept { return m_dims.end(); }

    /// @brief Const begin iterator
    const_iterator cbegin() const noexcept { return m_dims.cbegin(); }

    /// @brief Const end iterator
    const_iterator cend() const noexcept { return m_dims.cend(); }
    /// @}

    // Group: Data Access
    /// @{

    /// @brief Direct data access (const)
    const_pointer data() const noexcept { return m_dims.data(); }

    /// @brief Direct data ccess
    pointer data() noexcept { return m_dims.data(); }
    /// @}

    // Group: Operators
    /// @{

    /**
     * @brief Equality comparison
     * @param other Shape to compare with
     * @return true if all dimensions are equal
     */
    bool operator==(const Shape &other) const noexcept { return m_dims == other.m_dims; }

    /**
     * @brief Inequality comparison
     * @param other Shape to compare with
     * @return true if any dimension differs
     */
    bool operator!=(const Shape &other) const noexcept { return !(*this == other); }

    /**
     * @brief Three-way comparison (lexicographical ordering)
     * @param other Shape to compare with
     * @return std::strong_ordering result
     */
    auto operator<=>(const Shape &other) const noexcept { return m_dims <=> other.m_dims; }
    /// @}

    // Group: Utility methods
    /// @{

    /// @brief Returns true for 0-dimensional shape
    bool is_scalar() const noexcept { return m_dims.empty(); }

    /// @brief Returns true for 1-dmensional shape
    bool is_vector() const noexcept { return m_dims.size() == 1; }

    /// @brief Returns true for 2-dimensional shape
    bool is_matrix() const noexcept { return m_dims.size() == 2; }

    /**
     * @brief Convert to static shape with runtime check
     *
     * @tparam N Target number of dimensions
     * @return Result containing static shape or error
     * @throws Nothing (returns Error instead of throwing)
     *
     * @par Example:
     * @code
     * tensor::Shape<0> dynamic_shape{2, 3, 4};
     * auto result = dynamic_shape.to_static<3>();
     * if (result) {
     *     tensor::Shape<3> static_shape = result.unwrap();
     * }
     * @endcode
     */
    template <std::size_t N>
    error::Result<Shape<N>, error::Error> to_static() const {
        if (m_dims.size() != N) {
            return error::Err<Shape<N>>(
                error::Error(error::ErrorCode::ShapeMismatch, "Shape dimension count mismatch"));
        }

        Shape<N> result;
        std::copy(m_dims.begin(), m_dims.end(), result.begin());
        return error::Ok(result);
    }

    /// @}

  private:
    std::vector<value_type> m_dims;
};

/**
 * @brief Type aliases for common shapes
 * @ingroup Shape
 */
using DynamicShape = Shape<0>;  ///< Shape with dynamic dims
using Shape1D = Shape<1>;       ///< 1-dimensional shape (vector)
using Shape2D = Shape<2>;       ///< 2-dimensional shape (matrix)
using Shape3D = Shape<3>;       ///< 3-dimensional shape
using Shape4D = Shape<4>;       ///< 4-dimensional shape

/**
 * @brief Deduction guide for variadic integral arguments.
 *
 * Allows template argument deduction when constructing `Shape` from a list
 * of integral values. Deduces the template parameter N from the number of
 * arguments.
 *
 * @tparam Dims Integral types of the dimension values.
 * @par Example:
 * @code {cpp}
 * Shape shape{2, 3, 4}; // Deduces Shape<3> from 3 arguments
 * Shape shape(5, 6);    // Deduces Shape<2> from 2 arguments
 * @endcode
 *
 * @note This guide is preferred over the initializer_list guide when the
 *       number of arguments is known at the call state.
 */
template <Integral... Dims>
Shape(Dims...) -> Shape<sizeof...(Dims)>;

/**
 * @brief Deduction guide for initializer lists
 *
 * Allows template argument deduction when constructing Shape from an
 * initializer list. Always deduces Shape<0> (dynamic shape) because
 * the size of an initializer list is only known at runtime.
 *
 * @par Example:
 * @code
 * Shape shape = {2, 3, 4};  // Deduces Shape<0> (dynamic)
 * Shape shape{1, 2, 3, 4};  // Deduces Shape<0> (dynamic)
 * @endcode
 *
 * @note This guide is used when brace-initialization syntax is employed.
 *       The dynamic shape is chosen because initializer_list size is
 *       not a compile-time constant.
 */
Shape(std::initializer_list<std::size_t>) -> Shape<0>;

/**
 * @brief Deduction guide for integral ranges
 *
 * Allows template argument deduction when constructing Shape from any range
 * containing integral values. Always deduces Shape<0> (dynamic shape) because
 * range sizes are typically determined at runtime.
 *
 * @tparam R Range type satisfying the IntegralRange concept
 *
 * @par Example:
 * @code
 * std::vector<int> dims{2, 3, 4};
 * Shape shape(dims);          // Deduces Shape<0> from vector
 *
 * std::array<size_t, 3> arr{1, 2, 3};
 * Shape shape(arr);           // Deduces Shape<0> from array
 *
 * int c_array[] = {4, 5, 6};
 * Shape shape(c_array);       // Deduces Shape<0> from C array
 * @endcode
 *
 * @note This guide handles any range type, including containers, arrays,
 *       and spans. The dynamic shape is chosen because range sizes are
 *       generally not compile-time constants in the general case.
 */
template <IntegralRange R>
Shape(const R &) -> Shape<0>;

/**
 * @brief Create a static shape from variadic integral arguments.
 *
 * Factory function that creates a `Shape` with static dimensions determined
 * by the number of arguments. The shape type is deduced at compile-time.
 *
 * @tparam Dims Integral types of the dimension values
 * @param dims Dimension sizes as variadic arguments
 * @return Shape<N> where N is the number of arguments
 * @constexpr Can be evaluated at compile-time if arguments are constant
 *
 * @par Example:
 * @code
 * auto static_shape = make_shape(2, 3, 4);  // Returns Shape<3>{2, 3, 4}
 * constexpr auto fixed_shape = make_shape(5, 6);  // Compile-time Shape<2>
 * @endcode
 *
 * @note Prefer this over constructor syntax when you want explicit static
 *       shape creation without template parameter syntax.
 */
template <Integral... Dims>
constexpr auto make_shape(Dims... dims) {
    return Shape<sizeof...(Dims)>(dims...);
}

/**
 * @brief Create a dynamic shape from an initializer list
 *
 * Factory function that creates a dynamic Shape (Shape<0>) from an
 * initializer list of dimension sizes.
 *
 * @param dims Initializer list containing dimension sizes
 * @return Shape<0> with the specified dimensions
 *
 * @par Example:
 * @code
 * auto dynamic_shape = make_dynamic_shape({2, 3, 4});  // Shape<0>{2, 3, 4}
 * auto empty_shape = make_dynamic_shape({});           // Shape<0> (scalar)
 * @endcode
 *
 * @note Useful when you explicitly want a dynamic shape regardless of
 *       the number of dimensions provided.
 */
inline auto make_dynamic_shape(std::initializer_list<size_t> dims) { return Shape<0>(dims); }

/**
 * @brief Create a dynamic shape from any integral range
 *
 * Factory function that creates a dynamic Shape (Shape<0>) from any
 * range or container containing integral dimension values.
 *
 * @tparam R Range type satisfying the IntegralRange concept
 * @param range Range containing dimension sizes
 * @return Shape<0> with dimensions copied from the range
 *
 * @par Example:
 * @code
 * std::vector<int> dims{2, 3, 4};
 * auto from_vector = make_dynamic_shape(dims);  // Shape<0>{2, 3, 4}
 *
 * std::array<size_t, 2> arr{5, 6};
 * auto from_array = make_dynamic_shape(arr);    // Shape<0>{5, 6}
 *
 * int c_array[] = {7, 8, 9};
 * auto from_c_array = make_dynamic_shape(c_array); // Shape<0>{7, 8, 9}
 * @endcode
 *
 * @note This is the most flexible factory function, accepting any
 *       container, array, or range that provides integral values.
 */
template <IntegralRange R>
auto make_dynamic_shape(const R &range) {
    return Shape<0>(range);
}

// Shape manipulation functions
/**
 * @brief Concatenate two static shapes
 *
 * @tparam N Dimension count of first shape
 * @tparam M Dimension count of second shape
 * @param lhs First shape to concatenate
 * @param rhs Second shape to concatenate
 *
 * @return New Shap<N + M> containing dimensions from both shapes
 */
template <std::size_t N, std::size_t M>
constexpr Shape<N + M> concatenate_shapes(const Shape<N> &lhs, const Shape<M> &rhs) {
    Shape<N + M> result;

    std::copy(lhs.begin(), lhs.end(), result.begin());
    std::copy(lhs.begin(), rhs.end(), result.begin() + N);
    return result;
}

/**
 * @brief Concatenate two dynamic shapes
 *
 * @param lhs First shape to concatenate
 * @param rhs Second shape to concatenate
 * @return New Shape<0> containing dimensions from both shapes
 */
inline Shape<0> concatenate_shapes(const Shape<0> &lhs, const Shape<0> &rhs) {
    Shape<0> result;
    result.reserve(lhs.size() + rhs.size());
    result.reserve(lhs.size());

    std::copy(lhs.begin(), lhs.end(), result.begin());
    std::copy(rhs.begin(), rhs.end(), std::back_inserter(result));
    return result;
}

/**
 * @brief Add dimension to the beginning of a static shape
 *
 * @tparam N Original dimension count
 * @param shape Original shape
 * @param dim Dimension value to prepend
 * @return New Shape<N + 1> with added dimension
 */
template <std::size_t N>
constexpr Shape<N + 1> prepend_dimensions(const Shape<N> &shape, std::size_t dim) {
    Shape<N + 1> result;
    result[0] = dim;

    std::copy(shape.begin(), shape.end(), result.begin() + 1);
    return result;
}

/**
 * @brief Add dimension to the beginning of a dynamic shape
 *
 * @param shape Original shape
 * @param dim Dimension value to prepend
 * @return New Shape<0> with added dimension
 */
inline Shape<0> prepend_dimension(const Shape<0> &shape, std::size_t dim) {
    Shape<0> result;
    result.reserve(shape.size() + 1);
    result.push_back(dim);

    std::copy(shape.begin(), shape.end(), std::back_inserter(result));
    return result;
}

/**
 * @brief Add dimension to the end of a static shape
 *
 * @tparam N Original dimension count
 * @param shape Original shape
 * @param dim Dimension value to append
 * @return New Shape<N + 1> with added dimension
 * @constexpr Can be evaluated at compile-time
 */
template <std::size_t N>
constexpr Shape<N + 1> append_dimension(const Shape<N> &shape, std::size_t dim) {
    Shape<N + 1> result;
    std::copy(shape.begin(), shape.end(), result.begin());
    result[N] = dim;

    return result;
}

/**
 * @brief Add dimension to the end of a dynamic shape
 *
 * @param shape Original shape
 * @param dim Dimension value to append
 * @return New Shape<0> with added dimension
 */
inline Shape<0> append_dimension(const Shape<0> &shape, std::size_t dim) {
    Shape<0> result = shape;
    result.push_back(dim);

    return result;
}

/**
 * @brief Remove dimension at specified index from static shape
 *
 * @tparam N Original dimension count
 * @param shape Original shape
 * @param index Index of dimension to remove [0, N-1]
 * @return New Shape<N - 1> with dimension removed
 * @pre index < N
 * @constexpr Can be evaluated at compile-time
 */
template <std::size_t N>
constexpr Shape<N - 1> remove_dimension(const Shape<N> &shape, std::size_t index)
    requires(N > 0)
{
    assert(index < N);
    Shape<N - 1> result;

    std::copy(shape.begin(), shape.begin() + index, result.begin());
    std::copy(shape.begin() + index + 1, shape.end(), result.begin() + index);
    return result;
}

/**
 * @brief Remove dimension at specified index from dynamic shape
 *
 * @param shape Original shape
 * @param index Index of dimension to remove [0, size()-1]
 * @return Result containing new Shape<0> or error if index is invalid
 */
inline error::Result<Shape<0>, error::Error> remove_dimension(const Shape<0> &shape, std::size_t index) {
    if (index >= shape.size()) {
        return error::Err<Shape<0>>(error::Error(error::ErrorCode::InvalidIndex, "Dimension index out of range"));
    }

    Shape<0> result;
    result.reserve(shape.size() - 1);

    std::copy(shape.begin(), shape.begin() + index, std::back_inserter(result));
    std::copy(shape.begin() + index + 1, shape.end(), std::back_inserter(result));

    return error::Result<Shape<0>, error::Error>(std::move(result));
}

/**
 * @brief Remove dimensions of size 1 from static shape
 *
 * @tparam N Original dimension count
 * @param shape Original shape
 * @return New Shape<0> with size-1 dimensions removed
 */
template <std::size_t N>
Shape<0> squeeze(const Shape<N> &shape) {
    Shape<0> result;
    std::copy_if(shape.begin(), shape.end(), std::back_inserter(result), [](std::size_t dim) { return dim != 1; });

    return result;
}

/**
 * @brief Remove dimensions of size 1 from dynamic shape
 *
 * @param shape Original shape
 * @return New Shape<0> with size-1 dimensions removed
 */
inline Shape<0> squeeze(const Shape<0> &shape) {
    Shape<0> result;
    std::copy_if(shape.begin(), shape.end(), std::back_inserter(result), [](std::size_t dim) { return dim != 1; });

    return result;
}

/**
 * @brief Add dimensions of size 1 at specified axes to static shape
 *
 * @tparam N Original dimension count
 * @param shape Original shape
 * @param axes Positions where to insert new dimensions of size 1
 * @return New Shape<0> with added dimensions
 */
template <std::size_t N>
Shape<0> unsqueeze(const Shape<N> &shape, std::span<const std::size_t> axes) {
    Shape<0> result;
    std::size_t orig_idx = 0;
    std::size_t target_ndim = shape.size() + axes.size();

    for (std::size_t i = 0; i < target_ndim; ++i) {
        if (std::find(axes.begin(), axes.end(), i) != axes.end()) {
            result.push_back(1);
        } else {
            result.push_back(shape[orig_idx++]);
        }
    }

    return result;
}

/**
 * @brief Add dimensions of size 1 at specified axes to dynamic shape
 *
 * @param shape Original shape
 * @param axes Positions where to insert new dimensions of size 1
 * @return New Shape<0> with added dimensions
 */
inline Shape<0> unsqueeze(const Shape<0> &shape, std::span<const std::size_t> axes) {
    Shape<0> result;
    std::size_t orig_idx = 0;
    std::size_t target_ndim = shape.size() + axes.size();

    for (std::size_t i = 0; i < target_ndim; ++i) {
        if (std::find(axes.begin(), axes.end(), i) != axes.end()) {
            result.push_back(1);
        } else {
            result.push_back(shape[orig_idx++]);
        }
    }

    return result;
}

/**
 * @brief Check if two shapes are compatible for broadcasting
 *
 * @tparam S1 First shape type (must satisfy ShapeLike concept)
 * @tparam S2 Second shape type (must satisfy ShapeLike concept)
 * @param shape1 First shape to check
 * @param shape2 Second shape to check
 * @return true if shapes can be broadcast together, false otherwise
 */
template <ShapeLike S1, ShapeLike S2>
bool are_broadcastable(const S1 &shape1, const S2 &shape2) {
    std::size_t ndim1 = shape1.ndim();
    std::size_t ndim2 = shape2.ndim();
    std::size_t max_ndim = std::max(ndim1, ndim2);

    for (std::size_t i = 0; i < max_ndim; ++i) {
        std::size_t dim1 = (i < ndim1) ? shape1[ndim1 - 1 - i] : 1;
        std::size_t dim2 = (i < ndim2) ? shape2[ndim2 - 1 - i] : 1;

        if (dim1 != 1 && dim2 != 1 && dim1 != dim2) {
            return false;
        }
    }

    return true;
}

/**
 * @brief Compute the resulting shape from broadcasting two shapes
 *
 * @tparam S1 First shape type (must satisfy ShapeLike concept)
 * @tparam S2 Second shape type (must satisfy ShapeLike concept)
 * @param shape1 First shape to broadcast
 * @param shape2 Second shape to broadcast
 * @return New Shape<0> representing the broadcasted shape
 */
template <ShapeLike S1, ShapeLike S2>
Shape<0> broadcast_shapes(const S1 &shape1, const S2 &shape2) {
    std::size_t ndim1 = shape1.ndim();
    std::size_t ndim2 = shape2.ndim();
    std::size_t max_ndim = std::max(ndim1, ndim2);

    Shape<0> result;
    result.reserve(max_ndim);

    for (std::size_t i = 0; i < max_ndim; ++i) {
        std::size_t dim1 = (i < ndim1) ? shape1[ndim1 - 1 - i] : 1;
        std::size_t dim2 = (i < ndim2) ? shape2[ndim2 - 1 - i] : 1;

        if (dim1 == 1) {
            result.push_back(dim2);
        } else if (dim2 == 1) {
            result.push_back(dim1);
        } else if (dim1 == dim2) {
            result.push_back(dim1);
        } else {
            result.clear();
            break;
        }
    }

    std::reverse(result.begin(), result.end());
    return result;
}

/**
 * @brief Compute row-major (C-order) strides for a shape
 *
 * @tparam S Shape type (must satisfy ShapeLike concept)
 * @param shape Input shape
 * @return Shape<0> containing the strides for each dimension
 */
template <ShapeLike S>
Shape<0> compute_strides(const S &shape) {
    if (shape.empty()) {
        return Shape<0>{};
    }

    Shape<0> strides;
    strides.resize(shape.size());

    strides.back() = 1;
    for (std::size_t i = shape.size() - 1; i > 0; --i) {
        strides[i - 1] = strides[i] * shape[i];
    }

    return strides;
}

/**
 * @brief Compute column-major (Fortran-order) strides for a shape
 *
 * @tparam S Shape type (must satisfy ShapeLike concept)
 * @param shape Input shape
 * @return Shape<0> containing the strides for each dimension
 */
template <ShapeLike S>
Shape<0> compute_strides_fortran(const S &shape) {
    if (shape.empty()) {
        return Shape<0>{};
    }

    Shape<0> strides;
    strides.resize(shape.size());

    strides[0] = 1;
    for (std::size_t i = 1; i < shape.size(); ++i) {
        strides[i] = strides[i - 1] * shape[i - 1];
    }

    return strides;
}

/**
 * @brief Convert multi-dimensional index to flat index (row-major)
 *
 * @tparam S Shape type (must satisfy ShapeLike concept)
 * @tparam Indices Index range type (must satisfy IntegralRange concept)
 * @param indices Multi-dimensional indices
 * @param shape Shape of the array
 * @return Flat index in row-major order
 * @pre indices.size() == shape.size()
 */
template <ShapeLike S, IntegralRange Indices>
std::size_t ravel_multi_index(const Indices &indices, const S &shape) {
    assert(std::ranges::size(indices) == shape.size());

    std::size_t flat_index = 0;
    std::size_t stride = 1;

    auto idx_it = std::ranges::rbegin(indices);
    for (std::size_t i = shape.size(); i > 0; --i) {
        flat_index += (*idx_it) * stride;
        stride *= shape[i - 1];
        ++idx_it;
    }

    return flat_index;
}

/**
 * @brief Convert flat index to multi-dimensional index (row-major)
 * @tparam S Shape type (must satisfy ShapeLike concept)
 * @param flat_index Flat index to convert
 * @param shape Shape of the array
 * @return Shape<0> containing multi-dimensional indices
 */
template <ShapeLike S>
Shape<0> unravel_index(std::size_t flat_index, const S &shape) {
    Shape<0> indices;
    indices.resize(shape.size());

    for (std::size_t i = shape.size(); i > 0; --i) {
        indices[i - 1] = flat_index % shape[i - 1];
        flat_index /= shape[i - 1];
    }

    return indices;
}

/**
 * @brief Stream output operator for shapes
 *
 * @tparam N Dimension count
 * @param os Output stream
 * @param shape Shape to output
 * @return Reference to the output stream
 */
template <std::size_t N>
std::ostream &operator<<(std::ostream &os, const Shape<N> &shape) {
    os << "Shape(";
    for (std::size_t i = 0; i < shape.size(); ++i) {
        if (i > 0) os << ", ";
        os << shape[i];
    }

    os << ")";
    return os;
}
}  // namespace brezel::core