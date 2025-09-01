#pragma once

#include <concepts>
#include <iterator>
#include <ranges>
#include <type_traits>

#include "brezel/core/dtype.hpp"

namespace brezel::core {
/**
 * @brief Concept for arithmetic types
 */
template <typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

/**
 * @brief Concept for integral types
 */
template <typename T>
concept Integral = std::integral<T>;

/**
 * @brief Concept for floating point types
 */
template <typename T>
concept FloatingPoint = std::floating_point<T>;

/**
 * @brief Concept for signed types
 */
template <typename T>
concept Signed = std::signed_integral<T> || std::floating_point<T>;

/**
 * @brief Concept for unsigned types
 */
template <typename T>
concept Unsigned = std::unsigned_integral<T>;

/**
 * @brief Concept for numeric types (arithmetic + bool)
 */
template <typename T>
concept Numeric = Arithmetic<T> || std::same_as<T, bool>;

/**
 * @brief Concept for tensor-supported scalar types
 */
template <typename T>
concept ScalarType = SupportedType<T>;

/**
 * @brief Concept for index types
 */
template <typename T>
concept IndexType = std::signed_integral<T> && sizeof(T) >= sizeof(std::ptrdiff_t);

/**
 * @brief Concept for size types
 */
template <typename T>
concept SizeType = std::unsigned_integral<T> && sizeof(T) >= sizeof(std::size_t);

/**
 * @brief Concept for iterator-like types
 */
template <typename T>
concept Iterator = std::input_or_output_iterator<T>;

/**
 * @brief Concept for contiguous iterator
 */
template <typename T>
concept ContiguousIterator = std::contiguous_iterator<T>;

/**
 * @brief Concept for random access iterator
 */
template <typename T>
concept RandomAccessIterator = std::random_access_iterator<T>;

/**
 * @brief Concept for range types
 */
template <typename T>
concept Range = std::ranges::range<T>;

/**
 * @brief Concept for contiguous ranges
 */
template <typename T>
concept ContiguousRange = std::ranges::contiguous_range<T>;

/**
 * @brief Concept for sized ranges
 */
template <typename T>
concept SizedRange = std::ranges::sized_range<T>;

/**
 * @brief Concept for ranges of integral values
 */
template <typename T>
concept IntegralRange = Range<T> && Integral<std::ranges::range_value_t<T>>;

/**
 * @brief Concept for ranges of scalar values
 */
template <typename T>
concept ScalarRange = Range<T> && ScalarType<std::ranges::range_value_t<T>>;

/**
 * @brief Concept for callable types with specific arguments
 *
 * @tparam F The callable type.
 * @tparam Args The argument types that callable must accept.
 */
template <typename F, typename... Args>
concept Callable = std::invocable<F, Args...>;

/**
 * @brief Concept for predicate functions
 *
 * @tparam F The predicate type.
 * @tparam T The argument type the predicate must accept.
 */
template <typename F, typename T>
concept Predicate = std::predicate<F, T>;

/**
 * @brief Concept for unary functions that return non-void.
 *
 * @tparam F The function type.
 * @tparam T The argument type.
 */
template <typename F, typename T>
concept UnaryFunction = Callable<F, T> && !std::same_as<std::invoke_result_t<F, T>, void>;

/**
 * @brief Concept for binary functions that return non-void.
 *
 * @tparam F The function type.
 * @tparam T The first argument type.
 * @tparam U The second argument type (defaults to T).
 */
template <typename F, typename T, typename U = T>
concept BinaryFunction = Callable<F, T, U> && !std::same_as<std::invoke_result_t<F, T, U>, void>;

/**
 * @brief Concept for reduction functions (binary functions that combine two
 * values of type T into one T).
 *
 * @tparam F The reduction function type.
 * @tparam T The value type.
 */
template <typename F, typename T>
concept ReductionFunction = BinaryFunction<F, T, T> && std::same_as<std::invoke_result_t<F, T, T>, T>;

/**
 * @brief Concept for types convertible to a specific type.
 *
 * @tparam From The source type.
 * @tparam To The target type.
 */
template <typename From, typename To>
concept ConvertibleTo = std::convertible_to<From, To>;

/**
 * @brief Concept for types that are the same after removing cv-qualifiers and
 * references.
 *
 * @tparam T The first type.
 * @tparam U The second type.
 */
template <typename T, typename U>
concept SameAs = std::same_as<std::remove_cvref_t<T>, std::remove_cvref_t<U>>;

/**
 * @brief Concept for movable types
 */
template <typename T>
concept Movable = std::movable<T>;

/**
 * @brief Concept for copyable types
 */
template <typename T>
concept Copyable = std::copyable<T>;

/**
 * @brief Concept for default constructible types
 */
template <typename T>
concept DefaultConstructible = std::default_initializable<T>;

/**
 * @brief Concept for types constructible from specific arguments.
 *
 * @tparam T The type to construct.
 * @tparam Args The argument types for construction.
 */
template <typename T, typename... Args>
concept ConstructibleFrom = std::constructible_from<T, Args...>;

/**
 * @brief Concept for swappable types
 */
template <typename T>
concept Swappable = std::swappable<T>;

/**
 * @brief Concept for comparable types
 */
template <typename T>
concept Comparable = std::totally_ordered<T>;

/**
 * @brief Concept for hashable types (support std::hash specialization).
 */
template <typename T>
concept Hashable = requires(T t) {
    { std::hash<T>{}(t) } -> std::convertible_to<std::size_t>;
};

/**
 * @brief Concept for allocator types.
 *
 * @tparam A The allocator type.
 *
 * Requirements:
 * - Must define `value_type`
 * - Must provide `allocate(size_t)` returning `value_type*`
 * - Must provide `deallocate(value_type*, size_t)` returning `void`
 */
template <typename A>
concept Allocator = requires(A a, typename A::value_type* p, std::size_t n) {
    typename A::value_type;
    { a.allocate(n) } -> std::same_as<typename A::value_type*>;
    { a.deallocate(p, n) } -> std::same_as<void>;
};

/**
 * @brief Concept for memory pool types.
 *
 * @tparam P The memory pool type.
 *
 * Requirements:
 * - Must provide `allocate(size_t, size_t)` returning `void*`
 * - Must provide `deallocate(void*, size_t)` returning `void`
 * - Must provide `reset()` returning `void`
 */
template <typename P>
concept MemoryPool = requires(P p, std::size_t size, std::size_t alignment) {
    { p.allocate(size, alignment) } -> std::convertible_to<void*>;
    { p.deallocate(std::declval<void*>(), size) } -> std::same_as<void>;
    { p.reset() } -> std::same_as<void>;
};

/**
 * @brief Concept for shape-like types.
 *
 * @tparam S The shape type.
 *
 * Requirements:
 * - Must provide `ndim()` returning number of dimensions
 * - Must provide `size()` returning total number of elements
 * - Must provide subscript access `[size_t]` returning dimension size
 * - Must provide `data()` returning pointer to dimension data
 */
template <typename S>
concept ShapeLike = requires(S s, std::size_t i) {
    { s.ndim() } -> std::convertible_to<std::size_t>;
    { s.size() } -> std::convertible_to<std::size_t>;
    { s[i] } -> std::convertible_to<std::size_t>;
    { s.data() } -> std::convertible_to<const std::size_t*>;
};

/**
 * @brief Concept for stride-like types (same requirements as ShapeLike).
 */
template <typename S>
concept StrideLike = ShapeLike<S>;

/**
 * @brief Concept for tensor-like types.
 *
 * @tparam T The tensor type.
 *
 * Requirements:
 * - Must define `value_type`
 * - Must provide `shape()` returning ShapeLike
 * - Must provide `dtype()` returning DType
 * - Must provide `data()` returning pointer to underlying data
 * - Must provide `size()` returning total number of elements
 * - Must provide `ndim()` returning number of dimensions
 */
template <typename T>
concept TensorLike = requires(T t) {
    typename T::value_type;
    { t.shape() } -> ShapeLike;
    { t.dtype() } -> std::same_as<DType>;
    { t.data() } -> std::convertible_to<const void*>;
    { t.size() } -> std::convertible_to<std::size_t>;
    { t.ndim() } -> std::convertible_to<std::size_t>;
};

/**
 * @brief Concept for mutable tensor-like types.
 *
 * Extends TensorLike with mutable data access requirements.
 */
template <typename T>
concept MutableTensorLike = TensorLike<T> && requires(T t) {
    { t.data() } -> std::convertible_to<void*>;
};

/**
 * @brief Concept for storage types.
 *
 * @tparam S The storage type.
 *
 * Requirements:
 * - Must define `value_type`
 * - Must provide `data()` returning pointer to stored data
 * - Must provide `size()` returning number of elements
 * - Must provide subscript access `[size_t]` returning element reference
 */
template <typename S>
concept StorageLike = requires(S s, std::size_t i) {
    typename S::value_type;
    { s.data() } -> std::convertible_to<const typename S::value_type*>;
    { s.size() } -> std::convertible_to<std::size_t>;
    { s[i] } -> std::convertible_to<const typename S::value_type&>;
};

/**
 * @brief Concept for mutable storage types.
 *
 * Extends StorageLike with mutable data access requirements.
 */
template <typename S>
concept MutableStorageLike = StorageLike<S> && requires(S s, std::size_t i) {
    { s.data() } -> std::convertible_to<typename S::value_type*>;
    { s[i] } -> std::convertible_to<typename S::value_type&>;
};

/**
 * @brief Type trait helpers for SFINAE.
 *
 * @internal
 * This namespace contains implementation details for type traits.
 */
namespace detail {
template <typename T>
struct is_tensor_like : std::bool_constant<TensorLike<T>> {};

template <typename T>
struct is_shape_like : std::bool_constant<ShapeLike<T>> {};

template <typename T>
struct is_scalar_type : std::bool_constant<ScalarType<T>> {};
}  // namespace detail

/**
 * @brief Variable template for checking if a type is tensor-like.
 *
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr bool is_tensor_like_v = detail::is_tensor_like<T>::value;

/**
 * @brief Variable template for checking if a type is shape-like.
 *
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr bool is_shape_like_v = detail::is_shape_like<T>::value;

/**
 * @brief Variable template for checking if a type is a scalar type.
 *
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr bool is_scalar_type_v = detail::is_scalar_type<T>::value;

/**
 * @brief Enable if helper for tensor-like types (pre-C++20 compatibility).
 *
 * @tparam T The type to condition on.
 */
template <typename T>
using enable_if_tensor_like_t = std::enable_if_t<is_tensor_like_v<T>>;

/**
 * @brief Enable if helper for shape-like types (pre-C++20 compatibility).
 *
 * @tparam T The type to condition on.
 */
template <typename T>
using enable_if_shape_like_t = std::enable_if_t<is_shape_like_v<T>>;

/**
 * @brief Enable if helper for scalar types (pre-C++20 compatibility).
 *
 * @tparam T The type to condition on.
 */
template <typename T>
using enable_if_scalar_type_t = std::enable_if_t<is_scalar_type_v<T>>;

}  // namespace brezel::core
