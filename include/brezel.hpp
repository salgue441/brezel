/**
 * @file brezel.hpp
 * @author your name (you@domain.com)
 * @brief
 * @version 0.1
 * @date 2025-09-02
 *
 * @copyright Copyright (c) 2025
 *
 */

#pragma once

// Core types
#include "brezel/core/concepts.hpp"
#include "brezel/core/dtype.hpp"
#include "brezel/core/shape.hpp"
#include "brezel/core/storage.hpp"

// Utilities
#include "brezel/utils/error.hpp"
#include "brezel/utils/logging.hpp"

/**
 * @brief Main tensor namespace (brezel)
 *
 * All tensor framework functionality is contained within the tensor namespace.
 * This includes core classes like Tensor, Shape, and Storage, as well as
 * utility functions and operators.
 */
namespace brezel {
/**
 * @brief Version information
 */
namespace version {
constexpr int major = BREZEL_VERSION_MAJOR;
constexpr int minor = BREZEL_VERSION_MINOR;
constexpr int patch = BREZEL_VERSION_PATCH;
constexpr const char* string = "1.0.0";
constexpr bool is_debug_build =
#ifdef BREZEL_BUILD
    true;
#else
    false;
#endif

constexpr bool has_simd_support =
#ifdef BREZEL_ENABLE_SIMD
    true;
#else
    false;
#endif

constexpr bool has_openmp_support =
#ifdef BREZEL_ENABLE_OPENMP
    true;
#else
    false;
#endif

}  // namespace version

/**
 * @brief Configuration information
 */
namespace config {
constexpr std::size_t default_alignment = BREZEL_ALIGNMENT;
constexpr std::size_t cache_line_size = 64;

/**
 * @brief Get runtime configuration information
 */
struct Info {
    std::string version;
    bool debug_build;
    bool simd_support;
    bool openmp_support;
    std::size_t alignment;
    std::size_t cache_line_size;

    Info()
        : version(version::string),
          debug_build(version::is_debug_build),
          simd_support(version::has_simd_support),
          openmp_support(version::has_openmp_support),
          alignment(default_alignment),
          cache_line_size(config::cache_line_size) {}
};

inline Info get_info() { return Info{}; }
}  // namespace config

/**
 * @brief Common type aliases for convenience
 */
using f32 = float;
using f64 = double;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

// Common tensor types
template <core::ScalarType T>
using Tensor1D = Tensor<T, 1>;

template <core::ScalarType T>
using Tensor2D = Tensor<T, 2>;

template <core::ScalarType T>
using Tensor3D = Tensor<T, 3>;

template <core::ScalarType T>
using Tensor4D = Tensor<T, 4>;

template <core::ScalarType T>
using TensorND = Tensor<T, 0>;

// Specific typed tensors
using FloatTensor = TensorND<float>;
using DoubleTensor = TensorND<double>;
using IntTensor = TensorND<int32_t>;
using LongTensor = TensorND<int64_t>;
using ByteTensor = TensorND<uint8_t>;
using BoolTensor = TensorND<bool>;

// 1D versions
using FloatTensor1D = Tensor1D<float>;
using DoubleTensor1D = Tensor1D<double>;
using IntTensor1D = Tensor1D<int32_t>;
using LongTensor1D = Tensor1D<int64_t>;

// 2D versions (matrices)
using FloatMatrix = Tensor2D<float>;
using DoubleMatrix = Tensor2D<double>;
using IntMatrix = Tensor2D<int32_t>;
using LongMatrix = Tensor2D<int64_t>;

/**
 * @brief Global initialization and cleanup
 */
namespace detail {
class GlobalState {
  public:
    GlobalState() { BREZEL_LOG_INFO("Tensor Framework initialized"); }
    ~GlobalState() { BREZEL_LOG_INFO("Tensor Framework shutting down"); }
};

inline void ensure_initialized() { static GlobalState state; }
}  // namespace detail

/**
 * @brief Initialize the tensor framework
 *
 * This function should be called before using the tensor framework.
 * It's safe to call multiple times.
 */
inline void initialize() { detail::ensure_initialized(); }

/**
 * @brief Set global configuration
 */
inline void set_num_threads(int num_threads) {
#ifdef BREZEL_ENABLE_OPENMP
    omp_set_num_threads(num_threads);
    BREZEL_LOG_INFO("Set number of threads to {}", num_threads);
#else
    (void)num_threads;
    BREZEL_LOG_WARN("OpenMP not enabled, cannot set number of threads");
#endif
}

inline int get_num_threads() {
#ifdef BREZEL_ENABLE_OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}
}  // namespace brezel