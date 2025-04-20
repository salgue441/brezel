/**
 * @file config.hpp
 * @author Carlos Salguero
 * @brief Configuration and feature detection for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file provides configuration options, feature detection,
 * platform-specific settings, and compile-time constants.
 */

#pragma once

#include <cstdint>
#include <string>
#include <string_view>

// Platform detection
#if defined(_WIN32) || defined(_WIN64)
    #define BREZEL_PLATFORM_WINDOWS 1
#elif defined(__APPLE__)
    #define BREZEL_PLATFORM_MACOS 1
#elif defined(__linux__)
    #define BREZEL_PLATFORM_LINUX 1
#else
    #define BREZEL_PLATFORM_UNKNOWN 1
#endif

// Compiler detection
#if defined(_MSC_VER)
    #define BREZEL_COMPILER_MSVC 1
#elif defined(__clang__)
    #define BREZEL_COMPILER_CLANG 1
#elif defined(__GNUC__)
    #define BREZEL_COMPILER_GCC 1
#else
    #define BREZEL_COMPILER_UNKNOWN 1
#endif

// Architecture detection
#if defined(__x86_64__) || defined(_M_X64)
    #define BREZEL_ARCH_X64 1
#elif defined(__aarch64__) || defined(_M_ARM64)
    #define BREZEL_ARCH_ARM64 1
#elif defined(__arm__) || defined(_M_ARM)
    #define BREZEL_ARCH_ARM32 1
#elif defined(__powerpc64__)
    #define BREZEL_ARCH_PPC64 1
#else
    #define BREZEL_ARCH_UNKNOWN 1
#endif

// C++ standard detection
#if __cplusplus >= 202002L
    #define BREZEL_CPP_20 1
#elif __cplusplus >= 201703L
    #define BREZEL_CPP_17 1
#else
    #error "brezel requires C++20 or later"
#endif

// SIMD extension detection
#if defined(__AVX512F__)
    #define BREZEL_SIMD_AVX512 1
    #define BREZEL_SIMD_WIDTH 64  // 512 bits = 64 bytes
#elif defined(__AVX2__)
    #define BREZEL_SIMD_AVX2 1
    #define BREZEL_SIMD_WIDTH 32  // 256 bits = 32 bytes
#elif defined(__AVX__)
    #define BREZEL_SIMD_AVX 1
    #define BREZEL_SIMD_WIDTH 32  // 256 bits = 32 bytes
#elif defined(__SSE4_2__)
    #define BREZEL_SIMD_SSE42 1
    #define BREZEL_SIMD_WIDTH 16  // 128 bits = 16 bytes
#elif defined(__ARM_NEON)
    #define BREZEL_SIMD_NEON 1
    #define BREZEL_SIMD_WIDTH 16  // 128 bits = 16 bytes
#else
    #define BREZEL_SIMD_NONE 1
    #define BREZEL_SIMD_WIDTH 1  // Scalar
#endif

// CUDA detection
#if defined(BREZEL_USE_CUDA) && BREZEL_USE_CUDA
    #define BREZEL_HAS_CUDA 1
#else
    #define BREZEL_HAS_CUDA 0
#endif

// OpenCL detection
#if defined(BREZEL_USE_OPENCL) && BREZEL_USE_OPENCL
    #define BREZEL_HAS_OPENCL 1
#else
    #define BREZEL_HAS_OPENCL 0
#endif

// ROCm/HIP detection
#if defined(BREZEL_USE_ROCM) && BREZEL_USE_ROCM
    #define BREZEL_HAS_ROCM 1
#else
    #define BREZEL_HAS_ROCM 0
#endif

// OneAPI detection
#if defined(BREZEL_USE_ONEAPI) && BREZEL_USE_ONEAPI
    #define BREZEL_HAS_ONEAPI 1
#else
    #define BREZEL_HAS_ONEAPI 0
#endif

// OpenMP detection
#if defined(_OPENMP)
    #define BREZEL_HAS_OPENMP 1
#else
    #define BREZEL_HAS_OPENMP 0
#endif

// Eigen detection
#if defined(BREZEL_USE_EIGEN) && BREZEL_USE_EIGEN
    #define BREZEL_HAS_EIGEN 1
#else
    #define BREZEL_HAS_EIGEN 0
#endif

// BLAS/LAPACK detection
#if defined(BREZEL_USE_BLAS) && BREZEL_USE_BLAS
    #define BREZEL_HAS_BLAS 1
#else
    #define BREZEL_HAS_BLAS 0
#endif

// Define debug/release status
#if defined(NDEBUG) || defined(RELEASE)
    #define BREZEL_DEBUG 0
#else
    #define BREZEL_DEBUG 1
#endif

// Define visibility attributes
#if defined(BREZEL_COMPILER_GCC) || defined(BREZEL_COMPILER_CLANG)
    #define BREZEL_EXPORT __attribute__((visibility("default")))
    #define BREZEL_IMPORT __attribute__((visibility("default")))
    #define BREZEL_HIDDEN __attribute__((visibility("hidden")))
#elif defined(BREZEL_COMPILER_MSVC)
    #define BREZEL_EXPORT __declspec(dllexport)
    #define BREZEL_IMPORT __declspec(dllimport)
    #define BREZEL_HIDDEN
#else
    #define BREZEL_EXPORT
    #define BREZEL_IMPORT
    #define BREZEL_HIDDEN
#endif

// Define API visibility based on build type
#if defined(BREZEL_BUILDING_LIBRARY)
    #define BREZEL_API BREZEL_EXPORT
#else
    #define BREZEL_API BREZEL_IMPORT
#endif

// Force inline attribute
#if defined(BREZEL_COMPILER_MSVC)
    #define BREZEL_FORCE_INLINE __forceinline
#elif defined(BREZEL_COMPILER_GCC) || defined(BREZEL_COMPILER_CLANG)
    #define BREZEL_FORCE_INLINE inline __attribute__((always_inline))
#else
    #define BREZEL_FORCE_INLINE inline
#endif

// Unreachable code marker
#if defined(BREZEL_COMPILER_GCC) || defined(BREZEL_COMPILER_CLANG)
    #if __cplusplus >= 202002L
        #define BREZEL_UNREACHABLE() std::unreachable()
    #else
        #define BREZEL_UNREACHABLE() __builtin_unreachable()
    #endif
#elif defined(BREZEL_COMPILER_MSVC)
    #define BREZEL_UNREACHABLE() __assume(0)
#else
    #define BREZEL_UNREACHABLE() \
        do {                     \
        } while (0)
#endif

// Likely/Unlikely hints
#if defined(BREZEL_COMPILER_GCC) || defined(BREZEL_COMPILER_CLANG)
    #define BREZEL_LIKELY(x) __builtin_expect(!!(x), 1)
    #define BREZEL_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
    #define BREZEL_LIKELY(x) (x)
    #define BREZEL_UNLIKELY(x) (x)
#endif

// Thread local storage
#define BREZEL_THREAD_LOCAL thread_local

// Define cacheline size (common default is 64 bytes)
#define BREZEL_CACHE_LINE_SIZE 64

// Define framework version
#define BREZEL_VERSION_MAJOR 0
#define BREZEL_VERSION_MINOR 1
#define BREZEL_VERSION_PATCH 0

namespace brezel {
/**
 * @brief Get the framework version string
 *
 * @return std::string Version string
 */
inline std::string get_version() {
    return std::to_string(BREZEL_VERSION_MAJOR) + "." + std::to_string(BREZEL_VERSION_MINOR) + "." +
           std::to_string(BREZEL_VERSION_PATCH);
}

/**
 * @brief Get platform name
 *
 * @return std::string_view Platform name
 */
constexpr std::string_view get_platform_name() {
#if defined(BREZEL_PLATFORM_WINDOWS)
    return "Windows";
#elif defined(BREZEL_PLATFORM_MACOS)
    return "macOS";
#elif defined(BREZEL_PLATFORM_LINUX)
    return "Linux";
#else
    return "Unknown";
#endif
}

/**
 * @brief Get compiler name
 *
 * @return std::string_view Compiler name
 */
constexpr std::string_view get_compiler_name() {
#if defined(BREZEL_COMPILER_MSVC)
    return "MSVC";
#elif defined(BREZEL_COMPILER_CLANG)
    return "Clang";
#elif defined(BREZEL_COMPILER_GCC)
    return "GCC";
#else
    return "Unknown";
#endif
}

/**
 * @brief Get architecture name
 *
 * @return std::string_view Architecture name
 */
constexpr std::string_view get_architecture_name() {
#if defined(BREZEL_ARCH_X64)
    return "x86_64";
#elif defined(BREZEL_ARCH_ARM64)
    return "ARM64";
#elif defined(BREZEL_ARCH_ARM32)
    return "ARM32";
#elif defined(BREZEL_ARCH_PPC64)
    return "PPC64";
#else
    return "Unknown";
#endif
}

/**
 * @brief Check if running in debug mode
 *
 * @return bool True if in debug mode
 */
constexpr bool is_debug_build() {
    return BREZEL_DEBUG == 1;
}

/**
 * @brief Check if CUDA support is available
 *
 * @return bool True if CUDA is available
 */
constexpr bool has_cuda_support() {
    return BREZEL_HAS_CUDA == 1;
}

/**
 * @brief Check if OpenCL support is available
 *
 * @return bool True if OpenCL is available
 */
constexpr bool has_opencl_support() {
    return BREZEL_HAS_OPENCL == 1;
}

/**
 * @brief Check if ROCm/HIP support is available
 *
 * @return bool True if ROCm/HIP is available
 */
constexpr bool has_rocm_support() {
    return BREZEL_HAS_ROCM == 1;
}

/**
 * @brief Check if OneAPI support is available
 *
 * @return bool True if OneAPI is available
 */
constexpr bool has_oneapi_support() {
    return BREZEL_HAS_ONEAPI == 1;
}

/**
 * @brief Check if OpenMP support is available
 *
 * @return bool True if OpenMP is available
 */
constexpr bool has_openmp_support() {
    return BREZEL_HAS_OPENMP == 1;
}

/**
 * @brief Check if Eigen support is available
 *
 * @return bool True if Eigen is available
 */
constexpr bool has_eigen_support() {
    return BREZEL_HAS_EIGEN == 1;
}

/**
 * @brief Check if BLAS/LAPACK support is available
 *
 * @return bool True if BLAS/LAPACK is available
 */
constexpr bool has_blas_support() {
    return BREZEL_HAS_BLAS == 1;
}

/**
 * @brief Get the SIMD width in bytes
 *
 * @return int SIMD width
 */
constexpr int get_simd_width() {
    return BREZEL_SIMD_WIDTH;
}

/**
 * @brief Get the SIMD extension name
 *
 * @return std::string_view SIMD extension name
 */
constexpr std::string_view get_simd_extension() {
#if defined(BREZEL_SIMD_AVX512)
    return "AVX-512";
#elif defined(BREZEL_SIMD_AVX2)
    return "AVX2";
#elif defined(BREZEL_SIMD_AVX)
    return "AVX";
#elif defined(BREZEL_SIMD_SSE42)
    return "SSE4.2";
#elif defined(BREZEL_SIMD_NEON)
    return "NEON";
#else
    return "None";
#endif
}
}  // namespace brezel