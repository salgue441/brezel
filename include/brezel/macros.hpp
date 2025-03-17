/**
 * @file macros.hpp
 * @author Carlos Salguero
 * @brief Common macros for the Brezel Tensor Framework.
 * @date 2025-03-16
 *
 * This file contains macros used throughout the Brezel tensor framework
 * for optimization, compiler compatibility, and common functionality.
 */

#pragma once

#include <cstdint>
#include <string>
#include <type_traits>

// Version information
#define BREZEL_MAJOR_VERSION 0
#define BREZEL_MINOR_VERSION 1
#define BREZEL_PATCH_VERSION 0
#define BREZEL_VERSION_STRING "0.1.0"

// Compiler detection
#if defined(_MSC_VER)
#define BREZEL_COMPILER_MSVC
#elif defined(__clang__)
#define BREZEL_COMPILER_CLANG
#elif defined(__GNUC__)
#define BREZEL_COMPILER_GCC
#endif

// OS detection
#if defined(_WIN32) || defined(_WIN64)
#define BREZEL_OS_WINDOWS
#elif defined(__APPLE__)
#define BREZEL_OS_MACOS
#elif defined(__linux__)
#define BREZEL_OS_LINUX
#endif

// Architecture detection
#if defined(__x86_64__) || defined(_M_X64)
#define BREZEL_ARCH_X64
#elif defined(__aarch64__) || defined(_M_ARM64)
#define BREZEL_ARCH_ARM64
#endif

// Visibility and export macros
#if defined(BREZEL_OS_WINDOWS)
#define BREZEL_EXPORT __declspec(dllexport)
#define BREZEL_IMPORT __declspec(dllimport)
#else
#define BREZEL_EXPORT __attribute__((visibility("default")))
#define BREZEL_IMPORT __attribute__((visibility("default")))
#endif

#if defined(BREZEL_BUILD_SHARED_LIBS)
#if defined(BREZEL_BUILDING_LIBRARY)
#define BREZEL_API BREZEL_EXPORT
#else
#define BREZEL_API BREZEL_IMPORT
#endif
#else
#define BREZEL_API
#endif

// Function inlining
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_ALWAYS_INLINE __forceinline
#define BREZEL_NEVER_INLINE __declspec(noinline)
#else
#define BREZEL_ALWAYS_INLINE __attribute__((always_inline)) inline
#define BREZEL_NEVER_INLINE __attribute__((noinline))
#endif

// Branch prediction hints
#if defined(BREZEL_COMPILER_GCC) || defined(BREZEL_COMPILER_CLANG)
#define BREZEL_LIKELY(x) __builtin_expect(!!(x), 1)
#define BREZEL_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define BREZEL_LIKELY(x) (x)
#define BREZEL_UNLIKELY(x) (x)
#endif

// Alignment
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_ALIGNED(x) __declspec(align(x))
#else
#define BREZEL_ALIGNED(x) __attribute__((aligned(x)))
#endif

// Unreachable code
#if defined(BREZEL_COMPILER_CLANG) || defined(BREZEL_COMPILER_GCC)
#define BREZEL_UNREACHABLE() __builtin_unreachable()
#elif defined(BREZEL_COMPILER_MSVC)
#define BREZEL_UNREACHABLE() __assume(0)
#else
#define BREZEL_UNREACHABLE() \
    do {                     \
    } while (0)
#endif

// Debug vs Release
#if !defined(NDEBUG)
#define BREZEL_DEBUG
#endif

// Assert macros
#ifdef BREZEL_DEBUG
#include <cassert>
#define BREZEL_ASSERT(condition, message) assert((condition) && (message))
#define BREZEL_DEBUG_ONLY(x) x
#else
#define BREZEL_ASSERT(condition, message) \
    do {                                  \
    } while (0)
#define BREZEL_DEBUG_ONLY(x)
#endif

// Logging macros (to be implemented with the logger)
#define BREZEL_LOG_TRACE(...)
#define BREZEL_LOG_DEBUG(...)
#define BREZEL_LOG_INFO(...)
#define BREZEL_LOG_WARN(...)
#define BREZEL_LOG_ERROR(...)
#define BREZEL_LOG_CRITICAL(...)

// Deprecation
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_DEPRECATED(msg) __declspec(deprecated(msg))
#else
#define BREZEL_DEPRECATED(msg) __attribute__((deprecated(msg)))
#endif

// Unused parameter/variable
#define BREZEL_UNUSED(x) (void)(x)

// SIMD detection
#if defined(__SSE4_2__)
#define BREZEL_HAS_SSE4_2
#endif

#if defined(__AVX__)
#define BREZEL_HAS_AVX
#endif

#if defined(__AVX2__)
#define BREZEL_HAS_AVX2
#endif

#if defined(__AVX512F__)
#define BREZEL_HAS_AVX512
#endif

// Memory alignment for SIMD operations
#define BREZEL_DEFAULT_ALIGNMENT 64

// Restrict keyword
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_RESTRICT __restrict
#else
#define BREZEL_RESTRICT __restrict__
#endif

// Function signature and source location
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_FUNCTION_NAME __FUNCSIG__
#define BREZEL_FUNCTION_SIGNATURE __FUNCSIG__
#else
#define BREZEL_FUNCTION_NAME __func__
#define BREZEL_FUNCTION_SIGNATURE __PRETTY_FUNCTION__
#endif

#define BREZEL_FILE __FILE__
#define BREZEL_LINE __LINE__

// String conversion and concatenation
#define BREZEL_STRINGIFY(x) #x
#define BREZEL_TOSTRING(x) BREZEL_STRINGIFY(x)
#define BREZEL_CONCAT_IMPL(a, b) a##b
#define BREZEL_CONCAT(a, b) BREZEL_CONCAT_IMPL(a, b)
#define BREZEL_UNIQUE_NAME(prefix) BREZEL_CONCAT(prefix, __LINE__)

// C++20 concepts support
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
#define BREZEL_HAS_CONCEPTS
#define BREZEL_CONCEPT concept
#else
#define BREZEL_CONCEPT
#endif

// CUDA support
#ifdef BREZEL_ENABLE_CUDA
#define BREZEL_HOST_DEVICE __host__ __device__
#define BREZEL_HOST __host__
#define BREZEL_DEVICE __device__
#else
#define BREZEL_HOST_DEVICE
#define BREZEL_HOST
#define BREZEL_DEVICE
#endif

// Execution policies
#define BREZEL_EXEC_SEQ 0        // Sequential execution
#define BREZEL_EXEC_PAR 1        // Parallel execution
#define BREZEL_EXEC_PAR_UNSEQ 2  // Parallel unsequenced execution

// Class property macros
#define BREZEL_DISALLOW_COPY(TypeName)  \
    TypeName(const TypeName&) = delete; \
    TypeName& operator=(const TypeName&) = delete

#define BREZEL_DISALLOW_MOVE(TypeName) \
    TypeName(TypeName&&) = delete;     \
    TypeName& operator=(TypeName&&) = delete

#define BREZEL_DISALLOW_COPY_AND_MOVE(TypeName) \
    BREZEL_DISALLOW_COPY(TypeName);             \
    BREZEL_DISALLOW_MOVE(TypeName)

// PyTorch-like tensor options macro
#define BREZEL_TENSOR_OPTIONS(...) ::brezel::TensorOptions(__VA_ARGS__)