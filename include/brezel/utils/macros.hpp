#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <tuple>

// Version information
#define BREZEL_VERSION_MAJOR 0
#define BREZEL_VERSION_MINOR 1
#define BREZEL_VERSION_PATCH 0
#define BREZEL_VERSION_STRING "0.1.0"

// Determine if we're in debug mode
#if !defined(NDEBUG) || defined(_DEBUG)
#define BREZEL_DEBUG_MODE 1
#else
#define BREZEL_DEBUG_MODE 0
#endif

// Platform detection
#if defined(_WIN32) || defined(_WIN64)
#define BREZEL_PLATFORM_WINDOWS 1
#elif defined(__linux__)
#define BREZEL_PLATFORM_LINUX 1
#elif defined(__APPLE__) && defined(__MACH__)
#define BREZEL_PLATFORM_MACOS 1
#else
#define BREZEL_PLATFORM_UNKNOWN 1
#endif

// Compiler detection
#if defined(_MSC_VER)
#define BREZEL_COMPILER_MSVC 1
#if _MSC_VER < 1929
#error "Minimum required MSVC version is Visual Studio 2019 16.10 (supports C++20)"
#endif
#elif defined(__clang__)
#define BREZEL_COMPILER_CLANG 1
#if __clang_major__ < 12
#error "Minimum required Clang version is 12.0.0 (supports C++20)"
#endif
#elif defined(__GNUC__)
#define BREZEL_COMPILER_GCC 1
#if __GNUC__ < 10 || (__GNUC__ == 10 && __GNUC_MINOR__ < 1)
#error "Minimum required GCC version is 10.1 (supports C++20)"
#endif
#else
#define BREZEL_COMPILER_UNKNOWN 1
#warning "Unknown compiler, proceed with caution"
#endif

// C++20 feature checks
#if __cplusplus < 202002L && !defined(_MSVC_LANG)
#error "This library requires C++20 or later"
#endif

// Function attributes
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_INLINE __forceinline
#define BREZEL_NOINLINE __declspec(noinline)
#define BREZEL_DEPRECATED(msg) [[deprecated(msg)]]
#elif defined(BREZEL_COMPILER_GCC) || defined(BREZEL_COMPILER_CLANG)
#define BREZEL_INLINE inline __attribute__((always_inline))
#define BREZEL_NOINLINE __attribute__((noinline))
#define BREZEL_DEPRECATED(msg) [[deprecated(msg)]]
#else
#define BREZEL_INLINE inline
#define BREZEL_NOINLINE
#define BREZEL_DEPRECATED(msg) [[deprecated(msg)]]
#endif

// Memory alignment (for SIMD operations)
#define BREZEL_DEFAULT_ALIGN 64

// Thread-local storage
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_THREAD_LOCAL __declspec(thread)
#else
#define BREZEL_THREAD_LOCAL thread_local
#endif

// Debug assertions
#if BREZEL_DEBUG_MODE
// Simple message version
#define BREZEL_ASSERT(condition, message)                                                       \
    do {                                                                                        \
        if (!(condition)) {                                                                     \
            std::cerr << fmt::format("Assertion failed: {}\nMessage: {}\nFile: {}\nLine: {}\n", \
                                     #condition, message, __FILE__, __LINE__);                  \
            std::abort();                                                                       \
        }                                                                                       \
    } while (0)

// Formatted message version
#define BREZEL_ASSERT_FMT(condition, fmt_string, ...)                                              \
    do {                                                                                           \
        if (!(condition)) {                                                                        \
            std::cerr << fmt::format("Assertion failed: {}\nMessage: {}\nFile: {}\nLine: {}\n",    \
                                     #condition, fmt::format(fmt_string, ##__VA_ARGS__), __FILE__, \
                                     __LINE__);                                                    \
            std::abort();                                                                          \
        }                                                                                          \
    } while (0)
#else
#define BREZEL_ASSERT(condition, message) \
    do {                                  \
    } while (0)
#define BREZEL_ASSERT_FMT(condition, fmt_string, ...) \
    do {                                              \
    } while (0)
#endif

// Exception macros
#include <fmt/core.h>
#include <fmt/format.h>

// Simple string message version
#define BREZEL_THROW(exception_type, message) throw exception_type(message)

// Templated version for fmt formatting
#define BREZEL_THROW_FMT(exception_type, fmt_string, ...) \
    throw exception_type(fmt_string, ##__VA_ARGS__)

// Common exception macros
#define BREZEL_THROW_RUNTIME_ERROR(message) BREZEL_THROW(brezel::utils::RuntimeError, message)
#define BREZEL_THROW_VALUE_ERROR(message) BREZEL_THROW(brezel::utils::ValueError, message)
#define BREZEL_THROW_INDEX_ERROR(message) BREZEL_THROW(brezel::utils::IndexError, message)
#define BREZEL_THROW_SHAPE_ERROR(message) BREZEL_THROW(brezel::utils::ShapeError, message)
#define BREZEL_THROW_DEVICE_ERROR(message) BREZEL_THROW(brezel::utils::DeviceError, message)
#define BREZEL_THROW_MEMORY_ERROR(message) BREZEL_THROW(brezel::utils::MemoryError, message)
#define BREZEL_THROW_NOT_IMPLEMENTED(message) \
    BREZEL_THROW(brezel::utils::NotImplementedError, message)

// Fmt versions with variadic arguments
#define BREZEL_THROW_RUNTIME_ERROR_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::RuntimeError, fmt_string, ##__VA_ARGS__)
#define BREZEL_THROW_VALUE_ERROR_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::ValueError, fmt_string, ##__VA_ARGS__)
#define BREZEL_THROW_INDEX_ERROR_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::IndexError, fmt_string, ##__VA_ARGS__)
#define BREZEL_THROW_SHAPE_ERROR_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::ShapeError, fmt_string, ##__VA_ARGS__)
#define BREZEL_THROW_DEVICE_ERROR_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::DeviceError, fmt_string, ##__VA_ARGS__)
#define BREZEL_THROW_MEMORY_ERROR_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::MemoryError, fmt_string, ##__VA_ARGS__)
#define BREZEL_THROW_NOT_IMPLEMENTED_FMT(fmt_string, ...) \
    BREZEL_THROW_FMT(brezel::utils::NotImplementedError, fmt_string, ##__VA_ARGS__)

// Logging macros
enum class LogLevel { TRACE, DEBUG, INFO, WARNING, ERROR, FATAL };

#ifdef BREZEL_WITH_SPDLOG
// If spdlog is available, use it for logging
#include <spdlog/spdlog.h>
#define BREZEL_LOG_TRACE(message) SPDLOG_TRACE(message)
#define BREZEL_LOG_DEBUG(message) SPDLOG_DEBUG(message)
#define BREZEL_LOG_INFO(message) SPDLOG_INFO(message)
#define BREZEL_LOG_WARN(message) SPDLOG_WARN(message)
#define BREZEL_LOG_ERROR(message) SPDLOG_ERROR(message)
#define BREZEL_LOG_CRITICAL(message) SPDLOG_CRITICAL(message)
#else
// Otherwise, use basic logging to cerr
#if BREZEL_DEBUG_MODE
#define BREZEL_LOG(level, message)                                                            \
    do {                                                                                      \
        std::ostringstream ss;                                                                \
        ss << "[" << #level << "] " << message << " (" << __FILE__ << ":" << __LINE__ << ")"; \
        std::cerr << ss.str() << std::endl;                                                   \
    } while (0)
#else
#define BREZEL_LOG(level, message) \
    do {                           \
    } while (0)
#endif

#define BREZEL_LOG_TRACE(message) BREZEL_LOG(TRACE, message)
#define BREZEL_LOG_DEBUG(message) BREZEL_LOG(DEBUG, message)
#define BREZEL_LOG_INFO(message) BREZEL_LOG(INFO, message)
#define BREZEL_LOG_WARN(message) BREZEL_LOG(WARNING, message)
#define BREZEL_LOG_ERROR(message) BREZEL_LOG(ERROR, message)
#define BREZEL_LOG_CRITICAL(message) BREZEL_LOG(FATAL, message)
#endif

// Performance measurement
#define BREZEL_MEASURE_TIME(operation, result_var)                                        \
    do {                                                                                  \
        auto start_time = std::chrono::high_resolution_clock::now();                      \
        operation;                                                                        \
        auto end_time = std::chrono::high_resolution_clock::now();                        \
        auto duration =                                                                   \
            std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time); \
        result_var = duration.count() / 1000.0;                                           \
    } while (0)

// Memory utilities
#if defined(BREZEL_COMPILER_MSVC)
#define BREZEL_ALIGNED_ALLOC(alignment, size) _aligned_malloc(size, alignment)
#define BREZEL_ALIGNED_FREE(ptr) _aligned_free(ptr)
#else
#define BREZEL_ALIGNED_ALLOC(alignment, size) std::aligned_alloc(alignment, size)
#define BREZEL_ALIGNED_FREE(ptr) free(ptr)
#endif

// Feature detection based on compile-time definitions
#if defined(BREZEL_WITH_OPENMP)
#define BREZEL_HAS_OPENMP 1
#else
#define BREZEL_HAS_OPENMP 0
#endif

#if defined(BREZEL_WITH_CUDA)
#define BREZEL_HAS_CUDA 1
#else
#define BREZEL_HAS_CUDA 0
#endif

#if defined(BREZEL_WITH_BLAS)
#define BREZEL_HAS_BLAS 1
#else
#define BREZEL_HAS_BLAS 0
#endif

#if defined(BREZEL_WITH_EIGEN)
#define BREZEL_HAS_EIGEN 1
#else
#define BREZEL_HAS_EIGEN 0
#endif

// Profiling macros
#if defined(BREZEL_ENABLE_PROFILING)
#define BREZEL_PROFILE_FUNCTION()  /* Implementation required */
#define BREZEL_PROFILE_SCOPE(name) /* Implementation required */
#else
#define BREZEL_PROFILE_FUNCTION()
#define BREZEL_PROFILE_SCOPE(name)
#endif

// Helper for unused variables/parameters
#define BREZEL_UNUSED(x) (void)(x)