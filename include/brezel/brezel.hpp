/**
 * @file brezel.hpp
 * @author Carlos Salguero
 * @brief Main include file for the brezel library
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This is the main include file for the brezel libray,
 * a modern C++ tensor framework designed to mimic PyTorch's API.
 * Include this file to access all functionality of the library.
 */

#pragma once

// Core includes
#include <brezel/core/device.hpp>
#include <brezel/core/dtype.hpp>
#include <brezel/core/exception.hpp>
#include <brezel/core/index.hpp>
#include <brezel/core/scalar.hpp>
#include <brezel/core/shape.hpp>
#include <brezel/core/storage.hpp>

// Version information
#define BREZEL_VERSION_MAJOR 0
#define BREZEL_VERSION_MINOR 1
#define BREZEL_VERSION_PATCH 0
#define BREZEL_VERSION_STRING "0.1.0"

namespace brezel {
/**
 * @brief Get the library version string
 *
 * @return const char* Version string in format "MAJOR.MINOR.PATCH"
 */
inline const char* version() {
    return BREZEL_VERSION_STRING;
}

/**
 * @brief Initialize the brezel library
 *
 * This function initializes internal components of the library.
 * It should be called before using any other functions.
 *
 * @param seed Seed for random number generators (default: random)
 */
void initialize(uint64_t seed = 0);

/**
 * @brief Clean up the brezel library
 *
 * This function cleans up internal components of the library.
 * It should be called when the library is no longer needed.
 */
void finalize();

/**
 * @brief Check if CUDA is available
 *
 * @return bool Whether CUDA is available
 */
bool cuda_is_available();

/**
 * @brief Get the number of available CUDA devices
 *
 * @return int Number of available CUDA devices
 */
int cuda_device_count();

/**
 * @brief Set the current CUDA device
 *
 * @param device_index CUDA device index
 */
void cuda_set_device(int device_index);

/**
 * @brief Get the current CUDA device
 *
 * @return int Current CUDA device index
 */
int cuda_current_device();

/**
 * @brief Synchronize the current CUDA device
 *
 * This function waits for all kernels on the current CUDA device to complete.
 */
void cuda_synchronize();

/**
 * @brief Set the number of threads used for CPU operations
 *
 * @param num_threads Number of threads
 */
void set_num_threads(int num_threads);

/**
 * @brief Get the number of threads used for CPU operations
 *
 * @return int Number of threads
 */
int get_num_threads();

/**
 * @brief Set the default tensor type
 *
 * @param dtype Default data type
 */
void set_default_dtype(DType dtype);

/**
 * @brief Get the default tensor type
 *
 * @return DType Default data type
 */
DType get_default_dtype();

/**
 * @brief Set whether to use deterministic algorithms
 *
 * @param deterministic Whether to use deterministic algorithms
 */
void set_deterministic(bool deterministic);

/**
 * @brief Check if deterministic algorithms are used
 *
 * @return bool Whether deterministic algorithms are used
 */
bool is_deterministic();

/**
 * @brief Set the random seed
 *
 * @param seed Random seed
 */
void manual_seed(uint64_t seed);

/**
 * @brief Get a random seed
 *
 * @return uint64_t Random seed
 */
uint64_t initial_seed();

}  // namespace brezel