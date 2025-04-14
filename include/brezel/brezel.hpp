#pragma once

/**
 * @brief brezel.hpp
 * @brief Main include file for the brezel tensor framework
 *
 * This header includes all public components of the brezel library
 */

// Core components
#include <brezel/core/device.hpp>
#include <brezel/core/types.hpp>

// Operations
// Neural network components
// Utilities
#include <brezel/utils/exception.hpp>
#include <brezel/utils/macros.hpp>
#include <brezel/utils/utils.hpp>

// Convenience namespace
namespace bz = brezel;

/**
 * @namespace brezel
 * @brief Main namespace for the brezel library
 */

/**
 * @namespace brezel::ops
 * @brief Namespace for tensor operations
 */

/**
 * @namespace brezel::nn
 * @brief Namespace for neural network components
 */

/**
 * @namespace brezel::utils
 * @brief Namespace for utility functions and classes
 */

/**
 * @namespace brezel::devices
 * @brief Namespace for device implementations
 */