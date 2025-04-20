/**
 * @file categories.hpp
 * @author Carlos Salguero
 * @brief Error category definitions for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines the error categories used throughout the brezel framework
 */

#pragma once

#include <cstdint>
#include <string_view>

#include <brezel/core/config.hpp>

namespace brezel::error {
/**
 * @brief Error categories for the brezel framework
 */
enum class Category : uint8_t {
    None,      ///< No error
    General,   ///< General errors
    Device,    ///< Device-related errors
    Memory,    ///< Memory management errors
    Tensor,    ///< Tensor operations errors
    IO,        ///< Input/output errors
    Math,      ///< Mathematical errors
    Runtime,   ///< Runtime errors
    Internal,  ///< Internal implementation errors
    System,    ///< System/OS errors
    Library,   ///< External library errors
    Cuda,      ///< CUDA-specific errors
    OpenCL,    ///< OpenCL-specific errors
    Format     ///< Format/parsing errors
};

/**
 * @brief Convert error category to string
 *
 * @param category Error category
 * @return std::string_view String representation
 */
constexpr std::string_view category_to_string(Category category) noexcept {
    switch (category) {
        case Category::None:
            return "None";

        case Category::General:
            return "General";

        case Category::Device:
            return "Device";

        case Category::Memory:
            return "Memory";

        case Category::Tensor:
            return "Tensor";

        case Category::IO:
            return "IO";

        case Category::Math:
            return "Math";

        case Category::Runtime:
            return "Runtime";

        case Category::Internal:
            return "Internal";

        case Category::System:
            return "System";

        case Category::Library:
            return "Library";

        case Category::Cuda:
            return "CUDA";

        case Category::OpenCL:
            return "OpenCL";

        case Category::Format:
            return "Format";
            
        default:
            return "Unknown";
    }
}
}  // namespace brezel::error