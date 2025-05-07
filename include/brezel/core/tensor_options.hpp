/**
 * @file tensor_options.hpp
 * @author Carlos Salguero
 * @brief Defines configuration options for tensors
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains the TensorOptions class, which encapsulates various
 * configuration options for tensors such as data type, device, and layout.
 */

#pragma once

#include <brezel/core/device.hpp>
#include <brezel/core/dtype.hpp>

namespace brezel {

/**
 * @brief Tensor layout type
 */
enum class Layout {
    Strided,  ///< Dense tensor with strided access
    Sparse,   ///< Sparse tensor
    COO,      ///< Coordinate format sparse tensor
    CSR,      ///< Compressed Sparse Row format
    CSC       ///< Compressed Sparse Column format
};

/**
 * @brief Memory format for tensor storage
 */
enum class MemoryFormat {
    Contiguous,      ///< Contiguous memory layout
    ChannelsLast,    ///< NHWC memory format for 4D tensors
    ChannelsLast3d,  ///< NDHWC memory format for 5D tensors
    Preserve,        ///< Preserve the current memory format
};

/**
 * @brief Configuration options for tensors
 *
 * This class encapsulates various configuration options for tensors,
 * such as data type, device, and layout.
 */
class TensorOptions {
public:
    /**
     * @brief Construct with default options
     */
    TensorOptions()
        : m_dtype(DType::Float32), m_device(Device::CPU), m_layout(Layout::Strided),
          m_memory_format(MemoryFormat::Contiguous), m_requires_grad(false),
          m_pinned_memory(false) {}

    /**
     * @brief Set the data type
     *
     * @param dtype The data type
     * @return TensorOptions& Reference to this
     */
    TensorOptions& dtype(DType dtype) {
        m_dtype = dtype;
        return *this;
    }

    /**
     * @brief Set the device
     *
     * @param device The device
     * @return TensorOptions& Reference to this
     */
    TensorOptions& device(const Device& device) {
        m_device = device;
        return *this;
    }

    /**
     * @brief Set the layout
     *
     * @param layout The layout
     * @return TensorOptions& Reference to this
     */
    TensorOptions& layout(Layout layout) {
        m_layout = layout;
        return *this;
    }

    /**
     * @brief Set the memory format
     *
     * @param memory_format The memory format
     * @return TensorOptions& Reference to this
     */
    TensorOptions& memory_format(MemoryFormat memory_format) {
        m_memory_format = memory_format;
        return *this;
    }

    /**
     * @brief Set whether the tensor requires gradients
     *
     * @param requires_grad Whether the tensor requires gradients
     * @return TensorOptions& Reference to this
     */
    TensorOptions& requires_grad(bool requires_grad) {
        m_requires_grad = requires_grad;
        return *this;
    }

    /**
     * @brief Set whether to use pinned memory
     *
     * @param pinned_memory Whether to use pinned memory
     * @return TensorOptions& Reference to this
     */
    TensorOptions& pinned_memory(bool pinned_memory) {
        m_pinned_memory = pinned_memory;
        return *this;
    }

    /**
     * @brief Get the data type
     *
     * @return DType The data type
     */
    DType dtype() const { return m_dtype; }

    /**
     * @brief Get the device
     *
     * @return const Device& The device
     */
    const Device& device() const { return m_device; }

    /**
     * @brief Get the layout
     *
     * @return Layout The layout
     */
    Layout layout() const { return m_layout; }

    /**
     * @brief Get the memory format
     *
     * @return MemoryFormat The memory format
     */
    MemoryFormat memory_format() const { return m_memory_format; }

    /**
     * @brief Check if the tensor requires gradients
     *
     * @return bool Whether the tensor requires gradients
     */
    bool requires_grad() const { return m_requires_grad; }

    /**
     * @brief Check if pinned memory is used
     *
     * @return bool Whether pinned memory is used
     */
    bool pinned_memory() const { return m_pinned_memory; }

    /**
     * @brief Equality operator
     *
     * @param other The other options to compare with
     * @return bool Whether the options are equal
     */
    bool operator==(const TensorOptions& other) const {
        return m_dtype == other.m_dtype && m_device == other.m_device &&
               m_layout == other.m_layout && m_memory_format == other.m_memory_format &&
               m_requires_grad == other.m_requires_grad && m_pinned_memory == other.m_pinned_memory;
    }

    /**
     * @brief Inequality operator
     *
     * @param other The other options to compare with
     * @return bool Whether the options are not equal
     */
    bool operator!=(const TensorOptions& other) const { return !(*this == other); }

private:
    DType m_dtype;                 ///< Data type of the tensor
    Device m_device;               ///< Device to store the tensor on
    Layout m_layout;               ///< Layout of the tensor
    MemoryFormat m_memory_format;  ///< Memory format of the tensor
    bool m_requires_grad;          ///< Whether the tensor requires gradients
    bool m_pinned_memory;          ///< Whether to use pinned memory
};

/**
 * @brief Convert layout to string
 *
 * @param layout Layout to convert
 * @return std::string String representation
 */
inline std::string layout_to_string(Layout layout) {
    switch (layout) {
        case Layout::Strided:
            return "Strided";
            
        case Layout::Sparse:
            return "Sparse";

        case Layout::COO:
            return "COO";

        case Layout::CSR:
            return "CSR";

        case Layout::CSC:
            return "CSC";

        default:
            return "Unknown";
    }
}

/**
 * @brief Convert memory format to string
 *
 * @param format Memory format to convert
 * @return std::string String representation
 */
inline std::string memory_format_to_string(MemoryFormat format) {
    switch (format) {
        case MemoryFormat::Contiguous:
            return "Contiguous";

        case MemoryFormat::ChannelsLast:
            return "ChannelsLast";

        case MemoryFormat::ChannelsLast3d:
            return "ChannelsLast3d";

        case MemoryFormat::Preserve:
            return "Preserve";

        default:
            return "Unknown";
    }
}

}  // namespace brezel