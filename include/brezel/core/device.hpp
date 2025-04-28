/**
 * @file device.hpp
 * @author Carlos Salguero
 * @brief Defines device types and utilities for the brezel library
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains device type definitinos and utilities for handling
 * different computational devices such as CPU and GPU.
 */

#pragma once

#include <ostream>
#include <stdexcept>
#include <string>

namespace brezel {
/**
 * @brief Enumeration of device types
 */
enum class DeviceType {
    CPU,     ///< CPU device
    CUDA,    ///< NVIDIA CUDA device
    OpenCL,  ///< OpenCL device
    Vulkan,  ///< Vulkan device
    Metal,   ///< Apple Metal device
    TPU,     ///< Tensor Processing Unit
    Custom   ///< Custom device type
};

/**
 * @brief Convert a device type to a string
 *
 * @param type The device type
 * @return std::string String representation
 */
inline std::string device_type_to_string(DeviceType type) {
    switch (type) {
        case DeviceType::CPU:
            return "CPU";

        case DeviceType::CUDA:
            return "CUDA";

        case DeviceType::OpenCL:
            return "OpenCL";

        case DeviceType::Vulkan:
            return "Vulkan";

        case DeviceType::Metal:
            return "Metal";

        case DeviceType::TPU:
            return "TPU";

        case DeviceType::Custom:
            return "Custom";

        default:
            return "Unknown";
    }
}

/**
 * @brief Class representing a computational device
 */
class Device {
public:
    /**
     * @brief Default constructor (creates CPU device)
     */
    Device() : m_type(DeviceType::CPU), m_index(0) {}

    /**
     * @brief Construct a device with the specified type and index
     *
     * @param type The device type
     * @param index The device index (for devices with multiple instances)
     */
    Device(DeviceType type, int index = 0) : m_type(type), m_index(index) { validate(); }

    /**
     * @brief Construct a device from a string specification
     *
     * @param device_str Device specification (e.g., "cpu", "cuda:0")
     */
    explicit Device(const std::string& device_str) {
        if (device_str == "cpu") {
            m_type = DeviceType::CPU;
            m_index = 0;
        } else if (device_str.find("cuda:") == 0) {
            m_type = DeviceType::CUDA;

            try {
                m_index = std::stoi(device_str.substr(5));
            } catch (const std::exception&) {
                throw std::invalid_argument("Invalid CUDA device index: " + device_str);
            }
        } else if (device_str.find("opencl:") == 0) {
            m_type = DeviceType::OpenCL;

            try {
                m_index = std::stoi(device_str.substr(7));
            } catch (const std::exception&) {
                throw std::invalid_argument("Invalid OpenCL device index: " + device_str);
            }
        } else if (device_str.find("vulkan:") == 0) {
            m_type = DeviceType::Vulkan;

            try {
                m_index = std::stoi(device_str.substr(7));
            } catch (const std::exception&) {
                throw std::invalid_argument("Invalid Vulkan device index: " + device_str);
            }
        } else if (device_str.find("metal:") == 0) {
            m_type = DeviceType::Metal;

            try {
                m_index = std::stoi(device_str.substr(6));
            } catch (const std::exception&) {
                throw std::invalid_argument("Invalid Metal device index: " + device_str);
            }
        } else if (device_str.find("tpu:") == 0) {
            m_type = DeviceType::TPU;

            try {
                m_index = std::stoi(device_str.substr(4));
            } catch (const std::exception&) {
                throw std::invalid_argument("Invalid TPU device index: " + device_str);
            }
        } else {
            throw std::invalid_argument("Unknown device: " + device_str);
        }

        validate();
    }

    /**
     * @brief Get the device type
     *
     * @return DeviceType The device type
     */
    DeviceType type() const { return m_type; }

    /**
     * @brief Get the device index
     *
     * @return int The device index
     */
    int index() const { return m_index; }

    /**
     * @brief Check if the device is a CPU
     *
     * @return bool Whether the device is a CPU
     */
    bool is_cpu() const { return m_type == DeviceType::CPU; }

    /**
     * @brief Check if the device is a CUDA device
     *
     * @return bool Whether the device is a CUDA device
     */
    bool is_cuda() const { return m_type == DeviceType::CUDA; }

    /**
     * @brief Check if the device is available
     *
     * @return bool Whether the device is available
     */
    bool is_available() const;

    /**
     * @brief Convert the device to a string
     *
     * @return std::string String representation
     */
    std::string to_string() const {
        std::string result = device_type_to_string(m_type);

        if (m_type != DeviceType::CPU) {
            result += ":" + std::to_string(m_index);
        }

        return result;
    }

    /**
     * @brief Equality operator
     *
     * @param other The other device
     * @return bool Whether the devices are equal
     */
    bool operator==(const Device& other) const {
        return m_type == other.m_type && m_index == other.m_index;
    }

    /**
     * @brief Inequality operator
     *
     * @param other The other device
     * @return bool Whether the devices are not equal
     */
    bool operator!=(const Device& other) const { return !(*this == other); }

    // Convenient static constants
    static const Device CPU;    ///< CPU device
    static const Device CUDA;   ///< Default CUDA device (index 0)
    static const Device CUDA1;  ///< CUDA device with index 1
    static const Device CUDA2;  ///< CUDA device with index 2

private:
    DeviceType m_type;
    int m_index;

    /**
     * @brief Validates the device configuration
     *
     * @throws std::invalid_argument If the device configuration is invalid
     */
    void validate() {
        if (m_index < 0) {
            throw std::invalid_argument("Device index must be non-negative");
        }

        if (m_type == DeviceType::CPU && m_index > 0) {
            throw std::invalid_argument("CPU device index must be 0");
        }

#ifdef BREZEL_USE_CUDA
        // TODO: Add CUDA-specific validation can be added here
#else
        if (m_type == DeviceType::CUDA) {
            throw std::invalid_argument("CUDA support is not enabled");
        }
#endif
    }
};

// Initialize static constants
inline const Device Device::CPU(DeviceType::CPU, 0);
inline const Device Device::CUDA(DeviceType::CUDA, 0);
inline const Device Device::CUDA1(DeviceType::CUDA, 1);
inline const Device Device::CUDA2(DeviceType::CUDA, 2);

/**
 * @brief Implementation of is_available method
 *
 * @return bool Whether the device is available
 */
inline bool Device::is_available() const {
    switch (m_type) {
        case DeviceType::CPU:
            return true;

        case DeviceType::CUDA:
#ifdef BREZEL_USE_CUDA
            // TODO: Check if CUDA is available and the requested device is valid
#else
            return false;
#endif

        case DeviceType::OpenCL:
        case DeviceType::Vulkan:
        case DeviceType::Metal:
        case DeviceType::TPU:
            // TODO: add validation here
            std::invalid_argument("Validation not supported yet");

        default:
            return false;
    }
}

/**
 * @brief Output stream for operator Device
 *
 * @param os Output stream
 * @param device Device to output
 * @return std::ostream& Reference to the output stream
 */
inline std::ostream& operator<<(std::ostream& os, const Device& device) {
    return os << device.to_string();
}

/**
 * @brief Get the current device
 *
 * @return Device The current device
 */
Device get_current_device();

/**
 * @brief Set the current device
 *
 * @param device The device to set as current
 */
void set_current_device(const Device& device);

/**
 * @brief Count the number of available devices of the specified type
 *
 * @param type The device type
 * @return int The number of available devices
 */
int device_count(DeviceType type);

/**
 * @brief Synchronize the specified device
 *
 * @param device The device to synchronize
 */
void synchronize(const Device& device);
}  // namespace brezel