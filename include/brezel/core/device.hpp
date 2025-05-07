/**
 * @file device.hpp
 * @author Carlos Salguero
 * @brief Defines device types and utilities for the brezel framework
 * @version 0.1
 * @date 2025-05-01
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains device type definition and utilities for
 * different computational accelerators
 */

#pragma once

#include <array>
#include <concepts>
#include <memory>
#include <ostream>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>

#include <boost/container/small_vector.hpp>
#include <boost/core/demangle.hpp>
#include <boost/predef.h>
#include <boost/serialization/strong_typedef.hpp>
#include <boost/system/error_code.hpp>
#include <fmt/format.h>
#include <fmt/ostream.h>

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
    ROCm,    ///< AMD ROCm device
    Custom   ///< Custom device type
};

/**
 * @brief Convert a device type to a string
 *
 * @param type The device type
 * @return std::string_view String representation
 */
[[nodiscard]] constexpr std::string_view device_type_to_string(DeviceType type) noexcept {
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

        case DeviceType::ROCm:
            return "ROCm";

        case DeviceType::Custom:
            return "Custom";

        default:
            return "Unknown";
    }
}

/**
 * @brief Hash function for DeviceType
 */
struct DeviceTypeHash {
    std::size_t operator()(DeviceType type) const noexcept {
        return static_cast<std::size_t>(type);
    }
}

// Forward declarations
class DeviceProperties;
class DeviceManager;

/**
 * @brief Class representing a computational device
 *
 * This class uses the PIMPL idiom for handling platform-specific device
 * details.
 */
class Device {
public:
    /**
     * @brief Default constructor (creates CPU device)
     */
    constexpr Device() noexcept = default;

    /**
     * @brief Construct a device with the specified type and index
     *
     * @param type The device type
     * @param index The device index (for devices with multiple instances)
     */
    constexpr Device(DeviceType type, int index = 0) : m_type(type), m_index(index) { validate(); }

    /**
     * @brief Construct a device from a string specification
     *
     * @param device_str Device specification (e.g., "cpu", "cuda:0")
     * @throws std::invalid_argument If the device string is invalid
     */
    explicit Device(std::string_view device_str) {
        namespace bp = boost::predef;

        if (device_str == "cpu") {
            m_type = DeviceType::CPU;
            m_index = 0;
        } else if (device_str.starts_with("cuda:")) {
            m_type = DeviceType::CUDA;

            try {
                m_index = std::stoi(std::string(device_str.substr(5)));
            } catch (const std::exception&) {
                throw std::invalid_argument(
                    fmt::format("Invalid CUDA device index: {}", device_str));
            }
        } else if (device_str.starts_with("opencl:")) {
            m_type = DeviceType::OpenCL;

            try {
                m_index = std::stoi(std::string(device_str.substr(7)));
            } catch (const std::exception&) {
                throw std::invalid_argument(
                    fmt::format("Invalid OpenCL device index: {}", device_str));
            }
        } else if (device_str.starts_with("vulkan:")) {
            m_type = DeviceType::Vulkan;

            try {
                m_index = std::stoi(std::string(device_str.substr(7)));
            } catch (const std::exception&) {
                throw std::invalid_argument(
                    fmt::format("Invalid Vulkan device index: {}", device_str));
            }
        } else if (device_str.starts_with("metal:")) {
            m_type = DeviceType::Metal;

            if (!bp::BOOST_OS_MACOS) {
                throw std::invalid_argument("Metal devices are only available on macOS");
            }

            try {
                m_index = std::stoi(std::string(device_str.substr(6)));
            } catch (const std::exception&) {
                throw std::invalid_argument(
                    fmt::format("Invalid Metal device index: {}", device_str));
            }
        } else if (device_str.starts_with("tpu:")) {
            m_type = DeviceType::TPU;

            try {
                m_index = std::stoi(std::string(device_str.substr(4)));
            } catch (const std::exception&) {
                throw std::invalid_argument(
                    fmt::format("Invalid TPU device index: {}", device_str));
            }
        } else if (device_str.starts_with("rocm:")) {
            m_type = DeviceType::ROCm;

            try {
                m_index = std::stoi(std::string(device_str.substr(5)));
            } catch (const std::exception&) {
                throw std::invalid_argument(
                    fmt::format("Invalid ROCm device index: {}", device_str));
            }
        } else {
            throw std::invalid_argument(fmt::format("Unknown device: {}", device_str));
        }

        validate();
    }

    /**
     * @brief Get the device type
     *
     * @return DeviceType The device type
     */
    [[nodiscard]] constexpr DeviceType type() const noexcept { return m_type; }

    /**
     * @brief Get the device index
     *
     * @return int The device index
     */
    [[nodiscard]] constexpr int index() const noexcept { return m_index; }

    /**
     * @brief Check if the device is a CPU
     *
     * @return bool Whether the device is a CPU
     */
    [[nodiscard]] constexpr bool is_cpu() const noexcept { return m_type == DeviceType::CPU; }

    /**
     * @brief Check if the device is a CUDA device
     *
     * @return bool Whether the device is a CUDA device
     */
    [[nodiscard]] constexpr bool is_cuda() const noexcept { return m_type == DeviceType::CUDA; }

    /**
     * @brief Check if the device is an OpenCL device
     *
     * @return bool Whether the device is an OpenCL device
     */
    [[nodiscard]] constexpr bool is_opencl() const noexcept { return m_type == DeviceType::OpenCL; }

    /**
     * @brief Check if the device is a Vulkan device
     *
     * @return bool Whether the device is a Vulkan device
     */
    [[nodiscard]] constexpr bool is_vulkan() const noexcept { return m_type == DeviceType::Vulkan; }

    /**
     * @brief Check if the device is a Metal device
     *
     * @return bool Whether the device is a Metal device
     */
    [[nodiscard]] constexpr bool is_metal() const noexcept { return m_type == DeviceType::Metal; }

    /**
     * @brief Check if the device is a TPU device
     *
     * @return bool Whether the device is a TPU device
     */
    [[nodiscard]] constexpr bool is_tpu() const noexcept { return m_type == DeviceType::TPU; }

    /**
     * @brief Check if the device is a ROCm device
     *
     * @return bool Whether the device is a ROCm device
     */
    [[nodiscard]] constexpr bool is_rocm() const noexcept { return m_type == DeviceType::ROCm; }

    /**
     * @brief Check if the device is GPU-based
     *
     * @return bool Whether the device is GPU-based (CUDA, Vulkan, Metal, or ROCm)
     */
    [[nodiscard]] constexpr bool is_gpu() const noexcept {
        return is_cuda() || is_vulkan() || is_metal() || is_rocm();
    }

    /**
     * @brief Check if the device is accelerator-based
     *
     * @return bool Whether the device is an accelerator (GPU or TPU)
     */
    [[nodiscard]] constexpr bool is_accelerator() const noexcept { return is_gpu() || is_tpu(); }

    /**
     * @brief Check if the device is available
     *
     * @return bool Whether the device is available
     */
    [[nodiscard]] bool is_available() const;

    /**
     * @brief Get device properties
     *
     * @return DeviceProperties The device properties
     */
    [[nodiscard]] DeviceProperties get_properties() const;

    /**
     * @brief Get the device memory info
     *
     * @return std::pair<size_t, size_t> Total and free memory in bytes
     */
    [[nodiscard]] std::pair<size_t, size_t> get_memory_info() const;

    /**
     * @brief Convert the device to a string
     *
     * @return std::string String representation
     */
    [[nodiscard]] std::string to_string() const {
        if (m_type == DeviceType::CPU) {
            return std::string(device_type_to_string(m_type));
        }

        return fmt::format("{}:{}", device_type_to_string(m_type), m_index);
    }

    /**
     * @brief Equality operator
     *
     * @param other The other device
     * @return bool Whether the devices are equal
     */
    [[nodiscard]] constexpr bool operator==(const Device& other) const noexcept {
        return m_type == other.m_type && m_index == other.m_index;
    }

    /**
     * @brief Inequality operator
     *
     * @param other The other device
     * @return bool Whether the devices are not equal
     */
    [[nodiscard]] constexpr bool operator!=(const Device& other) const noexcept {
        return !(*this == other);
    }

    /**
     * @brief Hash function for Device
     */
    [[nodiscard]] friend std::size_t hash_value(const Device& device) noexcept {
        std::size_t seed = 0;
        boost::hash_combine(seed, static_cast<std::size_t>(device.m_type));
        boost::hash_combine(seed, device.m_index);

        return seed;
    }

    /**
     * @brief Less-than operator for ordering in containers
     *
     * @param other The other device
     * @return bool Whether this device is ordered before the other
     */
    [[nodiscard]] constexpr bool operator<(const Device& other) const noexcept {
        if (m_type != other.m_type) {
            return static_cast<int>(m_type) < static_cast<int>(other.m_type);
        }

        return m_index < other.m_index;
    }

    // Convenient static constants
    static const Device CPU;     ///< CPU device
    static const Device CUDA;    ///< Default CUDA device (index 0)
    static const Device CUDA1;   ///< CUDA device with index 1
    static const Device CUDA2;   ///< CUDA device with index 2
    static const Device OpenCL;  ///< Default OpenCL device (index 0)
    static const Device Vulkan;  ///< Default Vulkan device (index 0)
    static const Device Metal;   ///< Default Metal device (index 0)
    static const Device ROCm;    ///< Default ROCm device (index 0)

private:
    DeviceType m_type{DeviceType::CPU};
    int m_index{0};

    friend class DeviceManager;

    // Functions
    /**
     * @brief Validates the device configuration
     *
     * @throws std::invalid_argument If the device configuration fails
     */
    constexpr void validate() const {
        if (m_index < 0) {
            throw std::invalid_argument("Device index must be positive");
        }

        if (m_type == DeviceType::CPU && m_index > 0) {
            throw std::invalid_argument("CPU device index must be 0");
        }
    }
};

// Initialize static constants
inline constexpr Device Device::CPU(DeviceType::CPU, 0);
inline constexpr Device Device::CUDA(DeviceType::CUDA, 0);
inline constexpr Device Device::CUDA1(DeviceType::CUDA, 1);
inline constexpr Device Device::CUDA2(DeviceType::CUDA, 2);
inline constexpr Device Device::OpenCL(DeviceType::OpenCL, 0);
inline constexpr Device Device::Vulkan(DeviceType::Vulkan, 0);
inline constexpr Device Device::Metal(DeviceType::Metal, 0);
inline constexpr Device Device::ROCm(DeviceType::ROCm, 0);

/**
 * @brief Device properties class
 */
class DeviceProperties {
public:
    std::string name;                  ///< Device name
    std::string vendor;                ///< Device vendor
    std::string driver_version;        ///< Driver version
    size_t total_memory = 0;           ///< Total memory in bytes
    int compute_capability_major = 0;  ///< Compute capability major version (CUDA)
    int compute_capability_minor = 0;  ///< Compute capability minor version (CUDA)
    int max_threads_per_block = 0;     ///< Maximum threads per block
    int max_shared_memory = 0;         ///< Maximum shared memory per block in bytes
    int warp_size = 0;                 ///< Warp size

    boost::container::small_vector<int, 3> max_grid_size;
    boost::container::small_vector<int, 3> max_block_size;

    /**
     * @brief Convert to string representation
     *
     * @return std::string String representation of properties
     */
    [[nodiscard]] std::string to_string() const {
        std::ostringstream ss;
        ss << "Device Properties:\n"
           << "  Name: " << name << "\n"
           << "  Vendor: " << vendor << "\n"
           << "  Driver Version: " << driver_version << "\n"
           << "  Total Memory: " << (total_memory / (1024 * 1024)) << " MB\n";

        if (compute_capability_major > 0) {
            ss << "  Compute Capability: " << compute_capability_major << "."
               << compute_capability_minor << "\n";
        }

        if (max_threads_per_block > 0) {
            ss << "  Max Threads per Block: " << max_threads_per_block << "\n";
        }

        if (max_shared_memory > 0) {
            ss << "  Max Shared Memory per Block: " << (max_shared_memory / 1024) << " KB\n";
        }

        if (warp_size > 0) {
            ss << "  Warp Size: " << warp_size << "\n";
        }

        return ss.str();
    }
}

/**
 * @brief Forward declaration of DeviceManager implementation
 */
namespace detail {
    class DeviceManagerImpl;
}

/**
 * @brief Device Manager class for handling device operations
 *
 * This class follows the Singleton pattern and uses PIMPL for
 * implementation details.
 */
class DeviceManager {
public:
    /**
     * @brief Get the singleton instance
     *
     * @return DeviceManager& Singleton instance
     */
    static DeviceManager& instance();

    /**
     * @brief Initialize the device manager
     *
     * @return boost::system::error_code Error code (0 if success)
     */
    boost::system::error_code initialize();

    /**
     * @brief Check if a device is available
     *
     * @param device Device to check
     * @return bool Whether the device is available
     */
    bool is_device_available(const Device& device) const;

    /**
     * @brief Get a list of all available devices
     *
     * @return std::vector<Device> List of available devices
     */
    std::vector<Device> get_available_devices() const;

    /**
     * @brief Get device properties
     *
     * @param device Device to query
     * @return DeviceProperties Device properties
     */
    DeviceProperties get_device_properties(const Device& device) const;

    /**
     * @brief Get device memory info
     *
     * @param device Device to query
     * @return std::pair<size_t, size_t> Total and free memory in bytes
     */
    std::pair<size_t, size_t> get_device_memory_info(const Device& device) const;

    /**
     * @brief Get the count of devices for a specific type
     *
     * @param type Device type
     * @return int Number of available devices
     */
    int get_device_count(DeviceType type) const;

private:
    DeviceManager();
    ~DeviceManager();

    // Prevent copying and assignment
    DeviceManager(const DeviceManager&) = delete;
    DeviceManager& operator=(const DeviceManager&) = delete;
    DeviceManager(DeviceManager&&) = delete;
    DeviceManager& operator=(DeviceManager&&) = delete;

    std::unique_ptr<detail::DeviceManagerImpl> m_impl;
};

// Functions
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