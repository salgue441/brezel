/**
 * @file device.cpp
 * @author Carlos Salguero
 * @brief Implementation of device-related functionality
 * @version 0.1
 * @date 2025-05-01
 *
 * @copyright Copyright (c) 2025
 *
 * This file implements device-related functionality for the brezel framework
 */

#include <atomic>
#include <iostream>
#include <memory>
#include <mutex>
#include <optional>
#include <shared_mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

#include <brezel/core/device.hpp>

#include <boost/algorithm/string.hpp>
#include <boost/dll/runtime_symbol_info.hpp>
#include <boost/filesystem.hpp>
#include <boost/process/environment.hpp>
#include <boost/system/error_code.hpp>
#include <fmt/format.h>

#ifdef BREZEL_USE_CUDA
    #include <cuda_runtime.h>
#endif

#ifdef BREZEL_USE_OPENCL
    #include <CL/cl.h>
#endif

#ifdef BREZEL_USE_VULKAN
    #include <vulkan/vulkan.h>
#endif

#ifdef BREZEL_USE_ROCM
    #include <hip/hip_runtime.h>
#endif

namespace brezel {
namespace detail {
/**
 * @brief Implementation of the DeviceManager (PIMPL pattern)
 */
class DeviceManagerImpl {
public:
    DeviceManagerImpl() : m_initialized(false) {}

    boost::system::error_code initialize() {
        if (m_initialized) {
            return boost::system::error_code();
        }

        boost::system::error_code error;

        m_available_devices.push_back(Device::CPU);

        DeviceProperties cpu_props;
        cpu_props.name = "CPU";
        cpu_props.vendor = get_cpu_vendor();
        cpu_props.total_memory = get_system_memory();
        m_device_properties[Device::CPU] = cpu_props;

        error = initialize_cuda_devices();
        if (error && error != boost::system::errc::not_supported) {
            std::cerr << "Error initializing CUDA devices: " << error.message() << std::endl;
        }

        error = initialize_opencl_devices();
        if (error && error != boost::system::errc::not_supported) {
            std::cerr << "Error initializing OpenCL devices: " << error.message() << std::endl;
        }

        error = initialize_vulkan_devices();
        if (error && error != boost::system::errc::not_supported) {
            std::cerr << "Error initializing Vulkan devices: " << error.message() << std::endl;
        }

        error = initialize_rocm_devices();
        if (error && error != boost::system::errc::not_supported) {
            std::cerr << "Error initializing ROCm devices: " << error.message() << std::endl;
        }

        m_initialized.store(true);
        return boost::system::error_code();
    }

    bool is_device_available(const Device& device) const {
        std::shared_lock lock(m_mutex);

        if (!m_initialized) {
            return device.is_cpu();
        }

        for (const auto& d : m_available_devices) {
            if (d == devices) {
                return true;
            }
        }

        return false;
    }

    std::vector<Device> get_available_devices() const {
        std::shared_lock lock(m_mutex);
        return m_available_devices;
    }

    DeviceProperties get_device_properties(const Device& device) const {
        std::shared_lock lock(m_mutex);
        auto it = m_device_properties.find(device);

        if (it != m_device_properties.end()) {
            return it->second;
        }

        return DeviceProperties();
    }

private:
    std::atomic<bool> m_initialized;
    mutable std::shared_mutex m_mutex;
    std::vector<Device> m_available_devices;
    std::unordered_map<Device, DeviceProperties> m_device_properties;

    static thread_local Device t_current_device;

    // Methods
    /**
     * @brief Simple implementation that relies on OS-specific methods
     * to find the cpu vendor.
     *
     * @details A more robust implementation would use CPUID on x86 platforms
     *
     * @return std::string Vendor name string
     */
    std::string get_cpu_vendor() const {
#if defined(_WIN32)
        auto env = boost::process::environment();
        auto processor_id = env["PROCESSOR_IDENTIFIER"].to_string();

        if (!processor_id.empty()) {
            return processor_id;
        }
#elif defined(__APPLE__) || defined(__linux__)
        try {
            boost::filesystem::ifstream cpuinfo("/proc/cpuinfo");
            std::string line;

            while (std::getline(cpuinfo, line)) {
                if (boost::starts_with(line, "vendor_id") ||
                    boost::starts_with(line, "model_name")) {
                    auto pos = line.find(":");
                    if (pos != std::string::npos) {
                        return boost::trim_copy(line.substr(pos + 1));
                    }
                }
            }
        } catch (...) {
            // Ignore errors and fallback to generic name
        }
#endif

        return fmt::format("Generic CPU ({} scores)", std::thread::hardware_concurrency());
    }

    size_t get_system_memory() const {
#if defined(_WIN32)
        MEMORYSTATUSEX status;
        status.dwLength = sizeof(status);

        if (GlobalMemoryStatusEx(&status)) {
            return status.ullTotalPhys;
        }
#elif defined(__APPLE__)
        int mib[2] = {CTL_HW, HW_MEMSIZE};
        int64_t size = 0;
        size_t len = sizeof(size);

        if (sysctl(mib, 2, &size, &len, NULL, 0) == 0) {
            return static_cast<size_t>(size);
        }
#elif defined(__linux__)
        try {
            boost::filesystem::ifstream meminfo("/proc/meminfo");
            std::string line;

            while (std::getline(meminfo, line)) {
                if (boost::starts_with(line, "MemTotal")) {
                    size_t kb = 0;

                    std::sscanf(line.c_str(), "MemTotal: %zu kB", &kb);
                    return kb * 1024;
                }
            }
        } catch (...) {
            // Ignore errors
        }
#endif

        return 8ULL * 1024 * 1024 * 1024;
    }
};

thread_local Device DeviceManagerImpl::t_current_device = Device::CPU;
}  // namespace detail

// Device manager implementation
DeviceManager::DeviceManager() : m_impl(std::make_unique<detail::DeviceManagerImpl>) {}

DeviceManager::~DeviceManager() = default;

DeviceManager& DeviceManager::instance() {
    static DeviceManager instance;
    return instance;
}

boost::system::error_code DeviceManager::initialize() {
    return m_impl->initialize();
}

bool DeviceManager::is_device_available(const Device& device) const {
    return m_impl->is_device_available(device);
}

std::vector<Device> DeviceManager::get_available_devices() const {
    return m_impl->get_available_devices();
}

DeviceProperties DeviceManager::get_device_properties(const Device& device) const {
    return m_impl->get_device_properties(device);
}

std::pair<size_t, size_t> DeviceManager::get_device_memory_info(const Device& device) const {
    return m_impl->get_device_memory_info(device);
}

int DeviceManager::get_device_count(DeviceType type) const {
    return m_impl->get_device_count(type);
}

// Global device functions
namespace {
thread_local Device g_current_device = Device::CPU;
}

Device get_current_device() {
    return g_current_device;
}

void set_current_device(const Device& device) {
    if (!device.is_available()) {
        throw std::runtime_error(fmt::format("Device {} is not available", device.to_string()));
    }

#ifdef BREZEL_USE_CUDA
    if (device.is_cuda()) {
        cudaError_t error = cudaSetDevice(device.index());
        if (error != cudaSuccess) {
            throw std::runtime_error(fmt::format("Failed to set CUDA device {}: {}", device.index(),
                                                 cudaGetErrorString(error)));
        }
    }
#endif

#ifdef BREZEL_USE_ROCM
    if (device.is_rocm()) {
        hipError_t error = hipSetDevice(device.index());
        if (error != hipSuccess) {
            const char* error_str = hipGetErrorString(error);
            throw std::runtime_error(
                fmt::format("Failed to set ROCm device {}: {}", device.index(), error_str));
        }
    }
#endif

    g_current_device = device;
}

void synchronize(const Device& device) {
    if (device.is_cpu()) {
        return;
    }

#ifdef BREZEL_USE_CUDA
    if (device.is_cuda()) {
        int current_device;
        cudaGetDevice(&current_device);
        cudaSetDevice(device.index());

        cudaError_t error = cudaDeviceSynchronize();
        cudaSetDevice(current_device);

        if (error != cudaSuccess) {
            throw std::runtime_error(fmt::format("Failed to synchronize CUDA device {}: {}",
                                                 device.index(), cudaGetErrorString(error)));
        }

        return;
    }
#endif

#ifdef BREZEL_USE_ROCM
    if (device.is_rocm()) {
        int current_device;
        hipGetDevice(&current_device);
        hipSetDevice(device.index());

        hipError_t error = hipDeviceSynchronize();
        hipSetDevice(current_device);

        if (error != hipSuccess) {
            const char* error_str = hipGetErrorString(error);
            throw std::runtime_error(
                fmt::format("Failed to synchronize ROCm device {}: {}", device.index(), error_str));
        }

        return;
    }
#endif

    throw std::runtime_error(fmt::format("Synchronization not implemented for device type: {}",
                                         device_type_to_string(device.type())));
}
}  // namespace brezel