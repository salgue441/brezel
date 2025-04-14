#pragma once

#include <brezel/core/types.hpp>
#include <brezel/utils/exception.hpp>
#include <brezel/utils/macros.hpp>

#include <cstring>
#include <memory>
#include <string>

namespace brezel::devices {
/**
 * @brief Base device class that defines the interface for
 * all device implementations
 */
class Device {
public:
    /**
     * @brief Default constructor
     */
    Device() = default;

    /**
     * @brief Virtual destructor
     */
    virtual ~Device() = default;

    /**
     * @brief Get the device type
     *
     * @return DeviceType The type of the device
     */
    virtual DeviceType type() const = 0;

    /**
     * @brief Get the device name
     *
     * @return std::string The name of the device
     */
    virtual std::string name() const = 0;

    /**
     * @brief Check if the device is available
     *
     * @return true If the device is available
     * @return false If the device is not available
     */
    virtual bool is_available() const = 0;

    /**
     * @brief Allocate memory on the device
     *
     * @param size Size in bytes to allocate
     * @return void* Pointer to the allocated memory
     */
    virtual void* allocate(size_t size) = 0;

    /**
     * @brief Deallocate memory on the device
     *
     * @param ptr Pointer to the memory to deallocate
     */
    virtual void deallocate(void* ptr) = 0;

    /**
     * @brief Copy data from host to device
     *
     * @param dst Destination pointer (device memory)
     * @param src Source pointer (host memory)
     * @param size Size in bytes to copy
     */
    virtual void copy_host_to_device(void* dst, const void* src, size_t size) = 0;

    /**
     * @brief Copy data from device to host
     *
     * @param dst Destination pointer (host memory)
     * @param src Source pointer (device memory)
     * @param size Size in bytes to copy
     */
    virtual void copy_device_to_host(void* dst, const void* src, size_t size) = 0;

    /**
     * @brief Copy data from device to device
     *
     * @param dst Destination pointer (device memory)
     * @param src Source pointer (device memory)
     * @param size Size in bytes to copy
     */
    virtual void copy_device_to_device(void* dst, const void* src, size_t size) = 0;

    /**
     * @brief Fill device memory with a value
     *
     * @param ptr Pointer to the memory to fill
     * @param value Byte value to fill with
     * @param size Size in bytes to fill
     */
    virtual void memset(void* ptr, int value, size_t size) = 0;

    /**
     * @brief Synchronize the device
     *
     * Ensures all pending operations on the device are complete.
     */
    virtual void synchronize() = 0;
};

/**
 * @brief CPU implementation
 */
class CPU : public Device {
public:
    /**
     * @brief Default constructor
     */
    CPU() = default;

    /**
     * @brief Get the device type
     *
     * @return DeviceType Always DeviceType::CPU
     */
    DeviceType type() const override { return DeviceType::CPU; }

    /**
     * @brief Get the device name
     *
     * @return std::string "CPU"
     */
    std::string name() const override { return "CPU"; }

    /**
     * @brief Check if the device is available
     *
     * CPU is always available
     *
     * @return true Always returns true
     */
    bool is_available() const override { return true; }

    /**
     * @brief Allocates memory on the CPU
     *
     * @param size Size in bytes to allocate
     * @return void* Pointer to the allocated memory
     */
    void* allocate(size_t size) override {
        if (size == 0)
            return nullptr;

#if defined(BREZEL_PLATFORM_WINDOWS)
        return _aligned_malloc(size, BREZEL_DEFAULT_ALIGN);
#else
        return std::aligned_alloc(BREZEL_DEFAULT_ALIGN, size);
#endif
    }

    /**
     * @brief Deallocates memory on the CPU
     *
     * @param ptr Pointer to the memory to deallocate
     */
    void deallocate(void* ptr) override {
        if (ptr == nullptr)
            return;

#if defined(BREZEL_PLATFORM_WINDOWS)
        _aligned_free(ptr);
#else
        free(ptr);
#endif
    }

    /**
     * @brief Copy data from host to device (CPU to CPU)
     *
     * @param dst Destination pointer
     * @param src Source pointer
     * @param size Size in bytes to copy
     */
    void copy_host_to_device(void* dst, const void* src, size_t size) override {
        if (dst == nullptr || src == nullptr || size == 0)
            return;

        std::memcpy(dst, src, size);
    }

    /**
     * @brief Copy data from device to device (CPU to CPU)
     *
     * @param dst Destination pointer
     * @param src Source pointer
     * @param size Size in bytes to copy
     */
    void copy_device_to_device(void* dst, const void* src, size_t size) override {
        if (dst == nullptr || src == nullptr || size == 0)
            return;

        std::memcpy(dst, src, size);
    }

    /**
     * @brief Fill device memory with a value
     *
     * @param ptr Pointer to the memory to fill
     * @param value Byte value to fill with
     * @param size Size in bytes to fill
     */
    void memset(void* ptr, int value, size_t size) override {
        if (ptr == nullptr || size == 0)
            return;

        std::memset(ptr, value, size);
    }

    /**
     * @brief Synchronize the CPU
     *
     * No-op for CPU as operations are synchronous
     */
    void synchronize() override {}
};

#ifdef BREZEL_WITH_CUDA
// CUDA device would be implemented here
#endif
}  // namespace brezel::devices