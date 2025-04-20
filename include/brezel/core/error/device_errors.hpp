/**
 * @file device_errors.hpp
 * @author Carlos Salguero
 * @brief Device-related error classes for the brezel framework
 * @version 0.1
 * @date 2025-04-19
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines error classes related to device operations
 * in the brezel framework
 */

#pragma once

#include <source_location>
#include <string>
#include <string_view>

#include <brezel/core/config.hpp>
#include <brezel/core/error/base.hpp>

namespace brezel::error {
/**
 * @brief Device-related errors
 */
class BREZEL_API DeviceError : public Error {
public:
    /**
     * @brief Construct with message and error code
     *
     * @param message Error message
     * @param code Error code
     * @param location Source location
     */
    DeviceError(std::string message, Code code = Code::DeviceError,
                const std::source_location& location = std::source_location::current())
        : Error(std::move(message), code, location) {}

    /**
     * @brief Construct with error context
     *
     * @param context Error context
     */
    explicit DeviceError(ErrorContext context) : Error(std::move(context)) {}
};

/**
 * @brief Indicates device is not found
 */
class BREZEL_API DeviceNotFoundError : public DeviceError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeviceNotFoundError(
        std::string message, const std::source_location& location = std::source_location::current())
        : DeviceError(std::move(message), Code::DeviceNotFound, location) {}

    /**
     * @brief Construct with device type and index
     *
     * @param device_type Type of device
     * @param device_index Index of device
     * @param location Source location
     */
    DeviceNotFoundError(std::string_view device_type, int device_index,
                        const std::source_location& location = std::source_location::current())
        : DeviceError(fmt::format("Device not found: type={}, index={}", device_type, device_index),
                      Code::DeviceNotFound, location) {}
};

/**
 * @brief Indicates device is not supported
 */
class BREZEL_API DeviceNotSupportedError : public DeviceError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeviceNotSupportedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : DeviceError(std::move(message), Code::DeviceNotSupported, location) {}
};

/**
 * @brief Indicates device was lost during operation
 */
class BREZEL_API DeviceLostError : public DeviceError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeviceLostError(std::string message,
                             const std::source_location& location = std::source_location::current())
        : DeviceError(std::move(message), Code::DeviceLost, location) {}
};

/**
 * @brief Indicates device has run out of memory
 */
class BREZEL_API DeviceOutOfMemoryError : public DeviceError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeviceOutOfMemoryError(
        std::string message, const std::source_location& location = std::source_location::current())
        : DeviceError(std::move(message), Code::DeviceOutOfMemory, location) {}

    /**
     * @brief Construct with requested size and available memory
     *
     * @param requested_bytes Requested allocation size
     * @param available_bytes Available memory
     * @param device_name Device name
     * @param location Source location
     */
    DeviceOutOfMemoryError(size_t requested_bytes, size_t available_bytes,
                           std::string_view device_name,
                           const std::source_location& location = std::source_location::current())
        : DeviceError(
              fmt::format("Device out of memory: requested={} bytes, available={} bytes on {}",
                          requested_bytes, available_bytes, device_name),
              Code::DeviceOutOfMemory, location) {}
};

/**
 * @brief Indicates device is not initialized
 */
class BREZEL_API DeviceNotInitializedError : public DeviceError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeviceNotInitializedError(
        std::string message, const std::source_location& location = std::source_location::current())
        : DeviceError(std::move(message), Code::DeviceNotInitialized, location) {}
};

/**
 * @brief Indicates device is already in use
 */
class BREZEL_API DeviceAlreadyInUseError : public DeviceError {
public:
    /**
     * @brief Construct with message
     *
     * @param message Error message
     * @param location Source location
     */
    explicit DeviceAlreadyInUseError(
        std::string message, const std::source_location& location = std::source_location::current())
        : DeviceError(std::move(message), Code::DeviceAlreadyInUse, location) {}
};

/**
 * @brief CUDA-specific error
 */
class BREZEL_API CudaError : public DeviceError {
public:
    /**
     * @brief Construct with CUDA error code and message
     *
     * @param cuda_error CUDA error code
     * @param message Additional context message
     * @param location Source location
     */
    CudaError(int cuda_error, std::string message,
              const std::source_location& location = std::source_location::current())
        : DeviceError(ErrorContext(std::move(message), Code::CudaError, Category::Cuda,
                                   std::nullopt, cuda_error, location)) {}

    /**
     * @brief Get the CUDA error code
     *
     * @return int CUDA error code
     */
    [[nodiscard]] int cuda_error() const noexcept { return library_error().value_or(-1); }

    /**
     * @brief Get the CUDA error name
     *
     * @return std::string CUDA error name
     */
    [[nodiscard]] std::string cuda_error_name() const;
};

/**
 * @brief OpenCL-specific error
 */
class BREZEL_API OpenCLError : public DeviceError {
public:
    /**
     * @brief Construct with OpenCL error code and message
     *
     * @param opencl_error OpenCL error code
     * @param message Additional context message
     * @param location Source location
     */
    OpenCLError(int opencl_error, std::string message,
                const std::source_location& location = std::source_location::current())
        : DeviceError(ErrorContext(std::move(message), Code::OpenCLError, Category::OpenCL,
                                   std::nullopt, opencl_error, location)) {}

    /**
     * @brief Get the OpenCL error code
     *
     * @return int OpenCL error code
     */
    [[nodiscard]] int opencl_error() const noexcept { return library_error().value_or(-1); }

    /**
     * @brief Get the OpenCL error name
     *
     * @return std::string OpenCL error name
     */
    [[nodiscard]] std::string opencl_error_name() const;
};
}  // namespace brezel::error