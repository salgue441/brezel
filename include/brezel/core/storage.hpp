/**
 * @file storage.hpp
 * @author Carlos Salguero
 * @brief Defines the Storage class for tensor data
 * @version 0.1
 * @date 2025-04-27
 *
 * @copyright Copyright (c) 2025
 *
 * This file contains the Storage class, which manages memory allocation
 * and storage for tensor data across different devices.
 */

#pragma once

#include <cstddef>
#include <memory>
#include <stdexcept>

#include <brezel/core/device.hpp>
#include <brezel/core/dtype.hpp>
#include <brezel/core/exception.hpp>

namespace brezel {

/**
 * @brief Class for managing tensor data storage
 *
 * The Storage class is responsible for memory allocation, deallocation, and
 * data management for tensors. It handles different data types and devices.
 */
class Storage : public std::enable_shared_from_this<Storage> {
public:
    /**
     * @brief Default constructor (creates empty storage)
     */
    Storage() = default;

    /**
     * @brief Create storage with specified size and type
     *
     * @param num_bytes Size in bytes
     * @param dtype Data type
     * @param device Device to allocate on
     */
    Storage(size_t num_bytes, DType dtype, const Device& device = Device::CPU)
        : m_num_bytes(num_bytes), m_dtype(dtype), m_device(device) {
        if (num_bytes > 0) {
            allocate();
        }
    }

    /**
     * @brief Destructor
     */
    ~Storage() { deallocate(); }

    /**
     * @brief Move constructor
     *
     * @param other Storage to move from
     */
    Storage(Storage&& other) noexcept
        : m_data(other.m_data), m_num_bytes(other.m_num_bytes), m_dtype(other.m_dtype),
          m_device(other.m_device), m_owns_data(other.m_owns_data) {
        other.m_data = nullptr;
        other.m_num_bytes = 0;
        other.m_owns_data = false;
    }

    /**
     * @brief Move assignment
     *
     * @param other Storage to move from
     * @return Storage& Reference to this
     */
    Storage& operator=(Storage&& other) noexcept {
        if (this != &other) {
            deallocate();

            m_data = other.m_data;
            m_num_bytes = other.m_num_bytes;
            m_dtype = other.m_dtype;
            m_device = other.m_device;
            m_owns_data = other.m_owns_data;

            other.m_data = nullptr;
            other.m_num_bytes = 0;
            other.m_owns_data = false;
        }
        return *this;
    }

    /**
     * @brief Copy constructor (deleted)
     */
    Storage(const Storage&) = delete;

    /**
     * @brief Copy assignment (deleted)
     */
    Storage& operator=(const Storage&) = delete;

    /**
     * @brief Create a CPU storage and copy data from external source
     *
     * @tparam T Data type
     * @param data Source data
     * @param count Number of elements
     * @return std::shared_ptr<Storage> Shared pointer to new storage
     */
    template <typename T>
    static std::shared_ptr<Storage> from_cpu_data(const T* data, size_t count) {
        if (!data && count > 0) {
            throw std::invalid_argument(
                "Cannot create storage from null pointer with non-zero count");
        }

        const size_t bytes = count * sizeof(T);
        auto storage = std::make_shared<Storage>(bytes, get_dtype<T>(), Device::CPU);

        if (count > 0) {
            auto* dst = static_cast<T*>(storage->data());
            std::copy(data, data + count, dst);
        }

        return storage;
    }

    /**
     * @brief Create a new storage with the same properties but with new memory allocation
     *
     * @return std::shared_ptr<Storage> Clone of this storage
     */
    std::shared_ptr<Storage> clone() const {
        auto new_storage = std::make_shared<Storage>(m_num_bytes, m_dtype, m_device);

        if (m_num_bytes > 0 && m_data) {
            if (m_device.is_cpu()) {
                std::memcpy(new_storage->m_data, m_data, m_num_bytes);
            }
#ifdef BREZEL_USE_CUDA
            else if (m_device.is_cuda()) {
                // CUDA memory copy would go here
                throw std::runtime_error("CUDA memory copy not implemented");
            }
#endif
            else {
                throw std::runtime_error("Device not supported for clone");
            }
        }

        return new_storage;
    }

    /**
     * @brief Get a pointer to the data
     *
     * @return void* Pointer to the data
     */
    void* data() const noexcept { return m_data; }

    /**
     * @brief Get the size in bytes
     *
     * @return size_t Size in bytes
     */
    size_t size_bytes() const noexcept { return m_num_bytes; }

    /**
     * @brief Get the number of elements
     *
     * @return size_t Number of elements
     */
    size_t size_elements() const noexcept {
        return m_dtype == DType::Undefined ? 0 : m_num_bytes / dtype_size(m_dtype);
    }

    /**
     * @brief Get the data type
     *
     * @return DType Data type
     */
    DType dtype() const noexcept { return m_dtype; }

    /**
     * @brief Get the device
     *
     * @return const Device& Device
     */
    const Device& device() const noexcept { return m_device; }

    /**
     * @brief Check if the storage owns its data
     *
     * @return bool Whether the storage owns its data
     */
    bool owns_data() const noexcept { return m_owns_data; }

    /**
     * @brief Create a storage view from a subset of this storage
     *
     * @param offset Offset in bytes
     * @param size Size in bytes
     * @return std::shared_ptr<Storage> View storage
     * @throws std::out_of_range If the view extends beyond the storage
     */
    std::shared_ptr<Storage> view(size_t offset, size_t size) const {
        if (offset + size > m_num_bytes) {
            throw std::out_of_range("View extends beyond storage bounds");
        }

        auto view_storage = std::make_shared<Storage>();
        view_storage->m_data = static_cast<char*>(m_data) + offset;
        view_storage->m_num_bytes = size;
        view_storage->m_dtype = m_dtype;
        view_storage->m_device = m_device;
        view_storage->m_owns_data = false;

        return view_storage;
    }

    /**
     * @brief Copy data from another storage
     *
     * @param src Source storage
     * @throws std::invalid_argument If the storage sizes don't match
     * @throws std::runtime_error If cross-device copy is not supported
     */
    void copy_from(const Storage& src) {
        if (m_num_bytes != src.m_num_bytes) {
            throw std::invalid_argument("Storage sizes don't match for copy");
        }

        if (m_num_bytes == 0) {
            return;
        }

        if (m_device.is_cpu() && src.m_device.is_cpu()) {
            std::memcpy(m_data, src.m_data, m_num_bytes);
        }
#ifdef BREZEL_USE_CUDA
        else if (m_device.is_cuda() && src.m_device.is_cuda()) {
            // CUDA-to-CUDA copy would go here
            throw std::runtime_error("CUDA-to-CUDA copy not implemented");
        } else if (m_device.is_cuda() && src.m_device.is_cpu()) {
            // CPU-to-CUDA copy would go here
            throw std::runtime_error("CPU-to-CUDA copy not implemented");
        } else if (m_device.is_cpu() && src.m_device.is_cuda()) {
            // CUDA-to-CPU copy would go here
            throw std::runtime_error("CUDA-to-CPU copy not implemented");
        }
#endif
        else {
            throw std::runtime_error("Cross-device copy not supported");
        }
    }

    /**
     * @brief Convert to a storage on a different device
     *
     * @param device Target device
     * @return std::shared_ptr<Storage> Storage on the target device
     * @throws std::runtime_error If the device conversion is not supported
     */
    std::shared_ptr<Storage> to(const Device& device) const {
        if (m_device == device) {
            return std::const_pointer_cast<Storage>(shared_from_this());
        }

        auto new_storage = std::make_shared<Storage>(m_num_bytes, m_dtype, device);

        if (m_num_bytes == 0) {
            return new_storage;
        }

        if (m_device.is_cpu() && device.is_cpu()) {
            std::memcpy(new_storage->m_data, m_data, m_num_bytes);
        }
#ifdef BREZEL_USE_CUDA
        else if (m_device.is_cpu() && device.is_cuda()) {
            // CPU-to-CUDA copy would go here
            throw std::runtime_error("CPU-to-CUDA conversion not implemented");
        } else if (m_device.is_cuda() && device.is_cpu()) {
            // CUDA-to-CPU copy would go here
            throw std::runtime_error("CUDA-to-CPU conversion not implemented");
        } else if (m_device.is_cuda() && device.is_cuda()) {
            // CUDA-to-CUDA copy would go here
            throw std::runtime_error("CUDA-to-CUDA conversion not implemented");
        }
#endif
        else {
            throw std::runtime_error("Device conversion not supported");
        }

        return new_storage;
    }

    /**
     * @brief Convert to a storage with a different data type
     *
     * @param dtype Target data type
     * @return std::shared_ptr<Storage> Storage with the target data type
     * @throws std::runtime_error If the type conversion is not supported
     */
    std::shared_ptr<Storage> to(DType dtype) const {
        if (m_dtype == dtype) {
            return std::const_pointer_cast<Storage>(shared_from_this());
        }

        const size_t src_element_size = dtype_size(m_dtype);
        const size_t dst_element_size = dtype_size(dtype);
        const size_t num_elements = size_elements();
        const size_t new_size_bytes = num_elements * dst_element_size;

        auto new_storage = std::make_shared<Storage>(new_size_bytes, dtype, m_device);

        if (num_elements == 0) {
            return new_storage;
        }

        // Type conversion logic would go here
        // This is a placeholder; real implementation would handle all type combinations
        throw std::runtime_error("Type conversion not implemented");

        return new_storage;
    }

private:
    void* m_data = nullptr;            ///< Pointer to the data
    size_t m_num_bytes = 0;            ///< Size in bytes
    DType m_dtype = DType::Undefined;  ///< Data type
    Device m_device = Device::CPU;     ///< Device
    bool m_owns_data = true;           ///< Whether this storage owns its data

    /**
     * @brief Allocate memory
     *
     * @throws std::runtime_error If memory allocation fails
     */
    void allocate() {
        if (m_num_bytes == 0) {
            return;
        }

        if (m_device.is_cpu()) {
            m_data = std::malloc(m_num_bytes);
            if (!m_data) {
                throw std::runtime_error("Failed to allocate CPU memory");
            }
        }
#ifdef BREZEL_USE_CUDA
        else if (m_device.is_cuda()) {
            // CUDA memory allocation would go here
            throw std::runtime_error("CUDA memory allocation not implemented");
        }
#endif
        else {
            throw std::runtime_error("Device not supported for allocation");
        }

        m_owns_data = true;
    }

    /**
     * @brief Deallocate memory
     */
    void deallocate() {
        if (m_data && m_owns_data) {
            if (m_device.is_cpu()) {
                std::free(m_data);
            }
#ifdef BREZEL_USE_CUDA
            else if (m_device.is_cuda()) {
                // CUDA memory deallocation would go here
            }
#endif
        }

        m_data = nullptr;
    }
};

}  // namespace brezel