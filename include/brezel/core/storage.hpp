#pragma once

#include <brezel/core/device.hpp>
#include <brezel/core/types.hpp>
#include <brezel/utils/exception.hpp>
#include <brezel/utils/macros.hpp>

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>

namespace brezel {
/**
 * @brief Class responsible for managing memory for tensor data
 *
 * Storage abstracts away device-specific memory management and
 * provides a unified interface for allocating and accesing
 * tensor data.
 *
 * @tparam T Data type
 * @tparam DeviceType Device type (CPU, CUDA, etc.)
 */
template <typename T, typename DeviceType = devices::CPU>
class Storage {
public:
    using value_type = T;
    using device_type = DeviceType;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using size_type = size_t;

    /**
     * @brief Creates an empty storage
     */
    Storage()
        : m_device(std::make_shared<DeviceType>()), m_data(nullptr), m_size(0), m_capacity(0) {}

    /**
     * @brief Creates a storage with the specified size
     *
     * @param size Number of elements to allocate
     * @param device Device to allocate on
     */
    explicit Storage(size_type size,
                     std::shared_ptr<DeviceType> device = std::make_shared<DeviceType>())
        : m_device(device), m_size(size), m_capacity(size) {
        if (size > 0) {
            m_data = static_cast<pointer>(m_device->allocate(size * sizeof(T)));

            if (!m_data) {
                std::string msg = fmt::format("Failed to allocate {} bytes on {}", size * sizeof(T),
                                              m_device->name());

                throw brezel::utils::MemoryError(msg);
            }
        } else {
            m_data = nullptr;
        }
    }

    /**
     * @brief Create a storage with the specified size and initial value
     *
     * @param size Number of elements to allocate
     * @param value Value to initialize elements with
     * @param device Device to allocate on
     */
    Storage(size_type size, const T& value,
            std::shared_ptr<DeviceType> device = std::make_shared<DeviceType>())
        : Storage(size, device) {
        if (size > 0) {
            if constexpr (std::is_same_v<DeviceType, devices::CPU>) {
                std::fill_n(m_data, size, value);
            } else {
                std::vector<T> host_data(size, value);
                m_device->copy_host_to_device(m_data, host_data.data(), size * sizeof(T));
            }
        }
    }

    /**
     * @brief Create a storage with the specified data
     *
     * @param data Data to copy
     * @param size Size of the data
     * @param device Device to allocate on
     */
    Storage(const T* data, size_type size,
            std::shared_ptr<DeviceType> device = std::make_shared<DeviceType>())
        : Storage(size, device) {
        if (size > 0 && data) {
            m_device->copy_host_to_device(m_data, data, size * sizeof(T));
        }
    }

    /**
     * @brief Create a storage from a vector
     *
     * @param data Vector containing the data
     * @param device Device to allocate on
     */
    Storage(const std::vector<T>& data,
            std::shared_ptr<DeviceType> device = std::make_shared<DeviceType>())
        : Storage(data.data(), data.size(), device) {}

    /**
     * @brief Copy constructor
     *
     * @param other Storage to copy from
     */
    Storage(const Storage& other)
        : m_device(other.m_device), m_size(other.m_size), m_capacity(other.m_capacity) {
        if (m_size > 0) {
            m_data = static_cast<pointer>(m_device->allocate(m_size * sizeof(T)));

            if (!m_data) {
                std::string msg = fmt::format("Failed to allocate {} bytes on {}",
                                              m_size * sizeof(T), m_device->name());

                throw brezel::utils::MemoryError(msg);
            }

            m_device->copy_device_to_device(m_data, other.m_data, m_size * sizeof(T));
        } else {
            m_data = nullptr;
        }
    }

    /**
     * @brief Move constructor
     *
     * @param other Storage to move from
     */
    Storage(Storage&& other) noexcept
        : m_device(std::move(other.m_device)),
          m_data(other.m_data),
          m_size(other.m_size),
          m_capacity(other.m_capacity) {
        other.m_data = nullptr;
        other.m_size = 0;
        other.m_capacity = 0;
    }

    // Operators
    /**
     * @brief Copy assignment operator
     *
     * @param other Storage to copy from
     * @return Storage& Reference to this
     */
    Storage& operator=(const Storage& other) {
        if (this != &other) {
            if (m_data) {
                m_device->deallocate(m_data);
            }

            m_device = other.m_device;
            m_size = other.m_size;
            m_capacity = other.m_capacity;

            if (m_size > 0) {
                m_data = static_cast<pointer>(m_device->allocate(size * sizeof(T)));

                if (!m_data) {
                    std::string msg = fmt::format("Failed to allocate {} bytes on {}",
                                                  m_size * sizeof(T), m_device->name());

                    throw brezel::utils::MemoryError(msg);
                }

                m_device->copy_device_to_device(m_data, other.m_data, m_size * sizeof(T));
            } else {
                m_data = nullptr;
            }
        }

        return *this;
    }

    /**
     * @brief Move assignment operator
     *
     * @param other Storage to move from
     * @return Storage& Reference to this
     */
    Storage& operator=(Storage&& other) noexcept {
        if (this != &other) {
            if (m_data) {
                m_device->deallocate(m_data);
            }

            m_device = std::move(other.m_device);
            m_data = other.m_data;
            m_size = other.m_size;
            m_capacity = other.m_capacity;

            other.m_data = nullptr;
            other.m_size = 0;
            other.m_capacity = 0;
        }

        return *this;
    }

    /**
     * @brief Destructor
     */
    ~Storage() {
        if (m_data) {
            m_device->deallocate(m_data);
        }
    }

    // Accessors
    /**
     * @brief Get the size of the storage
     *
     * @return size_type Number of elements
     */
    size_type size() const { return m_size; }

    /**
     * @brief Get the capacity of the storage
     *
     * @return size_type Number of elements that can be stored without reallocation
     */
    size_type capacity() const { return m_capacity; }

    /**
     * @brief Check if the storage is empty
     *
     * @return true If size is 0
     * @return false If size is > 0
     */
    bool empty() const { return m_size == 0; }

    /**
     * @brief Get a pointer to the data
     *
     * @return pointer Pointer to the data
     */
    pointer data() { return m_data; }

    /**
     * @brief Get a const pointer to the data
     *
     * @return const_pointer Const pointer to the data
     */
    const_pointer data() const { return m_data; }

    /**
     * @brief Get the device
     *
     * @return const device_type& Reference to the device
     */
    const device_type& device() const { return *m_device; }

    /**
     * @brief Get a shared pointer to the device
     *
     * @return std::shared_ptr<device_type> Shared pointer to the device
     */
    std::shared_ptr<device_type> device_ptr() const { return m_device; }

    // Methods
    /**
     * @brief Resize the storage
     *
     * If the new size is larger than the capacity, reallocation will occur.
     * If the new size is smaller than the current size, the storage will be
     * truncated.
     *
     * @param new_size New size
     */
    void resize(size_type new_size) {
        if (new_size == m_size) {
            return;
        }

        if (new_size <= m_capacity) {
            m_size = new_size;
            return;
        }

        pointer new_data = static_cast<pointer>(m_device->allocate(new_size * sizeof(T)));
        if (!new_data) {
            std::string msg = fmt::format("Failed to allocate {} bytes on {}", new_size * sizeof(T),
                                          m_device->name());

            throw brezel::utils::MemoryError(msg);
        }

        if (m_data && m_size > 0) {
            m_device->copy_device_to_device(new_data, m_data, m_size * sizeof(T));
            m_device->deallocate(m_data);
        }

        m_data = new_data;
        m_size = new_size;
        m_capacity = new_size;
    }

    /**
     * @brief Reserve capacity for the storage
     *
     * If the new capacity is larger than the current capacity, reallocation
     * will occur. Otherwise, this function does nothing.
     *
     * @param new_capacity New capacity
     */
    void reserve(size_type new_capacity) {
        if (new_capacity <= m_capacity) {
            return;
        }

        pointer new_data = static_cast<pointer>(m_device->allocate(new_capacity * sizeof(T)));
        if (!new_data) {
            std::string msg = fmt::format("Failed to allocate {} bytes on {}",
                                          new_capacity * sizeof(T), m_device->name());

            throw brezel::utils::MemoryError(msg);
        }

        if (m_data && m_size > 0) {
            m_device->copy_device_to_device(new_data, m_data, m_size * sizeof(T));
            m_device->deallocate(m_data);
        }

        m_data = new_data;
        m_capacity = new_capacity;
    }

    /**
     * @brief Copy data from host to device
     *
     * @param host_data Pointer to host data
     * @param count Number of elements to copy
     */
    void copy_from_host(const T* host_data, size_type count) {
        if (!host_data || count == 0) {
            return;
        }

        if (count > m_size) {
            resize(count);
        }

        m_device->copy_host_to_device(data_, host_data, count * sizeof(T));
    }

    /**
     * @brief Copy data from device to host
     *
     * @param host_data Pointer to host buffer
     * @param count Number of elements to copy
     */
    void copy_to_host(T* host_data, size_type count) const {
        if (!host_data || count == 0) {
            return;
        }

        count = std::min(count, m_size);
        m_device->copy_device_to_host(host_data, m_data, count * sizeof(T));
    }

    /**
     * @brief Copy data to a host vector
     *
     * @return std::vector<T> Vector containing a copy of the data
     */
    std::vector<T> to_vector() const {
        std::vector<T> result(m_size);
        if (m_size > 0) {
            copy_to_host(result.data(), m_size);
        }

        return result;
    }

    /**
     * @brief Fill the storage with a value
     *
     * @param value Value to fill with
     */
    void fill(const T& value) {
        if (m_size == 0) {
            return;
        }

        if constexpr (std::is_same_v<DeviceType, devices::CPU>) {
            std::fill_n(m_data, m_size, value);
        } else {
            if constexpr (std::is_arithmetic_v<T>) {
                if (value == T(0)) {
                    m_device->memset(m_data, 0, m_size * sizeof(T));
                    return;
                }
            }

            std::vector<T> host_data(m_size, value);
            m_device->copy_host_to_device(m_data, host_data.data(), m_size * sizeof(T));
        }
    }

    /**
     * @brief Clone the storage
     *
     * @return Storage New storage with a copy of the data
     */
    Storage clone() const {
        Storage result(m_size, m_device);
        if (m_size > 0) {
            m_device->copy_device_to_device(result.m_data, m_data, m_size * sizeof(T));
        }

        return result;
    }

    /**
     * @brief Move the storage to a different device
     *
     * @tparam NewDeviceType New device type
     * @param new_device New device
     * @return Storage<T, NewDeviceType> Storage on the new device
     */
    template <typename NewDeviceType>
    Storage<T, NewDeviceType> to_device(std::shared_ptr<NewDeviceType> new_device) const {
        if constexpr (std::is_same_v<DeviceType, NewDeviceType>) {
            if (m_device.get() == new_device.get()) {
                return clone();
            }
        }

        Storage<T, NewDeviceType> result(m_size, new_device);
        if (m_size > 0) {
            if constexpr (std::is_same_v<DeviceType, devices::CPU> &&
                          std::is_same_v<NewDeviceType, devices::CPU>) {
                new_device->copy_device_to_device(result.data(), m_data, m_size * sizeof(T));
            } else {
                std::vector<T> host_data = to_vector();
                result.copy_from_host(host_data.data(), m_size);
            }
        }

        return result;
    }

private:
    std::shared_ptr<device_type> m_device;
    pointer m_data;
    size_type m_size;
    size_type m_capacity;
};
}  // namespace brezel