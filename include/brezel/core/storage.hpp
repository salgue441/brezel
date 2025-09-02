/**
 * @file storage.hpp
 * @author Carlos Salguero
 * @brief Tensor storage implementation with type erasure and allocator support
 * @version 0.1
 * @date 2025-09-02
 *
 * @copyright Copyright (c) 2025
 *
 * This file defines the Storage class template and related utilities for
 * managing tensor data storage with the following features:
 *
 * - Custom allocator support with proper propagation semantics
 * - Type-erased storage interface for runtime polymorphism
 * - Exception safety and strong exception guarantees
 * - Bounds checking in debug builds
 * - Comprehensive logging and error reporting
 * - STL container-like interface
 *
 * Key components:
 * - Storage: Templated storage container with allocator support
 * - StorageBase: Type-erased interface for runtime storage manipulation
 * - TypeErasedStorage: Concrete implementation of type-erased storage
 * - make_storage: Factory function for creating type-erased storage instances
 *
 * The Storage class provides:
 * - RAII memory management with custom allocators
 * - Copy and move semantics with allocator propagation
 * - Random access iterators and element access methods
 * - Dynamic resizing with geometric growth strategy
 * - Type information via DType system
 * - Thread-safe operations (when allocator is thread-safe)
 *
 * @note This implementation follows the C++ standard library container requirements
 *       and provides strong exception safety guarantees for all operations.
 *
 * @see DType for supported data types
 * @see concepts.hpp for type constraints
 * @see error.hpp for assertion macros
 * @see logging.hpp for logging facilities
 */

#pragma once

#include <memory>
#include <span>
#include <type_traits>
#include <utility>

#include "brezel/core/concepts.hpp"
#include "brezel/core/dtype.hpp"
#include "brezel/utils/error.hpp"
#include "brezel/utils/logging.hpp"

namespace brezel::core {
/**
 * @brief A container for tensor data storage with custom allocator support.
 *
 * The `Storage` class provides a dynamic array-like container specifically
 * designed for tensor operations. It supports custom allocators, type safety
 * through concepts, and provides strong exception safety guarantees.
 *
 * @tparam T The element type, must satisfy `ScalarType` concept.
 * @tparam Allocator The allocator type (defaults to `std::allocator<T>`)
 *
 * @note This class follows the standard library contianer interface and
 *       provides allocator-aware copy/move semantics with proper propagation.
 *
 * @exception Strong exception safety is provided for all operations unless
 *            otherwise specified. Operations that may throw include:
 *            - Memory allocation failures
 *            - Element construction/destruction failures
 *            - Out-of-bounds access (in debug builds)
 *
 * @see ScalarType
 * @see SupportedType
 * @see DTYpe
 */
template <ScalarType T, typename Allocator = std::allocator<T>>
class Storage {
  public:
    using value_type = T;
    using allocator_type = Allocator;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;
    using iterator = pointer;
    using const_iterator = const_pointer;

    static_assert(SupportedType<T>, "Unsupported storage type");

  public:
    /**
     * @brief Default constructor.
     *
     * Creates an empty storage with default-constructed allocator.
     */
    Storage() noexcept(std::is_nothrow_default_constructible_v<Allocator>)
        : m_data(nullptr), m_size(0), m_capacity(0), m_allocator() {}

    /**
     * @brief Constructs with a specific allocator.
     *
     * @param alloc The allocator to use
     */
    explicit Storage(const Allocator &allocator) noexcept : m_data(nullptr), m_size(0), m_capacity(0), m_allocator(0) {}

    /**
     * @brief Constructs with specified size.
     *
     * Creates storage with `size` default-constructed elements.
     *
     * @param size Number of elements to create
     * @param alloc Allocator to use (defaults to default allocator)
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    explicit Storage(size_type size, const Allocator &alloc = Allocator())
        : m_data(nullptr), m_size(0), m_capacity(0), m_allocator(alloc) {
        resize(size);
    }

    /**
     * @brief Constructs with specified size and value.
     *
     * Creates storage with `size` copies of `value`.
     *
     * @param size Number of elements to create
     * @param value Value to initialize elements with
     * @param alloc Allocator to use (defaults to default allocator)
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    Storage(size_type size, const value_type &value, const Allocator &alloc = Allocator())
        : m_data(nullptr), m_size(0), m_capacity(0), m_allocator(alloc) {
        resize(size, value);
    }

    /**
     * @brief Constructs from a span of data.
     *
     * Copies elements from the provided span.
     *
     * @param data Span containing elements to copy
     * @param alloc Allocator to use (defaults to default allocator)
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    explicit Storage(std::span<const T> data, const Allocator &alloc = Allocator())
        : m_data(nullptr), m_size(0), m_capacity(0), m_allocator(alloc) {
        assign(data.begin(), data.end());
    }

    /**
     * @brief Constructs from a range of elements.
     *
     * @tparam InputIt Input iterator type
     * @param first Beginning of the range
     * @param last End of the range
     * @param alloc Allocator to use (defaults to default allocator)
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    template <std::input_iterator InputIt>
    Storage(InputIt first, InputIt last, const Allocator &alloc = Allocator())
        : m_data(nullptr), m_size(0), m_capacity(0), m_allocator(alloc) {
        assign(first, last);
    }

    /**
     * @brief Constructs from initializer list.
     *
     * @param init Initializer list with elements
     * @param alloc Allocator to use (defaults to default allocator)
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    Storage(std::initializer_list<T> init, const Allocator &alloc = Allocator())
        : Storage(init.begin(), init.end(), alloc) {}

    /**
     * @brief Copy constructor.
     *
     * Copies elements and uses allocator copy construction traits.
     *
     * @param other Storage to copy from
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    Storage(const Storage &other)
        : m_data(nullptr),
          m_size(0),
          m_capacity(0),
          m_allocator(std::allocator_traits<Allocator>::select_on_container_copy_construction(other.m_allocator)) {
        assign(other.begin(), other.end());
    }

    /**
     * @brief Copy constructor with specific allocator.
     *
     * @param other Storage to copy from
     * @param alloc Allocator to use
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    Storage(const Storage &other, const Allocator &alloc)
        : m_data(nullptr), m_size(0), m_capacity(0), m_allocator(alloc) {
        assign(other.begin(), other.end());
    }

    /**
     * @brief Move constructor.
     *
     * Transfers ownership of resources from another storage.
     *
     * @param other Storage to move from (will be left in valid but unspecified state)
     */
    Storage(Storage &&other) noexcept
        : m_data(std::exchange(other.m_data, nullptr)),
          m_size(std::exchange(other.m_size, 0)),
          m_capacity(std::exchange(other.m_capacity, 0)),
          m_allocator(std::move(other.m_allocator)) {}

    /**
     * @brief Move constructor with specific allocator.
     *
     * If allocators are equal, transfers ownership. Otherwise, copies elements.
     *
     * @param other Storage to move from
     * @param alloc Allocator to use
     * @exception noexcept if element moves are noexcept, otherwise may throw
     */
    Storage(Storage &&other, const Allocator &alloc) noexcept : m_allocator(alloc) {
        if (m_allocator == other.m_allocator) {
            m_data = std::exchange(other.m_data, nullptr);
            m_size = std::exchange(other.m_size, 0);
            m_capacity = std::exchange(other.m_capacity, 0);
        } else {
            m_data = nullptr;
            m_size = 0;
            m_capacity = 0;

            assign(std::make_move_iterator(other.begin()), std::make_move_iterator(other.end()));
        }
    }

    /**
     * @brief Destructor.
     *
     * Destroys all elements and deallocates memory.
     */
    ~Storage() noexcept {
        clear();
        deallocate();
    }

    // Group: Assignment Operators
    /// @{

    /**
     * @brief Copy assignment operator.
     *
     * @param other Storage to copy from
     * @return Reference to this storage
     * @exception May throw std::bad_alloc or exception from element
     *            construction
     */
    Storage &operator=(const Storage &other) {
        if (this != &other) {
            if constexpr (std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value) {
                if (m_allocator != other.m_allocator) {
                    clear();
                    deallocate();

                    m_allocator = other.m_allocator;
                }
            }

            assign(other.begin(), other.end());
        }

        return *this;
    }

    /**
     * @brief Move assignment operator.
     *
     * @param other Storage to move from
     * @return Reference to this storage
     * @exception noexcept if allocators allow it, otherwise may throw
     */
    Storage &operator=(Storage &&other) noexcept {
        if (this != &other) {
            clear();
            deallocate();

            if constexpr (std::allocator_traits<Allocator>::propagate_on_container_move_assignment::value ||
                          std::allocator_traits<Allocator>::is_always_equal::value) {
                m_data = std::exchange(other.m_data, nullptr);
                m_size = std::exchange(other.m_size, 0);
                m_capacity = std::exchange(other.m_capacity, 0);

                if constexpr (std::allocator_traits<Allocator>::propagate_on_container_move_assignment::value) {
                    m_allocator = std::move(other.m_allocator);
                }
            } else {
                assign(std::make_move_iterator(other.begin()), std::make_move_iterator(other.end()));
            }
        }

        return *this;
    }

    /**
     * @brief Initializer list assignment operator.
     *
     * @param init Initializer list to assign from
     * @return Reference to this storage
     * @exception May throw std::bad_alloc or exceptions from element
     *            construction
     */
    Storage &operator=(std::initializer_list<T> init) {
        assign(init);
        return *this;
    }
    /// @}

    // Group: Element Access
    /// @{

    /**
     * @brief Subscript operator (no bounds checking).
     *
     * @param pos Position of the element to access
     * @return Reference to the element at position `pos`
     * @exception noexcept
     * @pre `pos < size()`
     * @warning In debug builds, asserts bounds checking. In release builds,
     *          accessing out-of-bounds is undefined behavior.
     */
    reference operator[](size_type pos) noexcept {
        BREZEL_ASSERT(pos < m_size, "Storage index out of bounds");
        return m_data[pos];
    }

    /**
     * @brief Const subscript operator (no bounds checking).
     *
     * @param pos Position of the element to access
     * @return Const reference to the element at position `pos`
     * @exception noexcept
     * @pre `pos < size()`
     */
    const_reference operator[](size_type pos) const noexcept {
        BREZEL_ASSERT(pos < m_size, "Storage index out of bounds");
        return m_data[pos];
    }

    /**
     * @brief Access element with bounds checking.
     *
     * @param pos Position of the element to access
     * @return Reference to the element at position `pos`
     * @exception std::out_of_range if `pos >= size()`
     */
    reference at(size_type pos) {
        if (pos >= m_size) {
            throw std::out_of_range("Storage index out of range");
        }

        return m_data[pos];
    }

    /**
     * @brief Const access element with bounds checking.
     *
     * @param pos Position of the element to access
     * @return Const reference to the element at position `pos`
     * @exception std::out_of_range if `pos >= size()`
     */
    const_reference at(size_type pos) const {
        if (pos >= m_size) {
            throw std::out_of_range("Storage index out of range");
        }

        return m_data[pos];
    }

    /**
     * @brief Access first element.
     *
     * @return Reference to the first element
     * @exception noexcept
     * @pre `!empty()`
     */
    reference front() noexcept {
        BREZEL_ASSERT(!empty(), "Storage is empty");
        return m_data[0];
    }

    /**
     * @brief Access first element (const).
     *
     * @return Const reference to the first element
     * @exception noexcept
     * @pre `!empty()`
     */
    const_reference front() const noexcept {
        BREZEL_ASSERT(!empty(), "Storage is empty");
        return m_data[0];
    }

    /**
     * @brief Access last element.
     *
     * @return Reference to the last element
     * @exception noexcept
     * @pre `!empty()`
     */
    reference back() noexcept {
        BREZEL_ASSERT(!empty(), "Storage is empty");
        return m_data[m_size - 1];
    }

    /**
     * @brief Access last element (const).
     *
     * @return Const reference to the last element
     * @exception noexcept
     * @pre `!empty()`
     */
    const_reference back() const noexcept {
        BREZEL_ASSERT(!empty(), "Storage is empty");
        return m_data[m_size - 1];
    }

    /**
     * @brief Direct access to underlying data.
     *
     * @return Pointer to the underlying element array
     * @exception noexcept
     */
    pointer data() noexcept { return m_data; }

    /**
     * @brief Direct access to underlying data (const).
     *
     * @return Const pointer to the underlying element array
     * @exception noexcept
     */
    const_pointer data() const noexcept { return m_data; }
    /// @}

    // Group: Iterators
    /// @{

    /**
     * @brief Returns iterator to the beginning.
     *
     * @return Iterator to the first element
     */
    iterator begin() noexcept { return m_data; }

    /**
     * @brief Returns const iterator to the beginning.
     *
     * @return Const iterator to the first element
     */
    const_iterator begin() const noexcept { return m_data; }

    /**
     * @brief Returns const iterator to the beginning.
     *
     * @return Const iterator to the first element
     */
    const_iterator cbegin() const noexcept { return m_data; }

    /**
     * @brief Returns iterator to the end.
     *
     * @return Iterator to the element following the last element
     */
    iterator end() noexcept { return m_data + m_size; }

    /**
     * @brief Returns const iterator to the end.
     *
     * @return Const iterator to the element following the last element
     */
    const_iterator end() const noexcept { return m_data + m_size; }

    /**
     * @brief Returns const iterator to the end.
     *
     * @return Const iterator to the element following the last element
     */
    const_iterator cend() const noexcept { return m_data + m_size; }
    /// @}

    // Group: Capacity
    /// @{

    /**
     * @brief Checks whether the storage is empty.
     *
     * @return true if storage is empty, false otherwise
     * @exception noexcept
     */
    bool empty() const noexcept { return m_size == 0; }

    /**
     * @brief Returns the number of elements.
     *
     * @return Number of elements in the storage
     * @exception noexcept
     */
    size_type size() const noexcept { return m_size; }

    /**
     * @brief Returns the maximum possible number of elements.
     *
     * @return Maximum number of elements limited by allocator
     * @exception noexcept
     */
    size_type max_size() const noexcept { return std::allocator_traits<Allocator>::max_size(m_allocator); }

    /**
     * @brief Returns the number of elements that can be held in allocated
     * storage.
     *
     * @return Capacity of the currently allocated storage
     * @exception noexcept
     */
    size_type capacity() const noexcept { return m_capacity; }

    /**
     * @brief Increases the capacity to a value greater than or equal to
     * new_cap.
     *
     * If new_cap is greater than the current capacity(), new storage is
     * allocated, otherwise the function does nothing.
     *
     * @param new_cap New capacity
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element move/copy constructors
     */
    void reserve(size_type new_cap) {
        if (new_cap <= m_capacity) return;

        BREZEL_LOG_TRACE("Storage reserve: {} -> {} elements", m_capacity, new_cap);

        pointer new_data = std::allocator_traits<Allocator>::allocate(m_allocator, new_cap);

        try {
            if (m_data) {
                if constexpr (std::is_nothrow_move_constructible_v<T>) {
                    std::uninitialized_move(m_data, m_data + m_size, new_data);
                } else {
                    std::uninitialized_copy(m_data, m_data + m_size, new_data);
                }
            }

            m_data = new_data;
            m_capacity = new_cap;
        } catch (...) {
            std::allocator_traits<Allocator>::deallocate(m_allocator, new_data, new_cap);

            throw;
        }
    }

    /**
     * @brief Reduces memory usage by freeing unused memory.
     *
     * Requests the removal of unused capacity. After this call,
     * `capacity() == size()`.
     *
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element move/copy constructors
     */
    void shrink_to_fit() {
        if (m_size == m_capacity) return;
        if (m_size == 0) {
            deallocate();
            return;
        }

        BREZEL_LOG_TRACE("Storage shrink_to_fit: {} -> {} elements", m_capacity, m_size);

        pointer new_data = std::allocator_traits<Allocator>::allocate(m_allocator, m_size);

        try {
            if constexpr (std::is_nothrow_move_constructible<T>) {
                std::uninitialized_move(m_data, m_data + m_size, new_data);
            } else {
                std::uninitialized_copy(m_data, m_data + m_size, new_data);
            }

            std::destroy(m_data, m_data + m_size);
            std::allocator_traits<Allocator>::deallocate(m_allocator, m_data, m_capacity);

            m_data = new_data;
            m_capacity = m_size;
        } catch (...) {
            std::allocator_traits<Allocator>::deallocate(m_allocator, new_data, m_size);

            throw;
        }
    }
    /// @}

    // Group: Modifiers
    /// @{

    /**
     * @brief Removes all elements from the storage.
     *
     * Destroys all elements and sets size to 0, but does not deallocate memory.
     *
     * @exception noexcept
     */
    void clear() noexcept {
        if (m_data) {
            std::destroy(m_data, m_data + m_size);
            m_size = 0;
        }
    }

    /**
     * @brief Changes the number of elements stored.
     *
     * If `count < size()`, reduces size by destroying excess elements.
     * If `count > size()`, adds default-constructed elements.
     *
     * @param count New size
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element construction
     */
    void resize(size_type count) {
        if (count < m_size) {
            std::destroy(m_data + count, m_data + m_size);
            m_size = count;
        } else if (count > m_size) {
            if (count > m_capacity) reserve(calculate_growth(count));

            std::uninitialized_default_construct(m_data + m_size, m_data + count);
            m_size = count;
        }
    }

    /**
     * @brief Changes the number of elements stored with specified value.
     *
     * If `count < size()`, reduces size by destroying excess elements.
     * If `count > size()`, adds copies of `value`.
     *
     * @param count New size
     * @param value Value to initialize new elements with
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element construction
     */
    void resize(size_type count, const value_type &value) {
        if (m_count < m_size) {
            std::destroy(m_data + count, m_data + m_size);
            m_size = count;
        } else if (count > m_size) {
            if (count > m_capacity) reserve(calculate_growth(count));

            std::uninitialized_fill(m_data + m_size, m_data + count, value);
            m_size = count;
        }
    }

    /**
     * @brief Replaces the contents with count copies of value.
     *
     * @param count New size
     * @param value Value to initialize elements with
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element construction
     */
    void assign(size_type count, const T &value) {
        clear();
        if (count > m_capacity) {
            deallocate();
            reserve(count);
        }

        std::uninitialized_fill_n(m_data, count, value);
        m_size = count;
    }

    /**
     * @brief Replaces the contents with elements from a range.
     *
     * @tparam InputIt Input iterator type
     * @param first Beginning of the range
     * @param last End of the range
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element construction
     *
     * @note For input iterators, we can't know the size in advance
     */
    template <std::input_iterator InputIt>
    void assign(InputIn first, InputIn last) {
        clear();
        if constexpr (std::random_access_iterator<InputIt>) {
            size_type count = std::distance(first, last);
            if (count > capacity_) {
                deallocate();
                reserve(count);
            }

            std::uninitialized_copy(first, last, data_);
            m_size = count;
        } else {
            size_type new_size = 0;
            for (auto it = first; it != last; ++it) {
                if (new_size >= m_capacity) {
                    reserve(calculate_growth(new_size + 1));
                }

                std::construct_at(m_data + m_size, *it);
                ++new_size;
            }

            m_size = new_size;
        }
    }

    /**
     * @brief Replaces the contents with elements from initializer list.
     *
     * @param init Initializer list to assign from
     * @exception std::bad_alloc if allocation fails
     * @exception Any exception thrown by element construction
     */
    void assign(std::initializer_list<T> init) { assign(init.begin(), init.end()); }

    /**
     * @brief Exchanges the contents with another storage.
     *
     * @param other Storage to exchange contents with
     * @exception noexcept if allocators allow it
     */
    void swap(Storage &other) noexcept {
        using std::swap;

        if constexpr (td::allocator_traits<Allocator>::propagate_on_container_swap::value ||
                      std::allocator_traits<Allocator>::is_always_equal::value) {
            swap(m_data, other.m_data);
            swap(m_size, other.m_size);
            swap(m_capacity, other.m_capacity);

            if constexpr (std::allocator_traits<Allocator>::propagate_on_container_swap::value) {
                swap(m_allocator, other.m_allocator);
            }
        } else {
            Storage temp = std::move(this);
            *this = std::move(other);
            other = std::move(temp);
        }
    }
    /// @}

    // Group: Allocator and Type Information
    /// @{

    /**
     * @brief Returns the associated allocator.
     *
     * @return The allocator instance.
     */
    allocator_type get_allocator() const noexcept { return m_allocator; }

    /**
     * @brief Returns the data type of stored elements.
     *
     * @return DType enum value representing the element type.
     */
    constexpr DType dtype() const noexcept { return dtype_v<T>; }

    /**
     * @brief Returns the size of each element in bytes.
     *
     * @return Size of element type T in bytes
     */
    constexpr size_type element_size() const noexcept { return sizeof(T); }

    /// @}

    // Group: Operators
    /// @{

    /**
     * @brief Checks equality of two storages.
     *
     * Two storages are equal if they have the same size and their elements
     * compare equal element-wise.
     *
     * @param other Storage to compare with
     * @return true if storages are equal, false otherwise
     * @exception noexcept if element comparisons are noexcept
     */
    bool operator==(const Storage &other) const noexcept {
        return m_size == other.m_size && std::equal(begin(), end(), other.begin());
    }

    /**
     * @brief Checks inequality of two storages.
     *
     * @param other Storage to compare with
     * @return true if storages are not equal, false otherwise
     * @exception noexcept if element comparisons are noexcept
     */
    bool operator!=(const Storage &other) const noexcept { return !(*this == other); }
    /// @}

  private:
    /**
     * @brief Deallocates the underlying storage.
     */
    void deallocate() const {
        if (m_data) {
            std::allocator_traits<Allocator>::deallocate(m_allocator, m_data, m_capacity);

            m_data = nullptr;
            m_capacity = 0;
        }
    }

    /**
     * @brief Calculates new capacity for growth.
     *
     * Uses geometric growth strategy (at least doubling) while respecting
     * `max_size()`.
     *
     * @param new_size Requeted new size.
     * @return size_type Recommended new capacity.
     * @exception std::length_error if `new_size` exceeds `max_size()`.
     */
    size_type calculate_growth(size_type new_size) const {
        const size_type max_size_val = max_size();
        if (new_size > max_size_val) {
            throw std::length_error("Storage size exceeds maximum");
        }

        const size_type geometric_growth =
            m_capacity > max_size_val - m_capacity ? max_size_val : m_capacity + m_capacity;

        return std::max(new_size, geometric_growth);
    }

  private:
    pointer m_data;
    size_type m_size;
    size_type m_capacity;
    [[no_unique_address]] Allocator m_allocator;
};

/**
 * @brief Exchanges the contents of two `Storage` objects.
 *
 * This non-member function provides a convenient way to swap the contents
 * of two `Storage` objects with the same template parameters. It delegates
 * to the member swap function, which handles allocator propagation according
 * to the allocator traits.
 *
 * @tparam T The element type, must satisfy ScalarType concept
 * @tparam Allocator The allocator type
 * @param lhs First storage to swap
 * @param rhs Second storage to swap
 *
 * @exception noexcept This function is noexcept if the member swap function
 *                     is noexcept, which depends on the allocator properties:
 *                     - If allocators propagate on swap or are always equal,
 *                       the swap is a pointer swap and noexcept
 *                     - Otherwise, it may involve element moves and could throw
 *                       if element move operations throw
 *
 * @note The behavior depends on the allocator traits:
 *       - If `propagate_on_container_swap` is true or allocators are always
 *         equal, swaps pointers and metadata (O(1) complexity)
 *       - Otherwise, performs element-wise moving (O(n) complexity)
 *
 * @complexity Constant time if allocators allow pointer swapping,
 *             linear in the size of the containers otherwise.
 *
 * @see Storage::swap()
 * @see std::allocator_traits<Allocator>::propagate_on_container_swap
 * @see std::allocator_traits<Allocator>::is_always_equal
 *
 * @example
 * Storage<int> a{1, 2, 3};
 * Storage<int> b{4, 5, 6};
 * swap(a, b);
 * // a now contains {4, 5, 6}, b contains {1, 2, 3}
 */
template <ScalarType T, typename Allocator>
void swap(Storage<T, Allocator> &lhs, Storage<T, Allocator> &rhs) noexcept {
    lhs.swap(rhs);
}

/**
 * @brief Abstract base class for type-erased tensor storage.
 *
 * `StorageBase` provides a runtime polymorphic interface for tensor storage
 * that abstracts away the specific element type. This enables operations on
 * tensors without compile-time knowledge of their data type.
 *
 * This interface is designed for:
 * - Runtime type introspection and manipulation
 * - Heterogeneous collections of tensors
 * - Serialization/deserialization of tensor data
 * - Plugin systems and dynamic loading of tensor operations
 *
 * @note All derived classes must implement the pure virtual functions.
 * @note The interface provides noexcept guarantees where appropriate.
 * @note This is an abstract class and cannot be instantiated directly.
 *
 * @see TypeErasedStorage for a concrete implementation
 * @see make_storage() for factory function
 *
 * @invariant For any valid StorageBase implementation:
 *   - `data()` returns a valid pointer if size() > 0, nullptr otherwise
 *   - `element_size() > 0` for any concrete storage type
 *   - `dtype()` returns a valid DType value
 *   - `clone()` returns a deep copy of the storage
 *   - `resize()` maintains storage validity (may throw on allocation failure)
 *   - `clear()` leaves the storage in a valid empty state
 */
class StorageBase {
  public:
    /**
     * @brief Virtual destructor for proper polymorphic destruction.
     *
     * Ensures that derived classes are properly destroyed when deleted
     * through a base class pointer.
     */
    virtual ~StorageBase() = default;

    /**
     * @brief Returns a pointer to the underlying data buffer.
     *
     * The returned pointer provides type-erased access to the stored elements.
     * The actual element type can be determined using dtype().
     *
     * @return void* Pointer to the data buffer, or nullptr if empty
     */
    virtual void *data() noexcept = 0;

    /**
     * @brief Returns a const pointer to the underlying data buffer.
     *
     * @return const void* Const pointer to the data buffer, or nullptr if empty
     */
    virtual const void *data() const noexcept = 0;

    /**
     * @brief Returns the number of elements in the storage.
     *
     * @return size_t Number of elements (not bytes)
     */
    virtual size_t size() const noexcept = 0;

    /**
     * @brief Returns the size of each element in bytes.
     *
     * This can be used with data() to calculate total byte size:
     * `total_bytes = size() * element_size()`
     *
     * @return size_t Size of individual elements in bytes
     */
    virtual size_t element_size() const noexcept = 0;

    /**
     * @brief Returns the data type of the stored elements.
     *
     * @return DType The data type enum value
     */
    virtual DType dtype() const noexcept = 0;

    /**
     * @brief Creates a deep copy of the storage.
     *
     * This is a virtual constructor pattern that allows copying without
     * knowing the concrete derived type at compile time.
     *
     * @return std::unique_ptr<StorageBase> Unique pointer to a new copy
     * @exception May throw std::bad_alloc or exceptions from element copying
     */
    virtual std::unique_ptr<StorageBase> clone() const = 0;

    /**
     * @brief Resizes the storage to contain new_size elements.
     *
     * If `new_size > current size`, new elements are default-constructed.
     * If `new_size < current size`, elements are destroyed from the end.
     *
     * @param new_size New number of elements
     * @exception std::bad_alloc if memory allocation fails
     * @exception Any exception thrown by element construction/destruction
     */
    virtual void resize(size_t new_size) = 0;

    /**
     * @brief Removes all elements from the storage.
     *
     * Destroys all elements and sets size to 0, but may retain allocated
     * memory for future use. The storage remains in a valid state.
     */
    virtual void clear() noexcept = 0;
};

/**
 * @brief Concrete implementation of type-erased tensor storage.
 *
 * `TypeErasedStorage` wraps a typed `Storage` object and implements the
 * `StorageBase` interface, providing runtime polymorphism while maintaining
 * type safety internally. This allows operations on tensors without
 * compile-time knowledge of their specific data type.
 *
 * This class is designed for:
 * - Runtime tensor operations with type introspection
 * - Heterogeneous collections of tensors with different data types
 * - Serialization/deserialization where type information is determined at runtime
 * - Plugin systems that need to handle arbitrary tensor types
 *
 * @tparam T The element type, must satisfy `ScalarType` concept
 * @tparam Allocator The allocator type (defaults to `std::allocator<T>`)
 *
 * @note This class provides the same exception safety guarantees as the
 *       underlying `Storage` class. Most operations are noexcept if the
 *       corresponding `Storage` operations are noexcept.
 * @note The type erasure incurs a small runtime overhead due to virtual
 *       function calls, but maintains full type safety through the DType
 *       system.
 *
 * @see StorageBase for the interface documentation
 * @see Storage for the underlying typed storage implementation
 * @see make_storage() for factory function creation
 * @see DType for supported data types
 *
 * @invariant The wrapped `Storage` object is always in a valid state
 * @invariant `element_size()` always returns `sizeof(T)`
 * @invariant `dtype()` always returns `dtype_v<T>`
 */
template <ScalarType T, typename Allocator = std::allocator<T>>
class TypeErasedStorage : public StorageBase {
  public:
    using storage_type = Storage<T, Allocator>;

  public:
    /**
     * @brief Constructs a TypeErasedStorage with forwarded arguments.
     *
     * Forwards all arguments to the underlying `Storage` constructor.
     * This allows construction with the same flexibility as the base `Storage`
     * class.
     *
     * @tparam Args Argument types to forward to Storage constructor
     * @param args Arguments to forward to Storage constructor
     *
     * @exception May throw any exception thrown by the underlying Storage
     *            constructor
     * @see Storage for constructor exception guarantees
     */
    template <typename... Args>
    explicit TypeErasedStorage(Args &&...args) : m_storage(std::forward<Args>(args)...) {}

    /**
     * @brief Returns a type-erased pointer to the underlying data.
     *
     * @return void* Pointer to the data buffer, or nullptr if empty
     */
    void *data() noexcept override { return m_storage.data(); }

    /**
     * @brief Returns a const type-erased pointer to the underlying data.
     *
     * @return const void* Const pointer to the data buffer, or nullptr if empty
     */
    const void *data() const noexcept override { return m_storage.data(); }

    /**
     * @brief Returns the number of elements in the storage.
     *
     * @return size_t Number of elements
     * @exception noexcept
     */
    size_t size() const noexcept override { return m_storage.size(); }

    /**
     * @brief Returns the size of each element in bytes.
     *
     * @return size_t Size of element type T in bytes (always `sizeof(T)`)
     * @exception noexcept
     */
    size_t element_size() const noexcept override { return sizeof(T); }

    /**
     * @brief Returns the data type of the stored elements.
     *
     * @return DType The data type enum value (always `dtype_v<T>`)
     * @exception noexcept
     */
    DType dtype() const noexcept override { return dtype_v<T> };

    /**
     * @brief Creates a deep copy of the storage.
     *
     * Creates a new `TypeErasedStorage` containing a copy of the underlying
     * data.
     *
     * @return std::unique_ptr<StorageBase> Unique pointer to a new copy
     * @exception std::bad_alloc if memory allocation fails
     * @exception Any exception thrown by element copying
     */
    std::unique_ptr<StorageBase> clone() const override { return std::make_unique<TypeErasedStorage>(m_storage); }

    /**
     * @brief Resizes the storage to contain new_size elements.
     *
     * Delegates to the underlying `Storage::resize()` method.
     *
     * @param new_size New number of elements
     * @exception std::bad_alloc if memory allocation fails
     * @exception Any exception thrown by element construction/destruction
     */
    void resize(size_t new_size) override { m_storage.resize(new_size); }

    /**
     * @brief Removes all elements from the storage.
     *
     * Delegates to the underlying `Storage::clear()` method.
     *
     * @exception noexcept if element destruction is noexcept
     */
    void clear() noexcept override { m_storage.clear(); }

    /**
     * @brief Provides access to the underlying typed storage.
     *
     * This method allows type-safe access to the concrete storage
     * implementation when the type is known at compile time.
     *
     * @return storage_type& Reference to the underlying typed storage
     */
    storage_type &get() noexcept { return m_storage; }

    /**
     * @brief Provides const access to the underlying typed storage.
     *
     * @return const storage_type& Const reference to the underlying typed
     *         storage
     */
    const storage_type &get() const noexcept { return m_storage; }

  private:
    storage_type m_storage;
};

/**
 * @brief Factory function for creating type-erased storage instances.
 *
 * `make_storage` creates a `std::unique_ptr<StorageBase>` that owns a
 * `TypeErasedStorage` object, providing a convenient way to create
 * runtime-polymorphic storage with type safety.
 *
 * This function enables:
 * - Creation of type-erased storage without explicit template instantiation
 * - Seamless integration with polymorphic interfaces expecting `StorageBase`
 * - Flexible construction with any arguments supported by the underlying `Storage`
 * - Exception-safe resource management through `std::unique_ptr`
 *
 * @tparam T The element type, must satisfy `ScalarType` concept
 * @tparam Allocator The allocator type (defaults to `std::allocator<T>`)
 * @tparam Args Argument types to forward to `TypeErasedStorage` constructor
 *
 * @param args Arguments to forward to the `TypeErasedStorage` constructor.
 *             These are typically the same arguments as for `Storage<T,
 *             Allocator>` construction, such as:
 *             - Size and optional value for initialization
 *             - Iterators or spans for range construction
 *             - Initializer lists
 *             - Allocator instances
 *
 * @return std::unique_ptr<StorageBase> Unique pointer to type-erased storage
 *         containing the created `TypeErasedStorage<T, Allocator>` object
 *
 * @exception May throw any exception thrown by:
 *            - Memory allocation (`std::bad_alloc`)
 *            - Element construction in the underlying `Storage`
 *            - Forwarding constructor arguments
 *
 * @note The returned pointer can be used polymorphically through the 
 *       `StorageBase` interface while maintaining type safety through the 
 *       `dtype()` method.
 * @note This function provides strong exception safety guarantee - if an 
 *       exception is thrown, no memory is leaked and the program state remains 
 *       unchanged.
 *
 * @see TypeErasedStorage
 * @see StorageBase
 * @see Storage for available constructor arguments
 *
 * @example
 * // Create storage with 10 default-initialized integers
 * auto storage = make_storage<int>(10);
 *
 * // Create storage from initializer list
 * auto storage = make_storage<float>({1.0f, 2.0f, 3.0f});
 *
 * // Create storage with custom allocator
 * auto storage = make_storage<double>(my_allocator);
 *
 * // Create storage with size and value
 * auto storage = make_storage<int>(5, 42); // 5 elements with value 42
 */
template <ScalarType T, typename Allocator = std::allocator<T>, typename... Args>
std::unique_ptr<StorageBase> make_storage(Args &&...args) {
    return std::make_unique<TypeErasedStorage<T, Allocator>>(std::forward<Args>(...));
}
}  // namespace brezel::core