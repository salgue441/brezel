/**
 * @file storage_test.cpp
 * @author Carlos Salguero
 * @brief
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <algorithm>
#include <vector>

#include <brezel/core/device.hpp>
#include <brezel/core/dtype.hpp>
#include <brezel/core/storage.hpp>

#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("Storage construction and basic properties", "[storage][unit]") {
    SECTION("Default construction") {
        Storage empty;
        REQUIRE(empty.data() == nullptr);
        REQUIRE(empty.size_bytes() == 0);
        REQUIRE(empty.size_elements() == 0);
        REQUIRE(empty.dtype() == DType::Undefined);
        REQUIRE(empty.device() == Device::CPU);
        REQUIRE(empty.owns_data());
    }

    SECTION("Size and type construction") {
        const size_t num_bytes = 1000;
        Storage storage(num_bytes, DType::Float32, Device::CPU);

        REQUIRE(storage.data() != nullptr);
        REQUIRE(storage.size_bytes() == num_bytes);
        REQUIRE(storage.size_elements() == num_bytes / sizeof(float));
        REQUIRE(storage.dtype() == DType::Float32);
        REQUIRE(storage.device() == Device::CPU);
        REQUIRE(storage.owns_data());
    }

    SECTION("Zero-size construction") {
        Storage zero_storage(0, DType::Float32, Device::CPU);

        REQUIRE(zero_storage.data() == nullptr);
        REQUIRE(zero_storage.size_bytes() == 0);
        REQUIRE(zero_storage.size_elements() == 0);
    }

    SECTION("Construction with non-CPU device") {
#ifdef BREZEL_USE_CUDA
        // Tests for CUDA storage would go here
#else
        // Without CUDA support, constructing with CUDA device should throw
        REQUIRE_THROWS_AS(Storage(1000, DType::Float32, Device(DeviceType::CUDA)),
                          std::runtime_error);
#endif
    }
}

TEST_CASE("Storage data management", "[storage][unit]") {
    SECTION("from_cpu_data static method") {
        // Create test data
        std::vector<float> test_data = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f};

        // Create storage from data
        auto storage = Storage::from_cpu_data(test_data.data(), test_data.size());

        // Check properties
        REQUIRE(storage->dtype() == DType::Float32);
        REQUIRE(storage->device() == Device::CPU);
        REQUIRE(storage->size_bytes() == test_data.size() * sizeof(float));
        REQUIRE(storage->size_elements() == test_data.size());

        // Check data was copied correctly
        const float* storage_data = static_cast<const float*>(storage->data());
        for (size_t i = 0; i < test_data.size(); ++i) {
            REQUIRE(storage_data[i] == test_data[i]);
        }
    }

    SECTION("from_cpu_data with null pointer") {
        // Null pointer with zero count should be valid
        auto storage = Storage::from_cpu_data<float>(nullptr, 0);
        REQUIRE(storage->size_bytes() == 0);

        // Null pointer with non-zero count should throw
        REQUIRE_THROWS_AS(Storage::from_cpu_data<float>(nullptr, 5), std::invalid_argument);
    }

    SECTION("clone method") {
        // Create original storage with test data
        std::vector<int> test_data = {10, 20, 30, 40, 50};
        auto original = Storage::from_cpu_data(test_data.data(), test_data.size());

        // Clone the storage
        auto clone = original->clone();

        // Check properties are the same
        REQUIRE(clone->dtype() == original->dtype());
        REQUIRE(clone->device() == original->device());
        REQUIRE(clone->size_bytes() == original->size_bytes());
        REQUIRE(clone->size_elements() == original->size_elements());

        // Check data was copied correctly
        const int* original_data = static_cast<const int*>(original->data());
        const int* clone_data = static_cast<const int*>(clone->data());
        for (size_t i = 0; i < test_data.size(); ++i) {
            REQUIRE(clone_data[i] == original_data[i]);
        }

        // Verify data is separate (modifying original doesn't affect clone)
        int* mutable_original = const_cast<int*>(original_data);
        mutable_original[0] = 99;
        REQUIRE(clone_data[0] == 10);  // Still the original value
    }

    SECTION("view method") {
        // Create original storage with test data
        std::vector<char> test_data = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'};
        auto original = Storage::from_cpu_data(test_data.data(), test_data.size());

        // Create a view of a subset of the storage
        size_t offset = 2;  // Start at 'c'
        size_t size = 4;    // Take 4 bytes: 'c', 'd', 'e', 'f'
        auto view = original->view(offset, size);

        // Check properties
        REQUIRE(view->dtype() == original->dtype());
        REQUIRE(view->device() == original->device());
        REQUIRE(view->size_bytes() == size);
        REQUIRE_FALSE(view->owns_data());  // View doesn't own the data

        // Check data pointer is offset correctly
        const char* original_data = static_cast<const char*>(original->data());
        const char* view_data = static_cast<const char*>(view->data());
        REQUIRE(view_data == original_data + offset);

        // Check data access
        for (size_t i = 0; i < size; ++i) {
            REQUIRE(view_data[i] == test_data[i + offset]);
        }

        // Test invalid view (out of range)
        REQUIRE_THROWS_AS(original->view(4, 5), std::out_of_range);  // Would go beyond the end
    }

    SECTION("copy_from method") {
        // Create source storage with test data
        std::vector<double> source_data = {1.1, 2.2, 3.3, 4.4, 5.5};
        auto source = Storage::from_cpu_data(source_data.data(), source_data.size());

        // Create destination storage with the same size
        auto dest = std::make_shared<Storage>(source->size_bytes(), DType::Float64, Device::CPU);

        // Copy data from source to destination
        dest->copy_from(*source);

        // Check data was copied correctly
        const double* source_ptr = static_cast<const double*>(source->data());
        const double* dest_ptr = static_cast<const double*>(dest->data());
        for (size_t i = 0; i < source_data.size(); ++i) {
            REQUIRE(dest_ptr[i] == source_ptr[i]);
        }

        // Test with different sizes
        auto smaller_dest =
            std::make_shared<Storage>(source->size_bytes() - 8, DType::Float64, Device::CPU);
        REQUIRE_THROWS_AS(smaller_dest->copy_from(*source), std::invalid_argument);
    }
}

TEST_CASE("Storage device and type conversion", "[storage][unit]") {
    SECTION("to() device conversion") {
        // Create storage on CPU
        std::vector<int> test_data = {1, 2, 3, 4, 5};
        auto cpu_storage = Storage::from_cpu_data(test_data.data(), test_data.size());

        // Convert to the same device (should return the same storage)
        auto same_device = cpu_storage->to(Device::CPU);
        REQUIRE(same_device.get() == cpu_storage.get());

#ifdef BREZEL_USE_CUDA
        // Tests for conversion to CUDA would go here
#else
        // Without CUDA support, conversion to CUDA should throw
        REQUIRE_THROWS_AS(cpu_storage->to(Device(DeviceType::CUDA)), std::runtime_error);
#endif
    }

    SECTION("to() dtype conversion") {
        // Create storage with int data
        std::vector<int> test_data = {1, 2, 3, 4, 5};
        auto int_storage = Storage::from_cpu_data(test_data.data(), test_data.size());

        // Convert to the same type (should return the same storage)
        auto same_type = int_storage->to(DType::Int32);
        REQUIRE(same_type.get() == int_storage.get());

        // Conversion to other types is not implemented yet
        REQUIRE_THROWS_AS(int_storage->to(DType::Float32), std::runtime_error);
    }
}

TEST_CASE("Storage move semantics", "[storage][unit]") {
    SECTION("Move construction") {
        // Create original storage
        Storage original(1000, DType::Float32, Device::CPU);
        void* original_data = original.data();
        size_t original_size = original.size_bytes();

        // Move construct a new storage
        Storage moved(std::move(original));

        // Check that data was moved
        REQUIRE(moved.data() == original_data);
        REQUIRE(moved.size_bytes() == original_size);
        REQUIRE(moved.dtype() == DType::Float32);
        REQUIRE(moved.device() == Device::CPU);

        // Check that original was emptied
        REQUIRE(original.data() == nullptr);
        REQUIRE(original.size_bytes() == 0);
    }

    SECTION("Move assignment") {
        // Create original storage
        Storage original(1000, DType::Float32, Device::CPU);
        void* original_data = original.data();
        size_t original_size = original.size_bytes();

        // Create another storage to move into
        Storage target(500, DType::Int32, Device::CPU);

        // Move assign
        target = std::move(original);

        // Check that data was moved
        REQUIRE(target.data() == original_data);
        REQUIRE(target.size_bytes() == original_size);
        REQUIRE(target.dtype() == DType::Float32);
        REQUIRE(target.device() == Device::CPU);

        // Check that original was emptied
        REQUIRE(original.data() == nullptr);
        REQUIRE(original.size_bytes() == 0);
    }

    // Note: Copy construction and assignment are deleted, so not tested
}