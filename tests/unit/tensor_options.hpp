/**
 * @file tensor_options.hpp
 * @author Carlos Salguero
 * @brief Unit tests for the TensorOptions class
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <sstream>

#include <brezel/core/device.hpp>
#include <brezel/core/dtype.hpp>
#include <brezel/core/tensor_options.hpp>

#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("TensorOptions construction and defaults", "[tensor_options][unit]") {
    SECTION("Default construction") {
        TensorOptions options;

        // Check default values
        REQUIRE(options.dtype() == DType::Float32);
        REQUIRE(options.device() == Device::CPU);
        REQUIRE(options.layout() == Layout::Strided);
        REQUIRE(options.memory_format() == MemoryFormat::Contiguous);
        REQUIRE_FALSE(options.requires_grad());
        REQUIRE_FALSE(options.pinned_memory());
    }
}

TEST_CASE("TensorOptions property setters", "[tensor_options][unit]") {
    SECTION("dtype setter") {
        TensorOptions options;
        options.dtype(DType::Float64);
        REQUIRE(options.dtype() == DType::Float64);
    }

    SECTION("device setter") {
        TensorOptions options;
        options.device(Device(DeviceType::CUDA, 1));
        REQUIRE(options.device() == Device(DeviceType::CUDA, 1));
    }

    SECTION("layout setter") {
        TensorOptions options;
        options.layout(Layout::Sparse);
        REQUIRE(options.layout() == Layout::Sparse);
    }

    SECTION("memory_format setter") {
        TensorOptions options;
        options.memory_format(MemoryFormat::ChannelsLast);
        REQUIRE(options.memory_format() == MemoryFormat::ChannelsLast);
    }

    SECTION("requires_grad setter") {
        TensorOptions options;
        options.requires_grad(true);
        REQUIRE(options.requires_grad());

        options.requires_grad(false);
        REQUIRE_FALSE(options.requires_grad());
    }

    SECTION("pinned_memory setter") {
        TensorOptions options;
        options.pinned_memory(true);
        REQUIRE(options.pinned_memory());

        options.pinned_memory(false);
        REQUIRE_FALSE(options.pinned_memory());
    }

    SECTION("Method chaining") {
        TensorOptions options;
        options.dtype(DType::Int32)
            .device(Device::CUDA)
            .layout(Layout::COO)
            .memory_format(MemoryFormat::ChannelsLast)
            .requires_grad(true)
            .pinned_memory(true);

        REQUIRE(options.dtype() == DType::Int32);
        REQUIRE(options.device() == Device::CUDA);
        REQUIRE(options.layout() == Layout::COO);
        REQUIRE(options.memory_format() == MemoryFormat::ChannelsLast);
        REQUIRE(options.requires_grad());
        REQUIRE(options.pinned_memory());
    }
}

TEST_CASE("TensorOptions comparison operators", "[tensor_options][unit]") {
    SECTION("Equality") {
        TensorOptions options1;
        TensorOptions options2;

        // Default-constructed options should be equal
        REQUIRE(options1 == options2);

        // Modify one option and check inequality
        options2.dtype(DType::Float64);
        REQUIRE_FALSE(options1 == options2);
        REQUIRE(options1 != options2);

        // Make them equal again
        options1.dtype(DType::Float64);
        REQUIRE(options1 == options2);

        // Modify another option
        options2.requires_grad(true);
        REQUIRE_FALSE(options1 == options2);
        REQUIRE(options1 != options2);
    }

    SECTION("Inequality") {
        TensorOptions options1;
        TensorOptions options2;

        // Default-constructed options should not be unequal
        REQUIRE_FALSE(options1 != options2);

        // Modify one option and check inequality
        options2.layout(Layout::Sparse);
        REQUIRE(options1 != options2);
        REQUIRE_FALSE(options1 == options2);
    }
}

TEST_CASE("Layout and MemoryFormat string conversion", "[tensor_options][unit]") {
    SECTION("layout_to_string") {
        REQUIRE(layout_to_string(Layout::Strided) == "Strided");
        REQUIRE(layout_to_string(Layout::Sparse) == "Sparse");
        REQUIRE(layout_to_string(Layout::COO) == "COO");
        REQUIRE(layout_to_string(Layout::CSR) == "CSR");
        REQUIRE(layout_to_string(Layout::CSC) == "CSC");
        REQUIRE(layout_to_string(static_cast<Layout>(999)) == "Unknown");
    }

    SECTION("memory_format_to_string") {
        REQUIRE(memory_format_to_string(MemoryFormat::Contiguous) == "Contiguous");
        REQUIRE(memory_format_to_string(MemoryFormat::ChannelsLast) == "ChannelsLast");
        REQUIRE(memory_format_to_string(MemoryFormat::ChannelsLast3d) == "ChannelsLast3d");
        REQUIRE(memory_format_to_string(MemoryFormat::Preserve) == "Preserve");
        REQUIRE(memory_format_to_string(static_cast<MemoryFormat>(999)) == "Unknown");
    }
}