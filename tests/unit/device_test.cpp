/**
 * @file device_test.cpp
 * @author Carlos Salguero
 * @brief Unit tests for the Device class
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <sstream>

#include <brezel/core/device.hpp>

#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("Device construction", "[device][unit]") {
    SECTION("Default construction") {
        Device default_device;
        REQUIRE(default_device.type() == DeviceType::CPU);
        REQUIRE(default_device.index() == 0);
        REQUIRE(default_device.is_cpu());
        REQUIRE_FALSE(default_device.is_cuda());
    }

    SECTION("Type and index construction") {
        Device cuda_device(DeviceType::CUDA, 1);
        REQUIRE(cuda_device.type() == DeviceType::CUDA);
        REQUIRE(cuda_device.index() == 1);
        REQUIRE_FALSE(cuda_device.is_cpu());
        REQUIRE(cuda_device.is_cuda());

        Device opencl_device(DeviceType::OpenCL, 2);
        REQUIRE(opencl_device.type() == DeviceType::OpenCL);
        REQUIRE(opencl_device.index() == 2);
        REQUIRE_FALSE(opencl_device.is_cpu());
        REQUIRE_FALSE(opencl_device.is_cuda());
    }

    SECTION("String construction") {
        Device device_from_string1("cpu");
        REQUIRE(device_from_string1.type() == DeviceType::CPU);
        REQUIRE(device_from_string1.index() == 0);

        Device device_from_string2("cuda:0");
        REQUIRE(device_from_string2.type() == DeviceType::CUDA);
        REQUIRE(device_from_string2.index() == 0);

        Device device_from_string3("cuda:2");
        REQUIRE(device_from_string3.type() == DeviceType::CUDA);
        REQUIRE(device_from_string3.index() == 2);

        Device device_from_string4("opencl:1");
        REQUIRE(device_from_string4.type() == DeviceType::OpenCL);
        REQUIRE(device_from_string4.index() == 1);

        Device device_from_string5("vulkan:3");
        REQUIRE(device_from_string5.type() == DeviceType::Vulkan);
        REQUIRE(device_from_string5.index() == 3);

        Device device_from_string6("metal:0");
        REQUIRE(device_from_string6.type() == DeviceType::Metal);
        REQUIRE(device_from_string6.index() == 0);

        Device device_from_string7("tpu:2");
        REQUIRE(device_from_string7.type() == DeviceType::TPU);
        REQUIRE(device_from_string7.index() == 2);
    }

    SECTION("Invalid string construction") {
        REQUIRE_THROWS_AS(Device("invalid"), std::invalid_argument);
        REQUIRE_THROWS_AS(Device("cuda:"), std::invalid_argument);
        REQUIRE_THROWS_AS(Device("cuda:abc"), std::invalid_argument);
        REQUIRE_THROWS_AS(Device("opencl:"), std::invalid_argument);
        REQUIRE_THROWS_AS(Device("opencl:xyz"), std::invalid_argument);
    }

    SECTION("Invalid index") {
        REQUIRE_THROWS_AS(Device(DeviceType::CUDA, -1), std::invalid_argument);
        REQUIRE_THROWS_AS(Device(DeviceType::OpenCL, -5), std::invalid_argument);
    }

    SECTION("CPU with non-zero index") {
        REQUIRE_THROWS_AS(Device(DeviceType::CPU, 1), std::invalid_argument);
    }
}

TEST_CASE("Device static constants", "[device][unit]") {
    SECTION("CPU device") {
        REQUIRE(Device::CPU.type() == DeviceType::CPU);
        REQUIRE(Device::CPU.index() == 0);
    }

    SECTION("CUDA devices") {
        REQUIRE(Device::CUDA.type() == DeviceType::CUDA);
        REQUIRE(Device::CUDA.index() == 0);

        REQUIRE(Device::CUDA1.type() == DeviceType::CUDA);
        REQUIRE(Device::CUDA1.index() == 1);

        REQUIRE(Device::CUDA2.type() == DeviceType::CUDA);
        REQUIRE(Device::CUDA2.index() == 2);
    }
}

TEST_CASE("Device string representation", "[device][unit]") {
    SECTION("to_string method") {
        REQUIRE(Device(DeviceType::CPU).to_string() == "CPU");
        REQUIRE(Device(DeviceType::CUDA, 0).to_string() == "CUDA:0");
        REQUIRE(Device(DeviceType::CUDA, 1).to_string() == "CUDA:1");
        REQUIRE(Device(DeviceType::OpenCL, 2).to_string() == "OpenCL:2");
        REQUIRE(Device(DeviceType::Vulkan, 3).to_string() == "Vulkan:3");
        REQUIRE(Device(DeviceType::Metal, 4).to_string() == "Metal:4");
        REQUIRE(Device(DeviceType::TPU, 5).to_string() == "TPU:5");
        REQUIRE(Device(DeviceType::Custom, 6).to_string() == "Custom:6");
    }

    SECTION("device_type_to_string function") {
        REQUIRE(device_type_to_string(DeviceType::CPU) == "CPU");
        REQUIRE(device_type_to_string(DeviceType::CUDA) == "CUDA");
        REQUIRE(device_type_to_string(DeviceType::OpenCL) == "OpenCL");
        REQUIRE(device_type_to_string(DeviceType::Vulkan) == "Vulkan");
        REQUIRE(device_type_to_string(DeviceType::Metal) == "Metal");
        REQUIRE(device_type_to_string(DeviceType::TPU) == "TPU");
        REQUIRE(device_type_to_string(DeviceType::Custom) == "Custom");
        REQUIRE(device_type_to_string(static_cast<DeviceType>(999)) == "Unknown");
    }

    SECTION("stream operator") {
        std::ostringstream oss;

        oss << Device(DeviceType::CPU);
        REQUIRE(oss.str() == "CPU");

        oss.str("");
        oss << Device(DeviceType::CUDA, 2);
        REQUIRE(oss.str() == "CUDA:2");
    }
}

TEST_CASE("Device comparison", "[device][unit]") {
    SECTION("Equality") {
        REQUIRE(Device(DeviceType::CPU) == Device(DeviceType::CPU));
        REQUIRE(Device(DeviceType::CUDA, 1) == Device(DeviceType::CUDA, 1));
        REQUIRE_FALSE(Device(DeviceType::CPU) == Device(DeviceType::CUDA));
        REQUIRE_FALSE(Device(DeviceType::CUDA, 0) == Device(DeviceType::CUDA, 1));
    }

    SECTION("Inequality") {
        REQUIRE(Device(DeviceType::CPU) != Device(DeviceType::CUDA));
        REQUIRE(Device(DeviceType::CUDA, 0) != Device(DeviceType::CUDA, 1));
        REQUIRE_FALSE(Device(DeviceType::CPU) != Device(DeviceType::CPU));
        REQUIRE_FALSE(Device(DeviceType::CUDA, 1) != Device(DeviceType::CUDA, 1));
    }
}

TEST_CASE("Device availability", "[device][unit]") {
    SECTION("CPU availability") {
        // CPU should always be available
        REQUIRE(Device(DeviceType::CPU).is_available());
    }

    SECTION("CUDA availability") {
#ifdef BREZEL_USE_CUDA
        // If CUDA is enabled, we would need more sophisticated testing
        // This is a placeholder for when CUDA support is implemented
#else
        // If CUDA is not enabled, CUDA devices should not be available
        REQUIRE_FALSE(Device(DeviceType::CUDA).is_available());
#endif
    }

    SECTION("Other device types") {
        REQUIRE_THROWS_AS(Device(DeviceType::OpenCL).is_available(), std::invalid_argument);
        REQUIRE_THROWS_AS(Device(DeviceType::Vulkan).is_available(), std::invalid_argument);
        REQUIRE_THROWS_AS(Device(DeviceType::Metal).is_available(), std::invalid_argument);
        REQUIRE_THROWS_AS(Device(DeviceType::TPU).is_available(), std::invalid_argument);
    }
}

TEST_CASE("Device utility functions", "[device][unit]") {
    SECTION("get_current_device") {
        Device current = get_current_device();
        REQUIRE(current.type() == DeviceType::CPU);
    }

    SECTION("set_current_device") {
        REQUIRE_NOTHROW(set_current_device(Device::CPU));

        Device current = get_current_device();
        REQUIRE(current == Device::CPU);
    }

    SECTION("device_count") {
        REQUIRE(device_count(DeviceType::CPU) >= 1);

        // CUDA count depends on hardware and build
#ifdef BREZEL_USE_CUDA
        // Can't make assumptions about hardware
#else
        // If CUDA is disabled in build, count should be 0
        REQUIRE(device_count(DeviceType::CUDA) == 0);
#endif
    }

    SECTION("synchronize") {
        REQUIRE_NOTHROW(synchronize(Device::CPU));

        // For other devices, behavior depends on implementation
    }
}