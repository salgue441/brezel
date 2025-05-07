/**
 * @file dtype_test.cpp
 * @author Carlos Salguero
 * @brief Unit tests for the DType enum and related functions
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <complex>

#include <brezel/core/dtype.hpp>

#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("DType size calculation", "[dtype][unit]") {
    SECTION("Integral data types") {
        REQUIRE(dtype_size(DType::Bool) == sizeof(bool));
        REQUIRE(dtype_size(DType::UInt8) == sizeof(uint8_t));
        REQUIRE(dtype_size(DType::Int8) == sizeof(int8_t));
        REQUIRE(dtype_size(DType::UInt16) == sizeof(uint16_t));
        REQUIRE(dtype_size(DType::Int16) == sizeof(int16_t));
        REQUIRE(dtype_size(DType::UInt32) == sizeof(uint32_t));
        REQUIRE(dtype_size(DType::Int32) == sizeof(int32_t));
        REQUIRE(dtype_size(DType::UInt64) == sizeof(uint64_t));
        REQUIRE(dtype_size(DType::Int64) == sizeof(int64_t));
    }

    SECTION("Floating point data types") {
        REQUIRE(dtype_size(DType::Float16) == 2);
        REQUIRE(dtype_size(DType::Float32) == sizeof(float));
        REQUIRE(dtype_size(DType::Float64) == sizeof(double));
        REQUIRE(dtype_size(DType::BFloat16) == 2);
    }

    SECTION("Complex data types") {
        REQUIRE(dtype_size(DType::Complex32) == 4);
        REQUIRE(dtype_size(DType::Complex64) == sizeof(std::complex<float>));
        REQUIRE(dtype_size(DType::Complex128) == sizeof(std::complex<double>));
    }

    SECTION("Other data types") {
        REQUIRE(dtype_size(DType::String) == sizeof(void*));
    }

    SECTION("Invalid data types") {
        REQUIRE_THROWS_AS(dtype_size(DType::Undefined), std::runtime_error);
    }
}

TEST_CASE("DType type classification", "[dtype][unit]") {
    SECTION("Floating point check") {
        REQUIRE(is_floating_point(DType::Float16));
        REQUIRE(is_floating_point(DType::Float32));
        REQUIRE(is_floating_point(DType::Float64));
        REQUIRE(is_floating_point(DType::BFloat16));
        REQUIRE_FALSE(is_floating_point(DType::Int32));
        REQUIRE_FALSE(is_floating_point(DType::Complex64));
        REQUIRE_FALSE(is_floating_point(DType::Bool));
        REQUIRE_FALSE(is_floating_point(DType::String));
    }

    SECTION("Integral check") {
        REQUIRE(is_integral(DType::UInt8));
        REQUIRE(is_integral(DType::Int8));
        REQUIRE(is_integral(DType::UInt16));
        REQUIRE(is_integral(DType::Int16));
        REQUIRE(is_integral(DType::UInt32));
        REQUIRE(is_integral(DType::Int32));
        REQUIRE(is_integral(DType::UInt64));
        REQUIRE(is_integral(DType::Int64));
        REQUIRE_FALSE(is_integral(DType::Float32));
        REQUIRE_FALSE(is_integral(DType::Bool));
        REQUIRE_FALSE(is_integral(DType::Complex64));
        REQUIRE_FALSE(is_integral(DType::String));
    }

    SECTION("Complex check") {
        REQUIRE(is_complex(DType::Complex32));
        REQUIRE(is_complex(DType::Complex64));
        REQUIRE(is_complex(DType::Complex128));
        REQUIRE_FALSE(is_complex(DType::Float32));
        REQUIRE_FALSE(is_complex(DType::Int32));
        REQUIRE_FALSE(is_complex(DType::Bool));
        REQUIRE_FALSE(is_complex(DType::String));
    }
}

TEST_CASE("DType string conversion", "[dtype][unit]") {
    SECTION("Standard data types") {
        REQUIRE(dtype_to_string(DType::Bool) == "Bool");
        REQUIRE(dtype_to_string(DType::UInt8) == "UInt8");
        REQUIRE(dtype_to_string(DType::Int8) == "Int8");
        REQUIRE(dtype_to_string(DType::UInt16) == "UInt16");
        REQUIRE(dtype_to_string(DType::Int16) == "Int16");
        REQUIRE(dtype_to_string(DType::UInt32) == "UInt32");
        REQUIRE(dtype_to_string(DType::Int32) == "Int32");
        REQUIRE(dtype_to_string(DType::UInt64) == "UInt64");
        REQUIRE(dtype_to_string(DType::Int64) == "Int64");
        REQUIRE(dtype_to_string(DType::Float16) == "Float16");
        REQUIRE(dtype_to_string(DType::Float32) == "Float32");
        REQUIRE(dtype_to_string(DType::Float64) == "Float64");
        REQUIRE(dtype_to_string(DType::Complex32) == "Complex32");
        REQUIRE(dtype_to_string(DType::Complex64) == "Complex64");
        REQUIRE(dtype_to_string(DType::Complex128) == "Complex128");
        REQUIRE(dtype_to_string(DType::BFloat16) == "BFloat16");
        REQUIRE(dtype_to_string(DType::String) == "String");
    }

    SECTION("Special cases") {
        REQUIRE(dtype_to_string(DType::Undefined) == "Undefined");
        REQUIRE(dtype_to_string(static_cast<DType>(999)) == "Unknown");
    }
}

TEST_CASE("C++ type to DType mapping", "[dtype][unit]") {
    SECTION("Basic type mapping") {
        REQUIRE(get_dtype<bool>() == DType::Bool);
        REQUIRE(get_dtype<uint8_t>() == DType::UInt8);
        REQUIRE(get_dtype<int8_t>() == DType::Int8);
        REQUIRE(get_dtype<uint16_t>() == DType::UInt16);
        REQUIRE(get_dtype<int16_t>() == DType::Int16);
        REQUIRE(get_dtype<uint32_t>() == DType::UInt32);
        REQUIRE(get_dtype<int32_t>() == DType::Int32);
        REQUIRE(get_dtype<uint64_t>() == DType::UInt64);
        REQUIRE(get_dtype<int64_t>() == DType::Int64);
        REQUIRE(get_dtype<float>() == DType::Float32);
        REQUIRE(get_dtype<double>() == DType::Float64);
        REQUIRE(get_dtype<std::complex<float>>() == DType::Complex64);
        REQUIRE(get_dtype<std::complex<double>>() == DType::Complex128);
        REQUIRE(get_dtype<std::string>() == DType::String);
    }

    SECTION("Alias types") {
        REQUIRE(get_dtype<char>() == get_dtype<int8_t>() ||
                get_dtype<char>() == get_dtype<uint8_t>());
        REQUIRE(get_dtype<unsigned char>() == DType::UInt8);
        REQUIRE(get_dtype<short>() == get_dtype<int16_t>());
        REQUIRE(get_dtype<unsigned short>() == get_dtype<uint16_t>());
        REQUIRE(get_dtype<int>() == get_dtype<int32_t>());
        REQUIRE(get_dtype<unsigned int>() == get_dtype<uint32_t>());
        REQUIRE(get_dtype<long long>() == get_dtype<int64_t>());
        REQUIRE(get_dtype<unsigned long long>() == get_dtype<uint64_t>());
    }
}