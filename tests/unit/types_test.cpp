#include <complex>
#include <sstream>

#include <brezel/core/types.hpp>

#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("Data type utilities", "[types][unit]") {
    SECTION("dtype_to_string") {
        REQUIRE(dtype_to_string(dtype_t::Float32) == "Float32");
        REQUIRE(dtype_to_string(dtype_t::Float64) == "Float64");
        REQUIRE(dtype_to_string(dtype_t::Int32) == "Int32");
        REQUIRE(dtype_to_string(dtype_t::Int64) == "Int64");
        REQUIRE(dtype_to_string(dtype_t::Uint8) == "Uint8");
        REQUIRE(dtype_to_string(dtype_t::Bool) == "Bool");
        REQUIRE(dtype_to_string(dtype_t::Complex64) == "Complex64");
    }

    SECTION("dtype_size") {
        REQUIRE(dtype_size(dtype_t::Float32) == sizeof(float));
        REQUIRE(dtype_size(dtype_t::Float64) == sizeof(double));
        REQUIRE(dtype_size(dtype_t::Int32) == sizeof(int32_t));
        REQUIRE(dtype_size(dtype_t::Int64) == sizeof(int64_t));
        REQUIRE(dtype_size(dtype_t::Uint8) == sizeof(uint8_t));
        REQUIRE(dtype_size(dtype_t::Bool) == sizeof(bool));
        REQUIRE(dtype_size(dtype_t::Complex64) == sizeof(std::complex<float>));
    }

    SECTION("dtype_type_name") {
        REQUIRE(dtype_to_type_name(dtype_t::Float32) == "float");
        REQUIRE(dtype_to_type_name(dtype_t::Float64) == "double");
        REQUIRE(dtype_to_type_name(dtype_t::Int32) == "int32_t");
        REQUIRE(dtype_to_type_name(dtype_t::Int64) == "int64_t");
        REQUIRE(dtype_to_type_name(dtype_t::Uint8) == "uint8_t");
        REQUIRE(dtype_to_type_name(dtype_t::Bool) == "bool");
        REQUIRE(dtype_to_type_name(dtype_t::Complex64) == "std::complex<float>");
    }

    SECTION("device_to_string") {
        REQUIRE(device_type_to_string(DeviceType::CPU) == "CPU");
        REQUIRE(device_type_to_string(DeviceType::CUDA) == "CUDA");
    }
}

TEST_CASE("Shape and stride operations", "[types][unit]") {
    SECTION("shapes_equal") {
        REQUIRE(shapes_equal({2, 3, 4}, {2, 3, 4}));
        REQUIRE_FALSE(shapes_equal({2, 3, 4}, {2, 3, 5}));
        REQUIRE_FALSE(shapes_equal({2, 3, 4}, {2, 3}));
    }

    SECTION("strides_equal") {
        REQUIRE(strides_equal({12, 4, 1}, {12, 4, 1}));
        REQUIRE_FALSE(strides_equal({12, 4, 1}, {12, 4, 2}));
        REQUIRE_FALSE(strides_equal({12, 4, 1}, {12, 4}));
    }

    SECTION("calculate_size") {
        REQUIRE(calculate_size({}) == 0);
        REQUIRE(calculate_size({5}) == 5);
        REQUIRE(calculate_size({2, 3}) == 6);
        REQUIRE(calculate_size({2, 3, 4}) == 24);
    }

    SECTION("calculate_strides") {
        // Row-major (Default)
        REQUIRE(calculate_strides({2, 3, 4}) == stride_t({12, 4, 1}));
        REQUIRE(calculate_strides({5, 5}) == stride_t({5, 1}));
        REQUIRE(calculate_strides({10}) == stride_t({1}));
        REQUIRE(calculate_strides({}) == stride_t({}));

        // Column-major
        REQUIRE(calculate_strides({2, 3, 4}, MemoryLayout::ColumnMajor) == stride_t({1, 2, 6}));
        REQUIRE(calculate_strides({5, 5}, MemoryLayout::ColumnMajor) == stride_t({1, 5}));
    }

    SECTION("linear_index_to_indices and indices_to_linear_index") {
        shape_t shape = {2, 3, 4};
        stride_t strides = calculate_strides(shape);

        REQUIRE(linear_index_to_indices(0, shape, strides) == std::vector<index_t>({0, 0, 0}));
        REQUIRE(linear_index_to_indices(1, shape, strides) == std::vector<index_t>({0, 0, 1}));
        REQUIRE(linear_index_to_indices(4, shape, strides) == std::vector<index_t>({0, 1, 0}));
        REQUIRE(linear_index_to_indices(23, shape, strides) == std::vector<index_t>({1, 2, 3}));

        // Test round-trip conversion
        for (size_t i = 0; i < calculate_size(shape); ++i) {
            auto indices = linear_index_to_indices(i, shape, strides);
            REQUIRE(indices_to_linear_index(indices, strides) == i);
        }

        // Test with different strides (column-major)
        stride_t colStrides = calculate_strides(shape, MemoryLayout::ColumnMajor);
        std::vector<index_t> indices = {1, 2, 3};
        size_t linearIndex = indices_to_linear_index(indices, colStrides);
        REQUIRE(linear_index_to_indices(linearIndex, shape, colStrides) == indices);
    }

    SECTION("is_valid_shape") {
        REQUIRE(is_valid_shape({2, 3, 4}));
        REQUIRE(is_valid_shape({1}));
        REQUIRE_FALSE(is_valid_shape({0}));
        REQUIRE_FALSE(is_valid_shape({2, 0, 4}));
        REQUIRE_FALSE(is_valid_shape({-1, 3, 4}));
    }

    SECTION("is_contiguous") {
        shape_t shape = {2, 3, 4};
        stride_t rowMajorStrides = calculate_strides(shape);
        stride_t colMajorStrides = calculate_strides(shape, MemoryLayout::ColumnMajor);
        stride_t nonContiguousStrides = {24, 4, 2};  // Not contiguous in either layout

        REQUIRE(is_contiguous(shape, rowMajorStrides));
        REQUIRE_FALSE(is_contiguous(shape, colMajorStrides));
        REQUIRE_FALSE(is_contiguous(shape, nonContiguousStrides));

        // Empty shape/strides should be considered contiguous
        REQUIRE(is_contiguous({}, {}));
    }

    SECTION("make_shape") {
        REQUIRE(make_shape() == shape_t{});
        REQUIRE(make_shape({2, 3, 4}) == shape_t({2, 3, 4}));
        REQUIRE(make_shape(2, 3, 4) == shape_t({2, 3, 4}));
    }

    SECTION("stream operators") {
        std::stringstream ss;

        // Test shape output
        shape_t shape = {2, 3, 4};
        ss << shape;
        REQUIRE(ss.str() == "[2, 3, 4]");

        // Clear and test strides output
        ss.str("");
        ss.clear();

        stride_t strides = {12, 4, 1};
        ss << strides;
        REQUIRE(ss.str() == "[12, 4, 1]");
    }
}