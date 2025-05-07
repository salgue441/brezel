/**
 * @file shape_test.cpp
 * @author Carlos Salguero
 * @brief Unit tests for the Shape class
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <sstream>

#include <brezel/core/shape.hpp>

#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("Shape construction and basic operations", "[shape][unit]") {
    SECTION("Default construction") {
        Shape empty;

        REQUIRE(empty.empty());
        REQUIRE(empty.size() == 0);
        REQUIRE(empty.numel() == 0);
    }

    SECTION("Initializar list construction") {
        Shape shape1{2, 3, 4};

        REQUIRE(shape1.size() == 3);
        REQUIRE(shape1.numel() == 24);
        REQUIRE(shape1[0] == 2);
        REQUIRE(shape1[1] == 3);
        REQUIRE(shape1[2] == 4);
    }

    SECTION("Vector construction") {
        std::vector<Shape::SizeType> dims{5, 6, 7};
        Shape shape2(dims);

        REQUIRE(shape2.size() == 3);
        REQUIRE(shape2.numel() == 210);
        REQUIRE(shape2[0] == 5);
        REQUIRE(shape2[1] == 6);
        REQUIRE(shape2[2] == 7);
    }

    SECTION("Iterator construction") {
        std::vector<Shape::SizeType> dims2{8, 9};
        Shape shape3(dims2.begin(), dims2.end());

        REQUIRE(shape3.size() == 2);
        REQUIRE(shape3.numel() == 72);
        REQUIRE(shape3[0] == 8);
        REQUIRE(shape3[1] == 9);
    }

    SECTION("Validation for negative dimensions") {
        REQUIRE_THROWS_AS(Shape({-1, 2, 3}), std::invalid_argument);
    }
}

TEST_CASE("Shape comparison operators", "[shape][unit]") {
    Shape shape1{2, 3, 4};
    Shape shape2{2, 3, 4};
    Shape shape3{4, 3, 2};

    SECTION("Equality") {
        REQUIRE(shape1 == shape2);
        REQUIRE_FALSE(shape1 == shape3);
    }

    SECTION("Inequality") {
        REQUIRE_FALSE(shape1 != shape2);
        REQUIRE(shape1 != shape3);
    }
}

TEST_CASE("Shape indexing", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Element access") {
        REQUIRE(shape[0] == 2);
        REQUIRE(shape[1] == 3);
        REQUIRE(shape[2] == 4);
    }

    SECTION("Out of range access") {
        REQUIRE_THROWS_AS(shape[3], std::out_of_range);
    }

    SECTION("Modification through indexing") {
        shape[1] = 5;
        REQUIRE(shape[1] == 5);
        REQUIRE(shape.numel() == 40);  // 2 * 5 * 4
    }
}

TEST_CASE("Shape iterators", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Range-based for loop") {
        std::vector<Shape::SizeType> dims;
        for (auto dim : shape) {
            dims.push_back(dim);
        }

        REQUIRE(dims.size() == 3);
        REQUIRE(dims[0] == 2);
        REQUIRE(dims[1] == 3);
        REQUIRE(dims[2] == 4);
    }

    SECTION("begin/end iterators") {
        auto it = shape.begin();
        REQUIRE(*it == 2);
        ++it;
        REQUIRE(*it == 3);
        ++it;
        REQUIRE(*it == 4);
        ++it;
        REQUIRE(it == shape.end());
    }

    SECTION("cbegin/cend iterators") {
        auto it = shape.cbegin();
        REQUIRE(*it == 2);
        ++it;
        REQUIRE(*it == 3);
        ++it;
        REQUIRE(*it == 4);
        ++it;
        REQUIRE(it == shape.cend());
    }
}

TEST_CASE("Shape reshape operation", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Valid reshape") {
        Shape reshaped = shape.reshape({4, 6});
        REQUIRE(reshaped.size() == 2);
        REQUIRE(reshaped[0] == 4);
        REQUIRE(reshaped[1] == 6);
        REQUIRE(reshaped.numel() == 24);
    }

    SECTION("Invalid reshape (different number of elements)") {
        REQUIRE_THROWS_AS(shape.reshape({5, 5}), std::invalid_argument);
    }
}

TEST_CASE("Shape strides calculation", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Row-major (C-style) strides") {
        auto strides = shape.strides();
        REQUIRE(strides.size() == 3);
        REQUIRE(strides[0] == 12);
        REQUIRE(strides[1] == 4);
        REQUIRE(strides[2] == 1);
    }

    SECTION("Column-major (Fortran-style) strides") {
        auto strides_f = shape.strides(false);
        REQUIRE(strides_f.size() == 3);
        REQUIRE(strides_f[0] == 1);
        REQUIRE(strides_f[1] == 2);
        REQUIRE(strides_f[2] == 6);
    }

    SECTION("Empty shape strides") {
        Shape empty;
        auto strides = empty.strides();
        REQUIRE(strides.empty());
    }
}

TEST_CASE("Shape broadcasting", "[shape][unit]") {
    Shape shape1{2, 3, 4};
    Shape shape2{3, 4};
    Shape shape3{1, 3, 4};
    Shape shape4{2, 1, 4};
    Shape shape5{5, 3, 4};

    SECTION("Broadcastable test") {
        REQUIRE(shape1.is_broadcastable_with(shape2));
        REQUIRE(shape1.is_broadcastable_with(shape3));
        REQUIRE(shape1.is_broadcastable_with(shape4));
        REQUIRE_FALSE(shape1.is_broadcastable_with(shape5));
    }

    SECTION("Broadcasting result") {
        Shape broadcast1 = shape1.broadcast_with(shape2);
        REQUIRE(broadcast1 == Shape({2, 3, 4}));

        Shape broadcast2 = shape1.broadcast_with(shape3);
        REQUIRE(broadcast2 == Shape({2, 3, 4}));

        Shape broadcast3 = shape1.broadcast_with(shape4);
        REQUIRE(broadcast3 == Shape({2, 3, 4}));

        REQUIRE_THROWS_AS(shape1.broadcast_with(shape5), std::invalid_argument);
    }
}

TEST_CASE("Shape slicing", "[shape][unit]") {
    Shape shape{2, 3, 4, 5};

    SECTION("Slice operation") {
        Shape slice1 = shape.slice(1, 3);
        REQUIRE(slice1 == Shape({3, 4}));
    }

    SECTION("Invalid slicing") {
        REQUIRE_THROWS_AS(shape.slice(3, 5), std::out_of_range);
        REQUIRE_THROWS_AS(shape.slice(2, 1), std::out_of_range);
    }
}

TEST_CASE("Shape unsqueeze and squeeze", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Unsqueeze operation") {
        Shape unsqueezed1 = shape.unsqueeze(0);
        REQUIRE(unsqueezed1 == Shape({1, 2, 3, 4}));

        Shape unsqueezed2 = shape.unsqueeze(2);
        REQUIRE(unsqueezed2 == Shape({2, 3, 1, 4}));

        Shape unsqueezed3 = shape.unsqueeze(3);
        REQUIRE(unsqueezed3 == Shape({2, 3, 4, 1}));
    }

    SECTION("Squeeze operation") {
        Shape shape_with_ones{2, 1, 3, 1, 4};
        Shape squeezed1 = shape_with_ones.squeeze(1);
        REQUIRE(squeezed1 == Shape({2, 3, 1, 4}));

        Shape squeezed2 = squeezed1.squeeze(2);
        REQUIRE(squeezed2 == Shape({2, 3, 4}));
    }

    SECTION("Squeeze all operation") {
        Shape shape_with_ones{2, 1, 3, 1, 4};
        Shape all_squeezed = shape_with_ones.squeeze_all();
        REQUIRE(all_squeezed == Shape({2, 3, 4}));

        Shape all_ones{1, 1, 1};
        Shape squeezed_ones = all_ones.squeeze_all();
        REQUIRE(squeezed_ones == Shape({1}));
    }

    SECTION("Invalid squeeze") {
        REQUIRE_THROWS_AS(shape.squeeze(0), std::invalid_argument);
    }
}

TEST_CASE("Shape permutation", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Permute operation") {
        Shape permuted = shape.permute({2, 0, 1});
        REQUIRE(permuted == Shape({4, 2, 3}));
    }

    SECTION("Invalid permutation") {
        REQUIRE_THROWS_AS(shape.permute({0, 1}), std::invalid_argument);     // Wrong size
        REQUIRE_THROWS_AS(shape.permute({0, 1, 3}), std::out_of_range);      // Index out of range
        REQUIRE_THROWS_AS(shape.permute({0, 0, 1}), std::invalid_argument);  // Duplicate indices
    }
}

TEST_CASE("Shape transpose", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("Transpose operation") {
        Shape transposed = shape.transpose();
        REQUIRE(transposed == Shape({2, 4, 3}));
    }

    SECTION("Invalid transpose") {
        Shape shape1{2};
        REQUIRE_THROWS_AS(shape1.transpose(), std::invalid_argument);
    }
}

TEST_CASE("Shape string conversion", "[shape][unit]") {
    SECTION("to_string method") {
        Shape empty;
        REQUIRE(empty.to_string() == "[]");

        Shape shape1{2, 3, 4};
        REQUIRE(shape1.to_string() == "[2, 3, 4]");
    }

    SECTION("stream operator") {
        std::ostringstream oss;
        Shape shape{5, 6, 7};
        oss << shape;
        REQUIRE(oss.str() == "[5, 6, 7]");
    }
}

TEST_CASE("Shape accessors", "[shape][unit]") {
    Shape shape{2, 3, 4};

    SECTION("dims() accessor") {
        const auto& dims = shape.dims();
        REQUIRE(dims.size() == 3);
        REQUIRE(dims[0] == 2);
        REQUIRE(dims[1] == 3);
        REQUIRE(dims[2] == 4);
    }
}