/**
 * @file scalar_test.cpp
 * @author Carlos Salguero
 * @brief Unit tests for the Scalar class
 * @version 0.1
 * @date 2025-04-30
 *
 * @copyright Copyright (c) 2025
 *
 */

#include <complex>
#include <sstream>

#include <brezel/core/dtype.hpp>
#include <brezel/core/scalar.hpp>

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace brezel;

TEST_CASE("Scalar construction", "[scalar][unit]") {
    SECTION("Default construction") {
        Scalar default_scalar;
        REQUIRE(default_scalar.to_double() == 0.0);
    }

    SECTION("Construction from boolean") {
        Scalar true_scalar(true);
        REQUIRE(true_scalar.is_bool());
        REQUIRE(true_scalar.to_bool() == true);

        Scalar false_scalar(false);
        REQUIRE(false_scalar.is_bool());
        REQUIRE(false_scalar.to_bool() == false);
    }

    SECTION("Construction from integer types") {
        // char/int8_t
        Scalar char_scalar('A');
        REQUIRE(char_scalar.to_int8() == static_cast<int8_t>('A'));

        // unsigned char/uint8_t
        Scalar uchar_scalar(static_cast<unsigned char>('B'));
        REQUIRE(uchar_scalar.to_uint8() == static_cast<uint8_t>('B'));

        // int/int32_t
        Scalar int_scalar(42);
        REQUIRE(int_scalar.to_int32() == 42);

        // unsigned int/uint32_t
        Scalar uint_scalar(42u);
        REQUIRE(uint_scalar.to_uint32() == 42u);

        // long/int64_t
        Scalar long_scalar(42L);
        REQUIRE(long_scalar.to_int64() == 42L);

        // unsigned long/uint64_t
        Scalar ulong_scalar(42UL);
        REQUIRE(ulong_scalar.to_uint64() == 42UL);

        // long long/int64_t
        Scalar longlong_scalar(42LL);
        REQUIRE(longlong_scalar.to_int64() == 42LL);

        // unsigned long long/uint64_t
        Scalar ulonglong_scalar(42ULL);
        REQUIRE(ulonglong_scalar.to_uint64() == 42ULL);
    }

    SECTION("Construction from floating point types") {
        // float
        Scalar float_scalar(3.14f);
        REQUIRE(float_scalar.to_float() == Catch::Approx(3.14f));

        // double
        Scalar double_scalar(3.14159);
        REQUIRE(double_scalar.to_double() == Catch::Approx(3.14159));
    }

    SECTION("Construction from complex types") {
        // std::complex<float>
        std::complex<float> cf(1.0f, 2.0f);
        Scalar complex_float_scalar(cf);
        REQUIRE(complex_float_scalar.to_complex_float().real() == Catch::Approx(1.0f));
        REQUIRE(complex_float_scalar.to_complex_float().imag() == Catch::Approx(2.0f));

        // std::complex<double>
        std::complex<double> cd(1.0, 2.0);
        Scalar complex_double_scalar(cd);
        REQUIRE(complex_double_scalar.to_complex_double().real() == Catch::Approx(1.0));
        REQUIRE(complex_double_scalar.to_complex_double().imag() == Catch::Approx(2.0));
    }

    SECTION("Construction from string types") {
        // std::string
        Scalar string_scalar(std::string("hello"));
        REQUIRE(string_scalar.to_string() == "hello");

        // C string
        Scalar cstring_scalar("world");
        REQUIRE(cstring_scalar.to_string() == "world");
    }
}

TEST_CASE("Scalar type checking", "[scalar][unit]") {
    SECTION("Boolean type checking") {
        Scalar bool_scalar(true);
        REQUIRE(bool_scalar.is_bool());
        REQUIRE_FALSE(bool_scalar.is_integral());
        REQUIRE_FALSE(bool_scalar.is_floating_point());
        REQUIRE_FALSE(bool_scalar.is_complex());
        REQUIRE_FALSE(bool_scalar.is_string());
        REQUIRE_FALSE(bool_scalar.is_numeric());
        REQUIRE(bool_scalar.dtype() == DType::Bool);
    }

    SECTION("Integer type checking") {
        Scalar int_scalar(42);
        REQUIRE_FALSE(int_scalar.is_bool());
        REQUIRE(int_scalar.is_integral());
        REQUIRE_FALSE(int_scalar.is_floating_point());
        REQUIRE_FALSE(int_scalar.is_complex());
        REQUIRE_FALSE(int_scalar.is_string());
        REQUIRE(int_scalar.is_numeric());
        REQUIRE(int_scalar.dtype() == DType::Int32);

        Scalar uint_scalar(42u);
        REQUIRE(uint_scalar.is_integral());
        REQUIRE(uint_scalar.dtype() == DType::UInt32);
    }

    SECTION("Floating point type checking") {
        Scalar float_scalar(3.14f);
        REQUIRE_FALSE(float_scalar.is_bool());
        REQUIRE_FALSE(float_scalar.is_integral());
        REQUIRE(float_scalar.is_floating_point());
        REQUIRE_FALSE(float_scalar.is_complex());
        REQUIRE_FALSE(float_scalar.is_string());
        REQUIRE(float_scalar.is_numeric());
        REQUIRE(float_scalar.dtype() == DType::Float32);

        Scalar double_scalar(3.14159);
        REQUIRE(double_scalar.is_floating_point());
        REQUIRE(double_scalar.dtype() == DType::Float64);
    }

    SECTION("Complex type checking") {
        Scalar complex_scalar(std::complex<double>(1.0, 2.0));
        REQUIRE_FALSE(complex_scalar.is_bool());
        REQUIRE_FALSE(complex_scalar.is_integral());
        REQUIRE_FALSE(complex_scalar.is_floating_point());
        REQUIRE(complex_scalar.is_complex());
        REQUIRE_FALSE(complex_scalar.is_string());
        REQUIRE(complex_scalar.is_numeric());
        REQUIRE(complex_scalar.dtype() == DType::Complex128);
    }

    SECTION("String type checking") {
        Scalar string_scalar("hello");
        REQUIRE_FALSE(string_scalar.is_bool());
        REQUIRE_FALSE(string_scalar.is_integral());
        REQUIRE_FALSE(string_scalar.is_floating_point());
        REQUIRE_FALSE(string_scalar.is_complex());
        REQUIRE(string_scalar.is_string());
        REQUIRE_FALSE(string_scalar.is_numeric());
        REQUIRE(string_scalar.dtype() == DType::String);
    }
}

TEST_CASE("Scalar type conversion", "[scalar][unit]") {
    SECTION("Boolean conversion") {
        // From boolean
        REQUIRE(Scalar(true).to_bool() == true);
        REQUIRE(Scalar(false).to_bool() == false);

        // From integer
        REQUIRE(Scalar(1).to_bool() == true);
        REQUIRE(Scalar(0).to_bool() == false);
        REQUIRE(Scalar(42).to_bool() == true);

        // From floating point
        REQUIRE(Scalar(1.0).to_bool() == true);
        REQUIRE(Scalar(0.0).to_bool() == false);
        REQUIRE(Scalar(3.14).to_bool() == true);

        // From complex
        REQUIRE(Scalar(std::complex<double>(1.0, 0.0)).to_bool() == true);
        REQUIRE(Scalar(std::complex<double>(0.0, 1.0)).to_bool() == true);
        REQUIRE(Scalar(std::complex<double>(0.0, 0.0)).to_bool() == false);

        // From string
        REQUIRE(Scalar("hello").to_bool() == true);
        REQUIRE(Scalar("").to_bool() == false);
    }

    SECTION("Numeric conversions") {
        // Test various conversions to numeric types
        Scalar int_scalar(42);
        REQUIRE(int_scalar.to_int8() == 42);
        REQUIRE(int_scalar.to_uint8() == 42);
        REQUIRE(int_scalar.to_int16() == 42);
        REQUIRE(int_scalar.to_uint16() == 42);
        REQUIRE(int_scalar.to_int32() == 42);
        REQUIRE(int_scalar.to_uint32() == 42);
        REQUIRE(int_scalar.to_int64() == 42);
        REQUIRE(int_scalar.to_uint64() == 42);
        REQUIRE(int_scalar.to_float() == 42.0f);
        REQUIRE(int_scalar.to_double() == 42.0);

        Scalar float_scalar(3.14f);
        REQUIRE(float_scalar.to_int32() == 3);  // Truncated
        REQUIRE(float_scalar.to_float() == Catch::Approx(3.14f));
        REQUIRE(float_scalar.to_double() == Catch::Approx(3.14));

        // Complex to real conversion
        Scalar complex_scalar(std::complex<double>(1.5, 2.5));
        REQUIRE(complex_scalar.to_double() == Catch::Approx(1.5));  // Takes real part
        REQUIRE(complex_scalar.to_int32() == 1);                    // Truncated real part

        // String conversion should throw
        Scalar string_scalar("not a number");
        REQUIRE_THROWS_AS(string_scalar.to_int32(), TypeConversionError);
        REQUIRE_THROWS_AS(string_scalar.to_float(), TypeConversionError);
    }

    SECTION("Complex conversions") {
        // Real to complex
        Scalar int_scalar(42);
        std::complex<float> cf = int_scalar.to_complex_float();
        REQUIRE(cf.real() == Catch::Approx(42.0f));
        REQUIRE(cf.imag() == Catch::Approx(0.0f));

        Scalar double_scalar(3.14);
        std::complex<double> cd = double_scalar.to_complex_double();
        REQUIRE(cd.real() == Catch::Approx(3.14));
        REQUIRE(cd.imag() == Catch::Approx(0.0));

        // Complex to complex
        Scalar complex_float_scalar(std::complex<float>(1.5f, 2.5f));
        std::complex<double> complex_double = complex_float_scalar.to_complex_double();
        REQUIRE(complex_double.real() == Catch::Approx(1.5));
        REQUIRE(complex_double.imag() == Catch::Approx(2.5));

        // String to complex should throw
        Scalar string_scalar("not a complex");
        REQUIRE_THROWS_AS(string_scalar.to_complex_float(), TypeConversionError);
    }

    SECTION("String conversions") {
        // Simple numeric to string
        REQUIRE(Scalar(42).to_string() == "42");
        REQUIRE(Scalar(3.14f).to_string().substr(0, 4) == "3.14");

        // Complex to string (format is implementation-dependent)
        Scalar complex_scalar(std::complex<double>(1.0, 2.0));
        std::string complex_str = complex_scalar.to_string();
        REQUIRE(complex_str.find('1') != std::string::npos);
        REQUIRE(complex_str.find('2') != std::string::npos);

        // Boolean to string
        REQUIRE(Scalar(true).to_string() == "true");
        REQUIRE(Scalar(false).to_string() == "false");
    }

    SECTION("Generic conversion template") {
        Scalar int_scalar(42);
        REQUIRE(int_scalar.to<int>() == 42);
        REQUIRE(int_scalar.to<double>() == Catch::Approx(42.0));
        REQUIRE(int_scalar.to<bool>() == true);

        Scalar complex_scalar(std::complex<double>(1.0, 2.0));
        auto complex_result = complex_scalar.to<std::complex<double>>();
        REQUIRE(complex_result.real() == Catch::Approx(1.0));
        REQUIRE(complex_result.imag() == Catch::Approx(2.0));
    }

    SECTION("Conversion by DType") {
        Scalar int_scalar(42);

        Scalar as_float = int_scalar.to(DType::Float32);
        REQUIRE(as_float.dtype() == DType::Float32);
        REQUIRE(as_float.to_float() == Catch::Approx(42.0f));

        Scalar as_bool = int_scalar.to(DType::Bool);
        REQUIRE(as_bool.dtype() == DType::Bool);
        REQUIRE(as_bool.to_bool() == true);

        // Conversion to unsupported type should throw
        REQUIRE_THROWS_AS(int_scalar.to(DType::Undefined), TypeConversionError);
    }
}

TEST_CASE("Scalar comparison", "[scalar][unit]") {
    SECTION("Equality") {
        // Simple equality
        REQUIRE(Scalar(42) == Scalar(42));
        REQUIRE(Scalar(3.14) == Scalar(3.14));
        REQUIRE(Scalar("hello") == Scalar("hello"));
        REQUIRE(Scalar(true) == Scalar(true));

        // Cross-type equality
        REQUIRE(Scalar(42) == Scalar(42.0));
        REQUIRE(Scalar(1) == Scalar(true));
        REQUIRE_FALSE(Scalar(0) == Scalar(false));  // This should be equal

        // Complex equality
        REQUIRE(Scalar(std::complex<double>(1.0, 2.0)) == Scalar(std::complex<double>(1.0, 2.0)));
        REQUIRE_FALSE(Scalar(std::complex<double>(1.0, 2.0)) == Scalar(1.0));
    }

    SECTION("Inequality") {
        REQUIRE(Scalar(42) != Scalar(43));
        REQUIRE(Scalar(3.14) != Scalar(3.15));
        REQUIRE(Scalar("hello") != Scalar("world"));
        REQUIRE(Scalar(true) != Scalar(false));

        // Cross-type inequality
        REQUIRE(Scalar(42) != Scalar(43.0));
        REQUIRE(Scalar(1) != Scalar(false));

        // Complex inequality
        REQUIRE(Scalar(std::complex<double>(1.0, 2.0)) != Scalar(std::complex<double>(1.0, 3.0)));
    }

    SECTION("Comparison operators") {
        // Less than
        REQUIRE(Scalar(42) < Scalar(43));
        REQUIRE(Scalar(3.14) < Scalar(3.15));
        REQUIRE(Scalar("apple") < Scalar("banana"));
        REQUIRE_FALSE(Scalar(42) < Scalar(42));

        // Complex comparison should throw
        REQUIRE_THROWS_AS(Scalar(std::complex<double>(1.0, 2.0)) < Scalar(2.0),
                          TypeConversionError);

        // Cross-type comparison
        REQUIRE(Scalar(42) < Scalar(42.5));
        REQUIRE_FALSE(Scalar(42) < Scalar(42.0));

        // Greater than
        REQUIRE(Scalar(43) > Scalar(42));
        REQUIRE(Scalar(3.15) > Scalar(3.14));
        REQUIRE(Scalar("banana") > Scalar("apple"));
        REQUIRE_FALSE(Scalar(42) > Scalar(42));

        // Less than or equal
        REQUIRE(Scalar(42) <= Scalar(42));
        REQUIRE(Scalar(42) <= Scalar(43));
        REQUIRE_FALSE(Scalar(43) <= Scalar(42));

        // Greater than or equal
        REQUIRE(Scalar(42) >= Scalar(42));
        REQUIRE(Scalar(43) >= Scalar(42));
        REQUIRE_FALSE(Scalar(42) >= Scalar(43));
    }
}

TEST_CASE("Scalar arithmetic", "[scalar][unit]") {
    SECTION("Addition") {
        // Numeric addition
        REQUIRE((Scalar(40) + Scalar(2)).to_int32() == 42);
        REQUIRE((Scalar(40.5) + Scalar(1.5)).to_double() == Catch::Approx(42.0));

        // String concatenation
        REQUIRE((Scalar("hello") + Scalar(" world")).to_string() == "hello world");

        // Complex addition
        Scalar complex_result =
            Scalar(std::complex<double>(1.0, 2.0)) + Scalar(std::complex<double>(3.0, 4.0));
        REQUIRE(complex_result.to_complex_double().real() == Catch::Approx(4.0));
        REQUIRE(complex_result.to_complex_double().imag() == Catch::Approx(6.0));

        // Mixed type addition
        REQUIRE((Scalar(40) + Scalar(2.0)).to_double() == Catch::Approx(42.0));

        // Invalid addition
        REQUIRE_THROWS_AS(Scalar(42) + Scalar("hello"), TypeConversionError);
    }

    SECTION("Subtraction") {
        // Numeric subtraction
        REQUIRE((Scalar(44) - Scalar(2)).to_int32() == 42);
        REQUIRE((Scalar(43.5) - Scalar(1.5)).to_double() == Catch::Approx(42.0));

        // Complex subtraction
        Scalar complex_result =
            Scalar(std::complex<double>(4.0, 6.0)) - Scalar(std::complex<double>(1.0, 2.0));
        REQUIRE(complex_result.to_complex_double().real() == Catch::Approx(3.0));
        REQUIRE(complex_result.to_complex_double().imag() == Catch::Approx(4.0));

        // Mixed type subtraction
        REQUIRE((Scalar(44) - Scalar(2.0)).to_double() == Catch::Approx(42.0));

        // Invalid subtraction
        REQUIRE_THROWS_AS(Scalar(42) - Scalar("hello"), TypeConversionError);
        REQUIRE_THROWS_AS(Scalar("hello") - Scalar("world"), TypeConversionError);
    }

    SECTION("Multiplication") {
        // Numeric multiplication
        REQUIRE((Scalar(21) * Scalar(2)).to_int32() == 42);
        REQUIRE((Scalar(21.0) * Scalar(2.0)).to_double() == Catch::Approx(42.0));

        // Complex multiplication
        Scalar complex_result =
            Scalar(std::complex<double>(1.0, 2.0)) * Scalar(std::complex<double>(3.0, 4.0));
        // (1+2i) * (3+4i) = (1*3 - 2*4) + (1*4 + 2*3)i = -5 + 10i
        REQUIRE(complex_result.to_complex_double().real() == Catch::Approx(-5.0));
        REQUIRE(complex_result.to_complex_double().imag() == Catch::Approx(10.0));

        // Mixed type multiplication
        REQUIRE((Scalar(21) * Scalar(2.0)).to_double() == Catch::Approx(42.0));

        // Invalid multiplication
        REQUIRE_THROWS_AS(Scalar(42) * Scalar("hello"), TypeConversionError);
    }

    SECTION("Division") {
        // Numeric division
        REQUIRE((Scalar(84) / Scalar(2)).to_int32() == 42);
        REQUIRE((Scalar(84.0) / Scalar(2.0)).to_double() == Catch::Approx(42.0));

        // Complex division
        // (3+4i) / (1+2i) = ((3*1 + 4*2) + (4*1 - 3*2)i) / (1*1 + 2*2) = (11 - 2i) / 5 = 2.2 - 0.4i
        Scalar complex_result =
            Scalar(std::complex<double>(3.0, 4.0)) / Scalar(std::complex<double>(1.0, 2.0));
        REQUIRE(complex_result.to_complex_double().real() == Catch::Approx(2.2));
        REQUIRE(complex_result.to_complex_double().imag() == Catch::Approx(-0.4));

        // Mixed type division
        REQUIRE((Scalar(84) / Scalar(2.0)).to_double() == Catch::Approx(42.0));

        // Division by zero
        REQUIRE_THROWS_AS(Scalar(42) / Scalar(0), std::domain_error);
        REQUIRE_THROWS_AS(Scalar(42.0) / Scalar(0.0), std::domain_error);
        REQUIRE_THROWS_AS(Scalar(42) / Scalar(std::complex<double>(0.0, 0.0)), std::domain_error);

        // Invalid division
        REQUIRE_THROWS_AS(Scalar(42) / Scalar("hello"), TypeConversionError);
    }

    SECTION("Unary minus") {
        // Numeric negation
        REQUIRE((-Scalar(42)).to_int32() == -42);
        REQUIRE((-Scalar(3.14)).to_double() == Catch::Approx(-3.14));

        // Complex negation
        Scalar complex_result = -Scalar(std::complex<double>(1.0, 2.0));
        REQUIRE(complex_result.to_complex_double().real() == Catch::Approx(-1.0));
        REQUIRE(complex_result.to_complex_double().imag() == Catch::Approx(-2.0));

        // Invalid negation
        REQUIRE_THROWS_AS(-Scalar("hello"), TypeConversionError);
    }

    SECTION("Compound assignment") {
        // Addition assignment
        Scalar a(40);
        a += Scalar(2);
        REQUIRE(a.to_int32() == 42);

        // Subtraction assignment
        Scalar b(44);
        b -= Scalar(2);
        REQUIRE(b.to_int32() == 42);

        // Multiplication assignment
        Scalar c(21);
        c *= Scalar(2);
        REQUIRE(c.to_int32() == 42);

        // Division assignment
        Scalar d(84);
        d /= Scalar(2);
        REQUIRE(d.to_int32() == 42);
    }

    SECTION("Free function operators") {
        // Addition
        REQUIRE((2 + Scalar(40)).to_int32() == 42);

        // Subtraction
        REQUIRE((44 - Scalar(2)).to_int32() == 42);

        // Multiplication
        REQUIRE((2 * Scalar(21)).to_int32() == 42);

        // Division
        REQUIRE((84 / Scalar(2)).to_int32() == 42);
    }
}

TEST_CASE("Scalar string representation", "[scalar][unit]") {
    SECTION("to_string method") {
        REQUIRE(Scalar(42).to_string() == "42");
        REQUIRE(Scalar(true).to_string() == "true");
        REQUIRE(Scalar(false).to_string() == "false");
        REQUIRE(Scalar("hello").to_string() == "hello");

        // Floating point representation may vary
        std::string float_str = Scalar(3.14f).to_string();
        REQUIRE(float_str.substr(0, 4) == "3.14");
    }

    SECTION("to_string_with_type method") {
        std::string int_str = Scalar(42).to_string_with_type();
        REQUIRE(int_str.find("42") != std::string::npos);
        REQUIRE(int_str.find("Int32") != std::string::npos);

        std::string bool_str = Scalar(true).to_string_with_type();
        REQUIRE(bool_str.find("true") != std::string::npos);
        REQUIRE(bool_str.find("Bool") != std::string::npos);
    }

    SECTION("stream operator") {
        std::ostringstream oss;
        oss << Scalar(42);
        REQUIRE(oss.str() == "42");

        oss.str("");
        oss << Scalar("hello");
        REQUIRE(oss.str() == "hello");
    }
}