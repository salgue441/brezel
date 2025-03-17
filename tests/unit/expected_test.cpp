#include <brezel/utils/error.hpp>
#include <brezel/utils/expected.hpp>

#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using namespace brezel::utils;

class TestError : public std::exception {
public:
    explicit TestError(std::string message) : m_message(std::move(message)) {}
    const char* what() const noexcept override { return m_message.c_str(); }
    bool operator==(const TestError& other) const { return m_message == other.m_message; }
    bool operator!=(const TestError& other) const { return !(*this == other); }

private:
    std::string m_message;
};

TEST_CASE("Expected with value", "{expected}") {
    auto ok_int = make_ok<int, TestError>(42);

    SECTION("Basic value checks") {
        REQUIRE(ok_int.has_value());
        REQUIRE(static_cast<bool>(ok_int));
        REQUIRE(ok_int.value() == 42);
    }

    SECTION("value_or") { REQUIRE(ok_int.value_or(0) == 42); }
}