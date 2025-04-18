#pragma once

#include <iostream>
#include <random>
#include <string>
#include <vector>

namespace test {
/**
 * @brief Generates a vector of random integers within a specified range using
 * a thread-safe generator
 *
 * @param count Number of random integers to generate
 * @param min Minimum value (inclusive) of the random distribution
 * @param max Maximum value (inclusive) of the random distribution
 * @return std::vector<int> Vector containing the random integers
 * @throws std::invalid_argument if max < min, count is 0, or
 *         if the range exceeds INT_MAX
 * @thread_safety This function is thread-safe
 */
inline std::vector<int> generate_random_ints(size_t count, int min = 0, int max = 100) {
    if (count == 0) {
        throw std::invalid_argument("count must be greater than 0");
    }

    if (max < min) {
        throw std::invalid_argument("max must be greater than or equal to min");
    }

    if (static_cast<int64_t>(max) - static_cast<int64_t>(min) > std::numeric_limits<int>::max()) {
        throw std::invalid_argument("range too large");
    }

    static thread_local std::mt19937 gen(std::random_device{}());
    std::uniform_int_distribution<int> distrib(min, max);

    std::vector<int> result;
    result.reserve(count);

    std::generate_n(std::back_inserter(result), count, [&]() { return distrib(gen); });

    return result;
}

/**
 * @brief Function to generate random double values
 *
 * @param count The size of the vector to generate (number of elements)
 * @param min Minimum value of the distribution
 * @param max Maximum value of the distribution
 * @return std::vector<int> Random resulting vector with n elements
 */
inline std::vector<double> generate_random_doubles(size_t count, int min = 0.0, int max = 1.0) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> distrib(min, max);

    std::vector<double> result(count);
    for (auto& val : result) {
        val = distrib(gen);
    }

    return result;
}

/**
 * @brief Function to compare two vectors of floating point values with
 * tolerance
 *
 * @tparam T Data type of the vectors
 * @param a First vector
 * @param b Second vector
 * @param tolerance Tolerance between differences
 * @return True if the vectors are equal
 * @return False if the vectors are not equal (even with tolerance)
 */
template <typename T>
inline bool are_vectors_equal(const std::vector<T>& a, const std::vector<T>& b,
                              T tolerance = 1e-6) {
    if (a.size() != b.size()) {
        return false;
    }

    for (size_t i = 0; i < a.size(); ++i) {
        if (std::abs(a[i] - b[i]) > tolerance) {
            return false;
        }
    }

    return true;
}
}  // namespace test