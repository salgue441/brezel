# CMake workflow helper functions for Brezel project
# This file provides standard targets and utilities for the build process

include(CMakeParseArguments)

# Function to register a benchmark
function(brezel_add_benchmark)
  if(NOT BREZEL_BUILD_BENCHMARKS)
    return()
  endif()

  cmake_parse_arguments(
    BENCHMARK # prefix
    "" # options
    "NAME;EXECUTABLE" # one-value keywords
    "SOURCES;DEPENDENCIES" # multi-value keywords
    ${ARGN}
  )

  if(NOT BENCHMARK_NAME)
    message(FATAL_ERROR "You must provide a NAME for the benchmark")
  endif()

  if(NOT BENCHMARK_SOURCES AND NOT BENCHMARK_EXECUTABLE)
    message(FATAL_ERROR "You must provide either SOURCES or an EXECUTABLE for the benchmark")
  endif()

  if(BENCHMARK_EXECUTABLE)
    add_executable(${BENCHMARK_NAME} ${BENCHMARK_EXECUTABLE})
  else()
    add_executable(${BENCHMARK_NAME} ${BENCHMARK_SOURCES})
  endif()

  target_link_libraries(${BENCHMARK_NAME} PRIVATE brezel::brezel ${BENCHMARK_DEPENDENCIES})

  # Set output directory
  set_target_properties(${BENCHMARK_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/benchmarks"
  )

  # Add the benchmark to a custom target
  if(NOT TARGET brezel_benchmarks)
    add_custom_target(brezel_benchmarks)
  endif()

  add_dependencies(brezel_benchmarks ${BENCHMARK_NAME})
endfunction()

# Function to register a test
function(brezel_add_test)
  if(NOT BREZEL_BUILD_TESTS)
    return()
  endif()

  cmake_parse_arguments(
    TEST # prefix
    "" # options
    "NAME;EXECUTABLE" # one-value keywords
    "SOURCES;DEPENDENCIES" # multi-value keywords
    ${ARGN}
  )

  if(NOT TEST_NAME)
    message(FATAL_ERROR "You must provide a NAME for the test")
  endif()

  if(NOT TEST_SOURCES AND NOT TEST_EXECUTABLE)
    message(FATAL_ERROR "You must provide either SOURCES or an EXECUTABLE for the test")
  endif()

  if(TEST_EXECUTABLE)
    add_executable(${TEST_NAME} ${TEST_EXECUTABLE})
  else()
    add_executable(${TEST_NAME} ${TEST_SOURCES})
  endif()

  target_link_libraries(${TEST_NAME} PRIVATE
    brezel::brezel
    Catch2::Catch2WithMain
    ${TEST_DEPENDENCIES}
  )

  # Set output directory
  set_target_properties(${TEST_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/tests"
  )

  # Register with CTest
  add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})

  # Set a timeout for tests (60 seconds)
  set_tests_properties(${TEST_NAME} PROPERTIES TIMEOUT 60)

  # Add to custom test target
  if(NOT TARGET brezel_tests)
    add_custom_target(brezel_tests)
  endif()

  add_dependencies(brezel_tests ${TEST_NAME})
endfunction()

# Function to register an example
function(brezel_add_example)
  if(NOT BREZEL_BUILD_EXAMPLES)
    return()
  endif()

  cmake_parse_arguments(
    EXAMPLE # prefix
    "" # options
    "NAME;EXECUTABLE" # one-value keywords
    "SOURCES;DEPENDENCIES" # multi-value keywords
    ${ARGN}
  )

  if(NOT EXAMPLE_NAME)
    message(FATAL_ERROR "You must provide a NAME for the example")
  endif()

  if(NOT EXAMPLE_SOURCES AND NOT EXAMPLE_EXECUTABLE)
    message(FATAL_ERROR "You must provide either SOURCES or an EXECUTABLE for the example")
  endif()

  if(EXAMPLE_EXECUTABLE)
    add_executable(${EXAMPLE_NAME} ${EXAMPLE_EXECUTABLE})
  else()
    add_executable(${EXAMPLE_NAME} ${EXAMPLE_SOURCES})
  endif()

  target_link_libraries(${EXAMPLE_NAME} PRIVATE brezel::brezel ${EXAMPLE_DEPENDENCIES})

  # Set output directory
  set_target_properties(${EXAMPLE_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/examples"
  )

  # Add to custom example target
  if(NOT TARGET brezel_examples)
    add_custom_target(brezel_examples)
  endif()

  add_dependencies(brezel_examples ${EXAMPLE_NAME})
endfunction()

# Function to enable formatting with clang-format
function(brezel_enable_formatting)
  find_program(CLANG_FORMAT clang-format)

  if(NOT CLANG_FORMAT)
    message(STATUS "clang-format not found. Formatting target will not be available.")
    return()
  endif()

  cmake_parse_arguments(
    FORMAT # prefix
    "" # options
    "" # one-value keywords
    "TARGETS;PATTERNS" # multi-value keywords
    ${ARGN}
  )

  if(NOT FORMAT_PATTERNS)
    set(FORMAT_PATTERNS
      "include/*.h"
      "include/*.hpp"
      "src/*.cpp"
      "src/*.h"
      "tests/*.cpp"
      "tests/*.h"
      "examples/*.cpp"
      "examples/*.h"
    )
  endif()

  # Create format target
  add_custom_target(format
    COMMENT "Running clang-format to format source code..."
  )

  # Add format commands for each pattern
  foreach(pattern ${FORMAT_PATTERNS})
    file(GLOB_RECURSE FILES ${pattern})

    if(FILES)
      add_custom_command(
        TARGET format
        COMMAND ${CLANG_FORMAT} -i ${FILES}
        COMMENT "Formatting ${pattern} files"
      )
    endif()
  endforeach()
endfunction()

# Function to add Docker targets
function(brezel_add_docker_targets)
  # Check if Docker is available
  find_program(DOCKER_EXECUTABLE docker)
  find_program(DOCKER_COMPOSE_EXECUTABLE docker-compose)

  if(NOT DOCKER_EXECUTABLE OR NOT DOCKER_COMPOSE_EXECUTABLE)
    message(STATUS "Docker and/or docker-compose not found. Docker targets will not be available.")
    return()
  endif()

  # Docker build target
  add_custom_target(docker-build
    COMMAND ${DOCKER_COMPOSE_EXECUTABLE} build
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Building Docker image..."
  )

  # Docker run with shell target
  add_custom_target(docker-shell
    COMMAND ${DOCKER_COMPOSE_EXECUTABLE} run --rm brezel-dev
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Starting interactive shell in Docker container..."
  )

  # Docker run build target
  add_custom_target(docker-run-build
    COMMAND ${DOCKER_COMPOSE_EXECUTABLE} run --rm brezel-dev bash -c
    "mkdir -p build && cd build && cmake .. -DCMAKE_BUILD_TYPE=Release && cmake --build . --parallel $(nproc)"
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Building project in Docker container..."
  )

  # Docker run tests target
  add_custom_target(docker-run-tests
    COMMAND ${DOCKER_COMPOSE_EXECUTABLE} run --rm brezel-dev bash -c
    "cd build && ctest --output-on-failure"
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Running tests in Docker container..."
  )

  # Docker clean target
  add_custom_target(docker-clean
    COMMAND ${DOCKER_COMPOSE_EXECUTABLE} down --rmi local --volumes --remove-orphans
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Cleaning Docker resources..."
  )
endfunction()