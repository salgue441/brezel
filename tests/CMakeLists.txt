# Tests configuration
if(NOT BREZEL_BUILD_TESTS)
  return()
endif()

message(STATUS "Configuring tests")

# Include catch2 for test discovery
find_package(Catch2 CONFIG REQUIRED)
include(Catch)

# Set up common includes and definitions
set(TEST_COMMON_INCLUDE_DIRS
  ${CMAKE_SOURCE_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/common
)

# Set up common link libraries
set(TEST_COMMON_LINK_LIBS
  brezel::brezel
  Catch2::Catch2WithMain
)

# Define a function to add tests consistently in subdirectories
function(brezel_add_test test_name)
  cmake_parse_arguments(
    TEST # prefix
    "" # options
    "" # one-value keywords
    "SOURCES;DEPENDENCIES;LABELS" # multi-value keywords
    ${ARGN}
  )

  # Create the test executable
  add_executable(${test_name} ${TEST_SOURCES})

  # Add include directories
  target_include_directories(${test_name} PRIVATE ${TEST_COMMON_INCLUDE_DIRS})

  # Link libraries
  target_link_libraries(${test_name} PRIVATE ${TEST_COMMON_LINK_LIBS} ${TEST_DEPENDENCIES})

  # Set output directory
  set_target_properties(${test_name} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/tests"
  )

  # Add to CTest
  add_test(NAME ${test_name} COMMAND ${test_name})

  # Add labels if provided
  if(TEST_LABELS)
    set_tests_properties(${test_name} PROPERTIES LABELS "${TEST_LABELS}")
  endif()

  # Add to build_tests target
  if(NOT TARGET build_tests)
    add_custom_target(build_tests)
  endif()

  add_dependencies(build_tests ${test_name})
endfunction()

# Add subdirectories for different test types
add_subdirectory(unit)
add_subdirectory(integration)

# Create targets to run different types of tests
add_custom_target(run_unit_tests
  COMMAND ${CMAKE_CTEST_COMMAND} -L "unit" --output-on-failure
  DEPENDS build_tests
  COMMENT "Running unit tests"
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

add_custom_target(run_integration_tests
  COMMAND ${CMAKE_CTEST_COMMAND} -L "integration" --output-on-failure
  DEPENDS build_tests
  COMMENT "Running integration tests"
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

# Create a target to run all tests
add_custom_target(run_tests
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
  DEPENDS build_tests
  COMMENT "Running all tests"
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)