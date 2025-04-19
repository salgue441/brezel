# Enable testing functionality
enable_testing()

# Set up function to add tests
function(brezel_add_test test_name)
  cmake_parse_arguments(
    TEST
    ""
    ""
    "SOURCES;DEPS"
    ${ARGN}
  )

  add_executable(${test_name} ${TEST_SOURCES})
  link_directories(${CMAKE_BINARY_DIR}/lib)

  target_link_libraries(${test_name}
    PRIVATE
    brezel_core
    Catch2::Catch2WithMain
    ${TEST_DEPS}
  )

  target_include_directories(${test_name}
    PRIVATE
    ${CMAKE_SOURCE_DIR}/include
  )

  add_test(NAME ${test_name} COMMAND ${test_name})

  set_target_properties(${test_name}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/tests"
  )
endfunction()

# Add unit tests
add_subdirectory(unit)

# Add custom test target
add_custom_target(build_tests)
add_dependencies(build_tests simple_test)

# Add custom target to run all tests
add_custom_target(run_tests
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
  DEPENDS build_tests
  COMMENT "Running all tests"
)