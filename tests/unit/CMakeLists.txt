# Add test executable
add_executable(simple_test
  device_test.cpp
  dtype_test.cpp
  scalar_test.cpp
  shape_test.cpp
)

# Link against the correct target based on what's available
if(TARGET brezel_core)
  target_link_libraries(simple_test PRIVATE brezel_core Catch2::Catch2WithMain)
else()
  target_link_libraries(simple_test PRIVATE brezel::brezel Catch2::Catch2WithMain)
endif()

# Register test with CTest
add_test(NAME simple_test COMMAND simple_test)

# Set output directory
set_target_properties(simple_test PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/tests"
)

# Include directories
target_include_directories(simple_test
  PRIVATE
  ${CMAKE_SOURCE_DIR}/include
)