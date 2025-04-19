# Add test executable
add_executable(simple_test
  types_test.cpp
)

# Link against correct library targets and testing framework
target_link_libraries(simple_test
  PRIVATE
  brezel_core
  Catch2::Catch2WithMain
)

# Register test with CTest
add_test(NAME simple_test COMMAND simple_test)
set_target_properties(simple_test PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/tests"
)

target_include_directories(simple_test
  PRIVATE
  ${CMAKE_SOURCE_DIR}/include
)