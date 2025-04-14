# Unit tests configuration

# Add expected_test executable
add_executable(expected_test)

# Link with required libraries
target_link_libraries(expected_test PRIVATE
  brezel::brezel
  Catch2::Catch2WithMain
)

# Register with CTest
add_test(NAME expected_test COMMAND expected_test)
