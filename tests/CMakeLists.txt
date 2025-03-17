# Enable testing
enable_testing()

# Find Catch2 - should already be set up by Dependencies.cmake
if(NOT TARGET Catch2::Catch2WithMain)
  message(STATUS "Catch2 not found through Dependencies.cmake, trying direct find_package")
  find_package(Catch2 REQUIRED)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/tests)
add_subdirectory(unit)

# Add more test directories as they are created
# add_subdirectory(integration)
# add_subdirectory(performance)