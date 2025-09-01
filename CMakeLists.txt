cmake_minimum_required(VERSION 3.20)

# Project setup
project(TensorFramework 
  VERSION 1.0.0
  DESCRIPTION "Modern C++ Tensor Framework"
  LANGUAGES CXX
)

# C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Build options
option(TENSOR_BUILD_TESTS "Build tests" ON)
option(TENSOR_BUILD_BENCHMARKS "Build benchmarks" ON)
option(TENSOR_BUILD_EXAMPLES "Build examples" ON)
option(TENSOR_ENABLE_SIMD "Enable SIMD optimizations" ON)
option(TENSOR_ENABLE_OPENMP "Enable OpenMP support" ON)
option(TENSOR_ENABLE_PROFILING "Enable profiling support" OFF)
option(TENSOR_HEADER_ONLY "Header-only mode" OFF)

# Include CMake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(CompilerOptions)
include(Dependencies)
include(Utils)

# Global definitions
add_compile_definitions(
  TENSOR_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
  TENSOR_VERSION_MINOR=${PROJECT_VERSION_MINOR}
  TENSOR_VERSION_PATCH=${PROJECT_VERSION_PATCH}
)

# Platform definitions
if(WIN32)
  add_compile_definitions(TENSOR_PLATFORM_WINDOWS _CRT_SECURE_NO_WARNINGS)
elseif(UNIX AND NOT APPLE)
  add_compile_definitions(TENSOR_PLATFORM_LINUX)
elseif(APPLE)
  add_compile_definitions(TENSOR_PLATFORM_MACOS)
endif()

# Configuration-specific definitions
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_definitions(TENSOR_DEBUG TENSOR_ENABLE_ASSERTS)
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
  add_compile_definitions(TENSOR_RELEASE NDEBUG)
elseif(CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo")
  add_compile_definitions(TENSOR_PROFILE TENSOR_ENABLE_PROFILING)
endif()

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Export compile commands for IDE support
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find dependencies
find_package(Threads REQUIRED)

# SIMD support
if(TENSOR_ENABLE_SIMD)
  find_simd_support()
endif()

# OpenMP support
if(TENSOR_ENABLE_OPENMP)
  find_package(OpenMP)
  if(OpenMP_CXX_FOUND)
    add_compile_definitions(TENSOR_ENABLE_OPENMP)
  endif()
endif()

# Core library
add_subdirectory(src)

# Examples
if(TENSOR_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

# Tests
if(TENSOR_BUILD_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()

# Tools
add_subdirectory(tools)

# Installation
include(GNUInstallDirs)

# Install headers
install(
  DIRECTORY include/tensor
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  FILES_MATCHING PATTERN "*.hpp"
)

# Install main header
install(
  FILES include/tensor.hpp
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

# Create and install config files
include(CMakePackageConfigHelpers)

# Generate version file
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/TensorFrameworkConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)

# Generate config file
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/TensorFrameworkConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/TensorFrameworkConfig.cmake"
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/TensorFramework
)

# Install config files
install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/TensorFrameworkConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/TensorFrameworkConfigVersion.cmake"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/TensorFramework
)

# Export targets
install(
  EXPORT TensorFrameworkTargets
  FILE TensorFrameworkTargets.cmake
  NAMESPACE TensorFramework::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/TensorFramework
)

# CPack configuration for packaging
set(CPACK_PACKAGE_NAME "TensorFramework")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Modern C++ Tensor Framework")
set(CPACK_PACKAGE_VENDOR "TensorFramework Team")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

include(CPack)

# Custom targets
add_custom_target(format
  COMMAND find ${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/src 
          ${CMAKE_SOURCE_DIR}/tests ${CMAKE_SOURCE_DIR}/examples
          -name '*.hpp' -o -name '*.cpp' | xargs clang-format -i
  COMMENT "Formatting code with clang-format"
)

add_custom_target(lint
  COMMAND clang-tidy -p ${CMAKE_BINARY_DIR} 
          ${CMAKE_SOURCE_DIR}/src/**/*.cpp
  COMMENT "Running static analysis with clang-tidy"
  DEPENDS ${CMAKE_BINARY_DIR}/compile_commands.json
)

add_custom_target(docs
  COMMAND doxygen ${CMAKE_SOURCE_DIR}/docs/Doxyfile
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  COMMENT "Generating documentation with Doxygen"
)

# Print configuration summary
message(STATUS "")
message(STATUS "TensorFramework Configuration Summary:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "  Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
message(STATUS "Options:")
message(STATUS "  Build tests: ${TENSOR_BUILD_TESTS}")
message(STATUS "  Build benchmarks: ${TENSOR_BUILD_BENCHMARKS}")
message(STATUS "  Build examples: ${TENSOR_BUILD_EXAMPLES}")
message(STATUS "  Enable SIMD: ${TENSOR_ENABLE_SIMD}")
message(STATUS "  Enable OpenMP: ${TENSOR_ENABLE_OPENMP}")
message(STATUS "  Enable profiling: ${TENSOR_ENABLE_PROFILING}")
message(STATUS "  Header-only mode: ${TENSOR_HEADER_ONLY}")
message(STATUS "")