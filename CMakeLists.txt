cmake_minimum_required(VERSION 3.16)

project(brezel
  VERSION 0.1.0
  DESCRIPTION "A modern C++ header-only tensor framework inspired by PyTorch"
  LANGUAGES CXX
)

# Define C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Add CMake modules path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Options
include(cmake/Options.cmake)
include(cmake/Utilities.cmake)

# Set output directories
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Define the interface library
add_library(brezel INTERFACE)
add_library(brezel::brezel ALIAS brezel)

# Set include directories
target_include_directories(brezel
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

# Set compile features
target_compile_features(brezel INTERFACE cxx_std_20)

# Apply compiler settings after the target is created
include(cmake/CompilerSettings.cmake)
brezel_setup_target(brezel)

# Find dependencies
find_package(fmt REQUIRED)
target_link_libraries(brezel INTERFACE fmt::fmt)

find_package(Threads REQUIRED)
target_link_libraries(brezel INTERFACE Threads::Threads)

if(BREZEL_USE_SPDLOG)
  find_package(spdlog REQUIRED)
  target_link_libraries(brezel INTERFACE spdlog::spdlog)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_SPDLOG)
endif()

if(BREZEL_USE_EIGEN)
  find_package(Eigen3 REQUIRED)
  target_link_libraries(brezel INTERFACE Eigen3::Eigen)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_EIGEN)
endif()

if(BREZEL_USE_BLAS)
  find_package(BLAS REQUIRED)

  # LAPACK may not be required, so let's make it optional for now
  find_package(LAPACK QUIET)

  if(BLAS_FOUND)
    target_link_libraries(brezel INTERFACE ${BLAS_LIBRARIES})
    target_compile_definitions(brezel INTERFACE BREZEL_WITH_BLAS)
  endif()

  if(LAPACK_FOUND)
    target_link_libraries(brezel INTERFACE ${LAPACK_LIBRARIES})
    target_compile_definitions(brezel INTERFACE BREZEL_WITH_LAPACK)
  endif()
endif()

if(BREZEL_USE_OPENMP)
  find_package(OpenMP REQUIRED)
  target_link_libraries(brezel INTERFACE OpenMP::OpenMP_CXX)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_OPENMP)
endif()

if(BREZEL_USE_CUDA)
  find_package(CUDA REQUIRED)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_CUDA)
  target_include_directories(brezel INTERFACE ${CUDA_INCLUDE_DIRS})
  target_link_libraries(brezel INTERFACE ${CUDA_LIBRARIES})
endif()

# Add subdirectories
if(BREZEL_COMPILE_EXTENSIONS)
  add_subdirectory(src)
  
  # Link the interface library to the core implementation
  target_link_libraries(brezel INTERFACE brezel::core)
endif()

if(BREZEL_BUILD_TESTS)
  find_package(Catch2 CONFIG REQUIRED)
  include(CTest)

  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests")
    add_subdirectory(tests)
  endif()
endif()

if(BREZEL_BUILD_EXAMPLES)
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/examples")
    add_subdirectory(examples)
  endif()
endif()

if(BREZEL_BUILD_BENCHMARKS)
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/benchmarks")
    add_subdirectory(benchmarks)
  endif()
endif()

# Installation
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Skip installation in CI environments
if(NOT DEFINED ENV{CI} OR NOT CI)
  install(
    DIRECTORY include/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
  )

  # Create and install package config files
  configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/brezelConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/brezelConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/brezel
  )

  write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/brezelConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
  )

  # Export targets
  install(
    TARGETS brezel
    EXPORT brezelTargets
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )

  install(
    EXPORT brezelTargets
    FILE brezelTargets.cmake
    NAMESPACE brezel::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/brezel
  )

  install(
    FILES
    ${CMAKE_CURRENT_BINARY_DIR}/brezelConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/brezelConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/brezel
  )

  # Create uninstall target
  if(NOT TARGET uninstall)
    configure_file(
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
      "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
      IMMEDIATE @ONLY
    )

    add_custom_target(uninstall
      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake
    )
  endif()
endif()

# Print configuration summary
include(cmake/Summary.cmake)