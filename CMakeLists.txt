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

# Options
option(BREZEL_BUILD_TESTS "Build tests" ON)
option(BREZEL_BUILD_EXAMPLES "Build examples" ON)
option(BREZEL_BUILD_BENCHMARKS "Build benchmarks" OFF)
option(BREZEL_USE_CUDA "Enable CUDA support" OFF)
option(BREZEL_USE_OPENMP "Enable OpenMP support" ON)
option(BREZEL_USE_BLAS "Enable BLAS/LAPACK support" ON)
option(BREZEL_COMPILE_EXTENSIONS "Compile optional performance extensions" OFF)
option(BREZEL_ENABLE_PROFILING "Enable performance profiling" OFF)
option(BREZEL_USE_DOCKER "Use Docker for building and testing" ON)
option(BREZEL_BUILD_DOCS "Build documentation" OFF)
option(BREZEL_DOCS_WITH_BUILD "Build documentation as part of the default build" OFF)

# Add CMake modules path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Include custom CMake modules that don't depend on targets
include(cmake/CompilerWarnings.cmake)
include(cmake/Sanitizers.cmake)
include(cmake/StaticAnalyzers.cmake)
include(cmake/Dependencies.cmake)
include(cmake/Workflow.cmake)
include(cmake/Documentation.cmake)

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

# Add compiler warnings
set_project_warnings(brezel)

# Apply sanitizers if enabled
enable_sanitizers(brezel)

# Link with dependencies
# fmt is required
target_link_libraries(brezel INTERFACE fmt::fmt)

# Add conditional compile definitions and dependencies AFTER target is created
if(BREZEL_USE_OPENMP)
  find_package(OpenMP REQUIRED)
  target_link_libraries(brezel INTERFACE OpenMP::OpenMP_CXX)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_OPENMP)
endif()

if(BREZEL_USE_BLAS)
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
  target_link_libraries(brezel INTERFACE ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_BLAS)
endif()

if(BREZEL_USE_CUDA)
  find_package(CUDA REQUIRED)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_CUDA)
  target_include_directories(brezel INTERFACE ${CUDA_INCLUDE_DIRS})
  target_link_libraries(brezel INTERFACE ${CUDA_LIBRARIES})
endif()

if(BREZEL_ENABLE_PROFILING)
  target_compile_definitions(brezel INTERFACE BREZEL_ENABLE_PROFILING)
endif()

# Add conditional compile definitions set in Dependencies.cmake
if(BREZEL_WITH_EIGEN)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_EIGEN)
  target_link_libraries(brezel INTERFACE Eigen3::Eigen)
endif()

if(BREZEL_WITH_SPDLOG)
  target_compile_definitions(brezel INTERFACE BREZEL_WITH_SPDLOG)
  target_link_libraries(brezel INTERFACE spdlog::spdlog)
endif()

# Check for threading library support
find_package(Threads REQUIRED)
target_link_libraries(brezel INTERFACE Threads::Threads)

# Conditionally compile extensions
if(BREZEL_COMPILE_EXTENSIONS)
  add_subdirectory(src)
endif()

# Add subdirectories for components
# if(BREZEL_BUILD_TESTS)
# enable_testing()
# add_subdirectory(tests)
# endif()

# if(BREZEL_BUILD_EXAMPLES)
# add_subdirectory(examples)
# endif()

# if(BREZEL_BUILD_BENCHMARKS)
# add_subdirectory(benchmarks)
# endif()

# Add Docker targets if Docker is enabled
if(BREZEL_USE_DOCKER)
  brezel_add_docker_targets()
endif()

# Add code formatting target and documentation
brezel_enable_formatting()
brezel_setup_documentation()

# Installation
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

install(
  TARGETS brezel
  EXPORT brezelTargets
  INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

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

install(
  FILES
  ${CMAKE_CURRENT_BINARY_DIR}/brezelConfig.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/brezelConfigVersion.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/brezel
)

install(
  EXPORT brezelTargets
  FILE brezelTargets.cmake
  NAMESPACE brezel::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/brezel
)

# Documentation
if(BREZEL_BUILD_DOCS)
  find_package(Doxygen)

  if(DOXYGEN_FOUND)
    # Doxygen configuration
    set(DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/docs)
    set(DOXYGEN_PROJECT_NAME "Brezel")
    set(DOXYGEN_PROJECT_NUMBER ${PROJECT_VERSION})
    set(DOXYGEN_PROJECT_BRIEF "A modern C++ tensor framework")
    set(DOXYGEN_EXTRACT_ALL YES)
    set(DOXYGEN_EXTRACT_PRIVATE YES)
    set(DOXYGEN_EXTRACT_PACKAGE YES)
    set(DOXYGEN_EXTRACT_STATIC YES)
    set(DOXYGEN_EXTRACT_LOCAL_CLASSES YES)
    set(DOXYGEN_RECURSIVE YES)
    set(DOXYGEN_USE_MDFILE_AS_MAINPAGE README.md)
    set(DOXYGEN_GENERATE_HTML YES)
    set(DOXYGEN_GENERATE_XML YES)
    set(DOXYGEN_GENERATE_LATEX NO)
    set(DOXYGEN_JAVADOC_AUTOBRIEF YES)
    set(DOXYGEN_QT_AUTOBRIEF YES)
    set(DOXYGEN_WARN_AS_ERROR YES)

    # Generate Doxyfile
    doxygen_add_docs(docs
      ${CMAKE_CURRENT_SOURCE_DIR}/include
      ${CMAKE_CURRENT_SOURCE_DIR}/README.md
      ALL
      COMMENT "Generating API documentation with Doxygen"
    )
  else()
    message(STATUS "Doxygen not found, documentation will not be built")
  endif()
endif()

# Packaging
include(cmake/Packaging.cmake)