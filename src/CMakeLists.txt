# src/CMakeLists.txt - Main implementation of the brezel tensor library

# Organize source files by component for better maintainability
set(BREZEL_CORE_SOURCES
  core/types.cpp

  # Add more core sources as they're created
  # core/tensor.cpp
  # core/device.cpp
  # core/storage.cpp
)

set(BREZEL_OPS_SOURCES

  # Add operation sources as they're created
  # ops/basic_ops.cpp
  # ops/reduction.cpp
  # ops/activation.cpp
)

set(BREZEL_AUTOGRAD_SOURCES

  # Add autograd sources as they're created
  # autograd/variable.cpp
  # autograd/function.cpp
  # autograd/engine.cpp
)

# Define the core library target
add_library(brezel_core
  ${BREZEL_CORE_SOURCES}
)

# Create alias target for core
add_library(brezel::core ALIAS brezel_core)

# Set target properties for core
set_target_properties(brezel_core PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED ON
  CXX_EXTENSIONS OFF
  POSITION_INDEPENDENT_CODE ON
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  OUTPUT_NAME "brezel_core"
)

# Add include directories for core
target_include_directories(brezel_core
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)

# Apply compiler settings from CompilerSettings.cmake
brezel_setup_target(brezel_core)

# Add required dependencies for core
target_link_libraries(brezel_core
  PUBLIC
  fmt::fmt
  Threads::Threads
)

# Add Boost libraries if needed
if(Boost_FOUND)
  target_link_libraries(brezel_core
    PUBLIC
    Boost::system
    Boost::thread
  )
endif()

# Optional dependencies based on build options
if(BREZEL_USE_OPENMP)
  target_link_libraries(brezel_core PUBLIC OpenMP::OpenMP_CXX)
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_OPENMP=1)
else()
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_OPENMP=0)
endif()

if(BREZEL_USE_BLAS)
  target_link_libraries(brezel_core PUBLIC BLAS::BLAS)
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_BLAS=1)

  if(BREZEL_WITH_LAPACK)
    target_link_libraries(brezel_core PUBLIC LAPACK::LAPACK)
    target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_LAPACK=1)
  else()
    target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_LAPACK=0)
  endif()
else()
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_BLAS=0 BREZEL_WITH_LAPACK=0)
endif()

if(BREZEL_USE_CUDA)
  target_link_libraries(brezel_core PUBLIC CUDA::cudart CUDA::cublas)
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_CUDA=1)

  # Set CUDA-specific compile options
  set_target_properties(brezel_core PROPERTIES
    CUDA_STANDARD 17
    CUDA_STANDARD_REQUIRED ON
  )
else()
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_CUDA=0)
endif()

if(BREZEL_USE_EIGEN)
  target_link_libraries(brezel_core PUBLIC Eigen3::Eigen)
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_EIGEN=1)
else()
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_EIGEN=0)
endif()

if(BREZEL_USE_SPDLOG)
  target_link_libraries(brezel_core PUBLIC spdlog::spdlog)
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_SPDLOG=1)
else()
  target_compile_definitions(brezel_core PUBLIC BREZEL_WITH_SPDLOG=0)
endif()

# Enable profiling if requested
if(BREZEL_ENABLE_PROFILING)
  target_compile_definitions(brezel_core PRIVATE BREZEL_PROFILING=1)
else()
  target_compile_definitions(brezel_core PRIVATE BREZEL_PROFILING=0)
endif()

# Add precompiled headers if enabled
if(BREZEL_WITH_PCH AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.16)
  target_precompile_headers(brezel_core PRIVATE
    <vector>
    <memory>
    <string>
    <unordered_map>
    <functional>
    <algorithm>
    <utility>
    <numeric>
    <cmath>
    <fmt/core.h>
  )
endif()

# Add static analysis tools if enabled
if(BREZEL_ENABLE_CLANG_TIDY AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.6)
  find_program(CLANG_TIDY_EXE NAMES clang-tidy)

  if(CLANG_TIDY_EXE)
    set_target_properties(brezel_core PROPERTIES
      CXX_CLANG_TIDY "${CLANG_TIDY_EXE};-checks=*,-modernize-use-trailing-return-type"
    )
  else()
    message(WARNING "clang-tidy requested but executable not found")
  endif()
endif()

if(BREZEL_ENABLE_CPPCHECK AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.10)
  find_program(CPPCHECK_EXE NAMES cppcheck)

  if(CPPCHECK_EXE)
    set_target_properties(brezel_core PROPERTIES
      CXX_CPPCHECK "${CPPCHECK_EXE};--std=c++20;--enable=all"
    )
  else()
    message(WARNING "cppcheck requested but executable not found")
  endif()
endif()

# Link the implementation to the brezel interface library and add to export
target_link_libraries(brezel INTERFACE brezel_core)

# Make sure brezel_core is in the export set
install(TARGETS brezel_core EXPORT brezelTargets)