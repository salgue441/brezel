#!/bin/bash
# This script is intended to be run inside the Docker container

set -e

# Default configuration
BUILD_DIR="build"
BUILD_TYPE="Release"
CMAKE_OPTIONS=""
BUILD_DOCS=OFF
BUILD_EXAMPLES=ON
BUILD_TESTS=ON
BUILD_BENCHMARKS=OFF
USE_SANITIZERS=OFF
WITH_COVERAGE=OFF
PARALLEL_JOBS=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  key="$1"
  case $key in
  --build-dir)
    BUILD_DIR="$2"
    shift
    shift
    ;;
  --build-type)
    BUILD_TYPE="$2"
    shift
    shift
    ;;
  --docs)
    BUILD_DOCS=ON
    shift
    ;;
  --no-examples)
    BUILD_EXAMPLES=OFF
    shift
    ;;
  --no-tests)
    BUILD_TESTS=OFF
    shift
    ;;
  --benchmarks)
    BUILD_BENCHMARKS=ON
    shift
    ;;
  --sanitizers)
    USE_SANITIZERS=ON
    shift
    ;;
  --coverage)
    WITH_COVERAGE=ON
    shift
    ;;
  --cmake-options)
    CMAKE_OPTIONS="$2"
    shift
    shift
    ;;
  -j | --jobs)
    PARALLEL_JOBS="$2"
    shift
    shift
    ;;
  *)
    echo "Unknown option: $key"
    echo "Usage: $0 [options]"
    echo "Options:"
    echo "  --build-dir DIR     Specify build directory (default: build)"
    echo "  --build-type TYPE   Specify build type: Debug, Release, etc. (default: Release)"
    echo "  --docs              Build documentation"
    echo "  --no-examples       Don't build examples"
    echo "  --no-tests          Don't build tests"
    echo "  --benchmarks        Build benchmarks"
    echo "  --sanitizers        Enable sanitizers"
    echo "  --coverage          Enable code coverage"
    echo "  --cmake-options     Additional CMake options (quoted)"
    echo "  -j, --jobs N        Number of parallel jobs (default: auto)"
    exit 1
    ;;
  esac
done

# Create build directory
mkdir -p $BUILD_DIR
cd $BUILD_DIR

# Sanitizer flags
SANITIZER_FLAGS=""
if [[ "$USE_SANITIZERS" == "ON" ]]; then
  if [[ "$BUILD_TYPE" == "Debug" ]]; then
    SANITIZER_FLAGS="-DENABLE_SANITIZER_ADDRESS=ON -DENABLE_SANITIZER_UNDEFINED_BEHAVIOR=ON"
  else
    echo "Warning: Sanitizers are most effective in Debug mode. Continuing with sanitizers in $BUILD_TYPE mode."
    SANITIZER_FLAGS="-DENABLE_SANITIZER_ADDRESS=ON -DENABLE_SANITIZER_UNDEFINED_BEHAVIOR=ON"
  fi
fi

# Coverage flags
COVERAGE_FLAGS=""
if [[ "$WITH_COVERAGE" == "ON" ]]; then
  if [[ "$BUILD_TYPE" == "Debug" ]]; then
    COVERAGE_FLAGS="-DENABLE_COVERAGE=ON"
  else
    echo "Error: Code coverage requires Debug build type."
    exit 1
  fi
fi

# Configure with CMake
echo "Configuring with CMake..."
cmake .. \
  -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
  -DBREZEL_BUILD_DOCS=$BUILD_DOCS \
  -DBREZEL_BUILD_EXAMPLES=$BUILD_EXAMPLES \
  -DBREZEL_BUILD_TESTS=$BUILD_TESTS \
  -DBREZEL_BUILD_BENCHMARKS=$BUILD_BENCHMARKS \
  $SANITIZER_FLAGS \
  $COVERAGE_FLAGS \
  $CMAKE_OPTIONS

# Build
echo "Building with $PARALLEL_JOBS parallel jobs..."
cmake --build . --parallel $PARALLEL_JOBS

# Run tests if enabled
if [[ "$BUILD_TESTS" == "ON" ]]; then
  echo "Running tests..."
  ctest --output-on-failure
fi

echo "Build completed successfully in directory: $BUILD_DIR"
