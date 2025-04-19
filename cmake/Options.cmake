# Build options
option(BREZEL_BUILD_TESTS "Build tests" ON)
option(BREZEL_BUILD_EXAMPLES "Build examples" ON)
option(BREZEL_BUILD_BENCHMARKS "Build benchmarks" OFF)
option(BREZEL_COMPILE_EXTENSIONS "Compile optional performance extensions" ON)
option(BREZEL_BUILD_DOCS "Build documentation" OFF)

# Feature options
option(BREZEL_USE_CUDA "Enable CUDA support" OFF)
option(BREZEL_USE_OPENMP "Enable OpenMP support" ON)
option(BREZEL_USE_BLAS "Enable BLAS/LAPACK support" ON)
option(BREZEL_USE_EIGEN "Use Eigen for linear algebra operations" ON)
option(BREZEL_USE_SPDLOG "Use spdlog for logging" ON)
option(BREZEL_ENABLE_PROFILING "Enable performance profiling" OFF)
option(BREZEL_WITH_PCH "Use precompiled headers to speed up compilation" ON)

# Developer options
option(BREZEL_WARNINGS_AS_ERRORS "Treat compiler warnings as errors" ON)
option(BREZEL_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" OFF)
option(BREZEL_ENABLE_SANITIZER_UNDEFINED "Enable undefined behavior sanitizer" OFF)
option(BREZEL_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
option(BREZEL_ENABLE_CLANG_TIDY "Enable clang-tidy" OFF)
option(BREZEL_ENABLE_CPPCHECK "Enable cppcheck" OFF)