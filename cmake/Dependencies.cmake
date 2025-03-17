include(FetchContent)

# Set the default for FetchContent to be quiet
set(FETCHCONTENT_QUIET OFF)

# Option for using system-installed dependencies vs. fetching them
option(BREZEL_USE_SYSTEM_DEPS "Use system-installed dependencies if available" ON)

# Function to find a package or fetch it if not found
function(find_package_or_fetch PKG_NAME GIT_REPO GIT_TAG)
  if(BREZEL_USE_SYSTEM_DEPS)
    find_package(${PKG_NAME} QUIET)
  endif()

  if(NOT ${PKG_NAME}_FOUND)
    message(STATUS "Fetching ${PKG_NAME} from ${GIT_REPO}")
    FetchContent_Declare(
      ${PKG_NAME}
      GIT_REPOSITORY ${GIT_REPO}
      GIT_TAG ${GIT_TAG}
    )
    FetchContent_MakeAvailable(${PKG_NAME})
  else()
    message(STATUS "Using system ${PKG_NAME}")
  endif()
endfunction()

# fmt library for string formatting
find_package_or_fetch(
  fmt
  https://github.com/fmtlib/fmt.git
  9.1.0
)

# Catch2 for testing (only if building tests)
if(BREZEL_BUILD_TESTS)
  find_package_or_fetch(
    Catch2
    https://github.com/catchorg/Catch2.git
    v3.4.0
  )
endif()

# Eigen (optional)
option(BREZEL_USE_EIGEN "Use Eigen for linear algebra operations" OFF)

if(BREZEL_USE_EIGEN)
  find_package_or_fetch(
    Eigen3
    https://gitlab.com/libeigen/eigen.git
    3.4.0
  )

  # We'll set the compile definition in the main CMakeLists.txt after the target is created
  set(BREZEL_WITH_EIGEN ON CACHE INTERNAL "Using Eigen for linear algebra")
endif()

# spdlog for logging (optional but recommended)
option(BREZEL_USE_SPDLOG "Use spdlog for logging" ON)

if(BREZEL_USE_SPDLOG)
  find_package_or_fetch(
    spdlog
    https://github.com/gabime/spdlog.git
    v1.11.0
  )

  # We'll set the compile definition in the main CMakeLists.txt after the target is created
  set(BREZEL_WITH_SPDLOG ON CACHE INTERNAL "Using spdlog for logging")
endif()