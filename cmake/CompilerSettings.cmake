# Set compiler flags and warnings
include(CheckCXXCompilerFlag)

# Determine the compiler
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  set(BREZEL_COMPILER_GCC TRUE)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(BREZEL_COMPILER_CLANG TRUE)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
  set(BREZEL_COMPILER_MSVC TRUE)
endif()

# Function to add compiler warnings - to be called after target is created
function(brezel_set_warnings target)
  if(NOT TARGET ${target})
    message(FATAL_ERROR "brezel_set_warnings: '${target}' is not a valid target.")
    return()
  endif()

  set(MSVC_WARNINGS
    /W4 # Baseline reasonable warnings
    /w14242 # Conversion type warnings
    /w14254 # Larger type to smaller type conversion
    /w14263 # Non-override of virtual function
    /w14265 # Class has virtual functions but destructor is not virtual
    /w14287 # Unsigned/negative constant mismatch
    /permissive- # Enforce standards conformance
  )

  set(CLANG_GCC_WARNINGS
    -Wall
    -Wextra
    -Wshadow
    -Wnon-virtual-dtor
    -Wold-style-cast
    -Wcast-align
    -Wunused
    -Woverloaded-virtual
    -Wpedantic
    -Wconversion
    -Wsign-conversion
    -Wnull-dereference
    -Wdouble-promotion
    -Wformat=2
  )

  if(BREZEL_COMPILER_MSVC)
    target_compile_options(${target} INTERFACE ${MSVC_WARNINGS})
  elseif(BREZEL_COMPILER_CLANG OR BREZEL_COMPILER_GCC)
    target_compile_options(${target} INTERFACE ${CLANG_GCC_WARNINGS})
  endif()

  if(BREZEL_WARNINGS_AS_ERRORS)
    if(BREZEL_COMPILER_MSVC)
      target_compile_options(${target} INTERFACE /WX)
    else()
      target_compile_options(${target} INTERFACE -Werror)
    endif()
  endif()
endfunction()

# Function to set up sanitizers
function(brezel_set_sanitizers target)
  if(NOT TARGET ${target})
    message(FATAL_ERROR "brezel_set_sanitizers: '${target}' is not a valid target.")
    return()
  endif()

  if(BREZEL_COMPILER_MSVC)
    # MSVC doesn't support most sanitizers directly
    return()
  endif()

  set(SANITIZERS "")

  if(BREZEL_ENABLE_SANITIZER_ADDRESS)
    list(APPEND SANITIZERS "address")
  endif()

  if(BREZEL_ENABLE_SANITIZER_UNDEFINED)
    list(APPEND SANITIZERS "undefined")
  endif()

  if(BREZEL_ENABLE_SANITIZER_THREAD)
    if("address" IN_LIST SANITIZERS)
      message(WARNING "Thread sanitizer does not work with Address sanitizer - disabling")
    else()
      list(APPEND SANITIZERS "thread")
    endif()
  endif()

  list(JOIN SANITIZERS "," LIST_OF_SANITIZERS)

  if(LIST_OF_SANITIZERS)
    if(NOT "${LIST_OF_SANITIZERS}" STREQUAL "")
      target_compile_options(${target} INTERFACE -fsanitize=${LIST_OF_SANITIZERS})
      target_link_options(${target} INTERFACE -fsanitize=${LIST_OF_SANITIZERS})
    endif()
  endif()
endfunction()

# Set up compiler flags for the target
function(brezel_setup_target target)
  if(NOT TARGET ${target})
    message(FATAL_ERROR "brezel_setup_target: '${target}' is not a valid target.")
    return()
  endif()

  # Add warnings
  brezel_set_warnings(${target})

  # Add sanitizers
  brezel_set_sanitizers(${target})

  # Optimization flags for release builds
  if(CMAKE_BUILD_TYPE STREQUAL "Release")
    if(BREZEL_COMPILER_MSVC)
      target_compile_options(${target} INTERFACE
        /O2 # Full optimization
        /Ob3 # Aggressive inlining
      )
    else()
      target_compile_options(${target} INTERFACE
        -O3 # Full optimization
        -march=native # Optimize for host architecture
      )

      # Link-time optimization
      check_cxx_compiler_flag(-flto COMPILER_SUPPORTS_LTO)

      if(COMPILER_SUPPORTS_LTO)
        target_compile_options(${target} INTERFACE -flto)
        target_link_options(${target} INTERFACE -flto)
      endif()
    endif()
  endif()

  # Set up platform-specific defines
  if(WIN32)
    target_compile_definitions(${target} INTERFACE BREZEL_PLATFORM_WINDOWS=1)
  elseif(UNIX AND NOT APPLE)
    target_compile_definitions(${target} INTERFACE BREZEL_PLATFORM_LINUX=1)
  elseif(APPLE)
    target_compile_definitions(${target} INTERFACE BREZEL_PLATFORM_MACOS=1)
  endif()

  # Set up debug/release defines
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_definitions(${target} INTERFACE BREZEL_DEBUG=1)
  else()
    target_compile_definitions(${target} INTERFACE BREZEL_DEBUG=0)
  endif()

  # Compiler-specific defines
  if(BREZEL_COMPILER_MSVC)
    target_compile_definitions(${target} INTERFACE BREZEL_COMPILER_MSVC=1)
  elseif(BREZEL_COMPILER_CLANG)
    target_compile_definitions(${target} INTERFACE BREZEL_COMPILER_CLANG=1)
  elseif(BREZEL_COMPILER_GCC)
    target_compile_definitions(${target} INTERFACE BREZEL_COMPILER_GCC=1)
  endif()
endfunction()
