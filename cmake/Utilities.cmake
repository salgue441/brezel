# Utility functions for Brezel

# Function to check if a compiler flag is supported
function(check_cxx_compiler_flag_and_add flag target scope)
  include(CheckCXXCompilerFlag)
  string(REGEX REPLACE "[^A-Za-z0-9]" "_" flag_var "${flag}")
  set(flag_var "COMPILER_SUPPORTS_${flag_var}")
  check_cxx_compiler_flag("${flag}" "${flag_var}")

  if(${flag_var})
    target_compile_options(${target} ${scope} "${flag}")
  endif()
endfunction()

# Function to add include directories to a target
function(brezel_target_include_directories target)
  if(TARGET ${target})
    foreach(dir IN LISTS ARGN)
      target_include_directories(${target} PRIVATE ${dir})
    endforeach()
  endif()
endfunction()

# Function to join list elements with a delimiter
function(join_list result delimiter)
  set(items "")

  foreach(item IN LISTS ARGN)
    if(items)
      set(items "${items}${delimiter}${item}")
    else()
      set(items "${item}")
    endif()
  endforeach()

  set(${result} "${items}" PARENT_SCOPE)
endfunction()

# Function to print colored status messages
function(print_status message)
  if(NOT WIN32)
    string(ASCII 27 Esc)
    set(ColorReset "${Esc}[m")
    set(Green "${Esc}[32m")
    message(STATUS "${Green}${message}${ColorReset}")
  else()
    message(STATUS "${message}")
  endif()
endfunction()

# Function to print error messages
function(print_error message)
  if(NOT WIN32)
    string(ASCII 27 Esc)
    set(ColorReset "${Esc}[m")
    set(Red "${Esc}[31m")
    message(STATUS "${Red}${message}${ColorReset}")
  else()
    message(STATUS "Error: ${message}")
  endif()
endfunction()

# Function to add a test with additional setup
function(brezel_add_test test_name)
  if(NOT BREZEL_BUILD_TESTS)
    return()
  endif()

  cmake_parse_arguments(
    TEST # prefix
    "" # options
    "" # one-value keywords
    "SOURCES;DEPENDENCIES" # multi-value keywords
    ${ARGN}
  )

  add_executable(${test_name} ${TEST_SOURCES})

  target_link_libraries(${test_name} PRIVATE
    brezel::brezel
    Catch2::Catch2WithMain
    ${TEST_DEPENDENCIES}
  )

  add_test(NAME ${test_name} COMMAND ${test_name})

  set_target_properties(${test_name} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/tests"
  )

  if(NOT TARGET build_tests)
    add_custom_target(build_tests)
  endif()

  add_dependencies(build_tests ${test_name})
endfunction()

# Function to add an example
function(brezel_add_example example_name)
  if(NOT BREZEL_BUILD_EXAMPLES)
    return()
  endif()

  cmake_parse_arguments(
    EXAMPLE # prefix
    "" # options
    "" # one-value keywords
    "SOURCES;DEPENDENCIES" # multi-value keywords
    ${ARGN}
  )

  add_executable(${example_name} ${EXAMPLE_SOURCES})

  target_link_libraries(${example_name} PRIVATE
    brezel::brezel
    ${EXAMPLE_DEPENDENCIES}
  )

  set_target_properties(${example_name} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/examples"
  )

  if(NOT TARGET build_examples)
    add_custom_target(build_examples)
  endif()

  add_dependencies(build_examples ${example_name})
endfunction()