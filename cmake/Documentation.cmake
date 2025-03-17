# Documentation-related CMake functions and macros

# Set up documentation options
option(BREZEL_BUILD_DOCS "Build documentation" OFF)
option(BREZEL_DOCS_WITH_EXAMPLES "Include examples in documentation" ON)
option(BREZEL_DOCS_WITH_TESTS "Include tests in documentation" OFF)

# Find Doxygen
find_package(Doxygen QUIET)

# Function to set up documentation target
function(brezel_setup_documentation)
  if(NOT BREZEL_BUILD_DOCS)
    return()
  endif()

  if(NOT DOXYGEN_FOUND)
    message(WARNING "Doxygen not found. Documentation will not be built.")
    return()
  endif()

  # Set up the output directory for the documentation
  set(DOXYGEN_OUTPUT_DIR "${CMAKE_BINARY_DIR}/docs")

  # Ensure the output directory exists
  file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})

  # Additional input paths for documentation
  set(DOXYGEN_ADDITIONAL_INPUTS "")

  # If examples should be included
  if(BREZEL_DOCS_WITH_EXAMPLES)
    set(DOXYGEN_ADDITIONAL_INPUTS "${DOXYGEN_ADDITIONAL_INPUTS} \\\n                         ${CMAKE_SOURCE_DIR}/examples")
  endif()

  # If tests should be included
  if(BREZEL_DOCS_WITH_TESTS)
    set(DOXYGEN_ADDITIONAL_INPUTS "${DOXYGEN_ADDITIONAL_INPUTS} \\\n                         ${CMAKE_SOURCE_DIR}/tests")
  endif()

  # Create docs/images directory if it doesn't exist (for any images to be included)
  file(MAKE_DIRECTORY "${CMAKE_SOURCE_DIR}/docs/images")

  # Configure the Doxyfile from the template
  configure_file(
    "${CMAKE_SOURCE_DIR}/docs/Doxyfile.in"
    "${CMAKE_BINARY_DIR}/docs/Doxyfile"
    @ONLY
  )

  # Check if docs target already exists
  if(NOT TARGET docs)
    # Define the documentation target
    add_custom_target(docs
      COMMAND ${DOXYGEN_EXECUTABLE} "${CMAKE_BINARY_DIR}/docs/Doxyfile"
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      COMMENT "Generating API documentation with Doxygen"
      VERBATIM
    )
  endif()

  # Add a target to serve the documentation locally (if Python is available)
  find_package(Python3 QUIET COMPONENTS Interpreter)

  if(Python3_FOUND)
    add_custom_target(serve-docs
      COMMAND ${Python3_EXECUTABLE} -m http.server 8000
      WORKING_DIRECTORY "${DOXYGEN_OUTPUT_DIR}/html"
      COMMENT "Serving documentation at http://localhost:8000 (press Ctrl+C to stop)"
      DEPENDS docs
      VERBATIM
    )
  endif()

  # Generate documentation as part of the default build if requested
  if(BREZEL_DOCS_WITH_BUILD)
    add_dependencies(brezel docs)
  endif()

  # Installation of documentation
  if(NOT DEFINED ENV{CI} AND NOT CI)
    install(
      DIRECTORY "${DOXYGEN_OUTPUT_DIR}/html"
      DESTINATION ${CMAKE_INSTALL_DOCDIR}
      OPTIONAL
    )
  endif()

  message(STATUS "Documentation setup complete. Run 'cmake --build . --target docs' to generate.")
endfunction()