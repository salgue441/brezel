
####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was brezelConfig.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

macro(check_required_components _NAME)
  foreach(comp ${${_NAME}_FIND_COMPONENTS})
    if(NOT ${_NAME}_${comp}_FOUND)
      if(${_NAME}_FIND_REQUIRED_${comp})
        set(${_NAME}_FOUND FALSE)
      endif()
    endif()
  endforeach()
endmacro()

####################################################################################

include(CMakeFindDependencyMacro)

# Find dependencies
find_dependency(fmt REQUIRED)
find_dependency(Threads REQUIRED)

# Find optional dependencies based on how brezel was built
if(ON)
    find_dependency(OpenMP REQUIRED)
endif()

if(ON)
    find_dependency(BLAS REQUIRED)
    find_dependency(LAPACK REQUIRED)
endif()

if(OFF)
    find_dependency(CUDA REQUIRED)
endif()

if(OFF)
    find_dependency(Eigen3 REQUIRED)
endif()

if(ON)
    find_dependency(spdlog REQUIRED)
endif()

# Include the targets file
include("${CMAKE_CURRENT_LIST_DIR}/brezelTargets.cmake")

# Set the version variables
set(brezel_VERSION 0.1.0)
set(brezel_VERSION_MAJOR 0)
set(brezel_VERSION_MINOR 1)
set(brezel_VERSION_PATCH 0)

# Check if components are requested
foreach(comp ${brezel_FIND_COMPONENTS})
    if(NOT ";;" MATCHES ";${comp};")
        set(brezel_FOUND FALSE)
        set(brezel_NOT_FOUND_MESSAGE "Requested component '${comp}' is not available in brezel.")
    endif()
endforeach()

# Report version and found status
if(NOT brezel_FIND_QUIETLY)
    message(STATUS "Found brezel: ${brezel_VERSION}")
endif()

check_required_components(brezel)
