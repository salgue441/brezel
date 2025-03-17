# Configure package generation with CPack
include(InstallRequiredSystemLibraries)
include(CPack)

# Get information for package generation
set(CPACK_PACKAGE_NAME "brezel")
set(CPACK_PACKAGE_VENDOR "Your Organization")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Brezel - A Modern C++ Tensor Framework")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(CPACK_PACKAGE_INSTALL_DIRECTORY "brezel-${PROJECT_VERSION}")
set(CPACK_PACKAGE_CONTACT "your.email@example.com")

# Source package settings
set(CPACK_SOURCE_GENERATOR "TGZ;ZIP")
set(CPACK_SOURCE_IGNORE_FILES
  "\\\\.git/"
  "\\\\.github/"
  "\\\\.vscode/"
  "/build.*/"
  "/\\\\.cache/"
  "/\\\\._*"
  "\\\\.DS_Store"
  "\\\\.idea/"
  "CMakeCache\\\\.txt"
  "CMakeFiles/"
  "\\\\.env"
  "cmake-build-.*/"
)

# Binary package settings
if(WIN32)
  set(CPACK_GENERATOR "NSIS;ZIP")
  set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
  set(CPACK_NSIS_PACKAGE_NAME "Brezel ${PROJECT_VERSION}")
  set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)
  set(CPACK_NSIS_MODIFY_PATH ON)
  set(CPACK_NSIS_DISPLAY_NAME "Brezel Tensor Framework")
  set(CPACK_NSIS_HELP_LINK "https://github.com/salgue441/brezel")
  set(CPACK_NSIS_URL_INFO_ABOUT "https://github.com/salgue441/brezel")
elseif(APPLE)
  set(CPACK_GENERATOR "DragNDrop;TGZ")
  set(CPACK_DMG_VOLUME_NAME "Brezel ${PROJECT_VERSION}")
  set(CPACK_DMG_FORMAT "UDBZ") # More aggressive compression
else() # Linux
  set(CPACK_GENERATOR "TGZ;DEB;RPM")

  # DEB-specific
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Your Name")
  set(CPACK_DEBIAN_PACKAGE_SECTION "devel")
  set(CPACK_DEBIAN_PACKAGE_DEPENDS "libfmt-dev, libblas-dev, liblapack-dev, libstdc++-dev")

  # RPM-specific
  set(CPACK_RPM_PACKAGE_RELEASE "1")
  set(CPACK_RPM_PACKAGE_LICENSE "MIT")
  set(CPACK_RPM_PACKAGE_GROUP "Development/Libraries")
  set(CPACK_RPM_PACKAGE_REQUIRES "fmt-devel, blas-devel, lapack-devel, gcc-c++")
endif()

# Docker packaging
find_program(DOCKER_EXECUTABLE docker)

if(DOCKER_EXECUTABLE)
  add_custom_target(package-docker
    COMMAND ${DOCKER_EXECUTABLE} build -t brezel:${PROJECT_VERSION} .
    COMMAND ${DOCKER_EXECUTABLE} save brezel:${PROJECT_VERSION} | gzip > ${CMAKE_BINARY_DIR}/brezel-${PROJECT_VERSION}.docker.tar.gz
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Creating Docker image package..."
    VERBATIM
  )
endif()

# Add a custom target to generate packages
add_custom_target(package-all
  COMMAND ${CMAKE_COMMAND} --build . --target package
  COMMAND ${CMAKE_COMMAND} --build . --target package_source
  DEPENDS brezel
  COMMENT "Generating all packages..."
  VERBATIM
)