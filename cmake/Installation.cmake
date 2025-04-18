include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# Install the library header files
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)

# Export the target
install(
  TARGETS brezel
  EXPORT brezelTargets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

# Install export targets
install(
  EXPORT brezelTargets
  FILE brezelTargets.cmake
  NAMESPACE brezel::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/brezel
)

# Generate and install package configuration
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

# Install additional files
install(
  FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE
  ${CMAKE_CURRENT_SOURCE_DIR}/README.md
  DESTINATION ${CMAKE_INSTALL_DOCDIR}
)

# Create uninstall target
if(NOT TARGET uninstall)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY
  )

  add_custom_target(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake
  )
endif()