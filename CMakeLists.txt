cmake_minimum_required (VERSION 2.8)

set (CMAKE_MACOSX_RPATH ON)

project (FortranUnitTest Fortran)

set (version 0.0.1)
 
set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")

set (sources
  "${PROJECT_SOURCE_DIR}/src/string_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/log_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/assert_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/test_case_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/unit_test.F90"
)

add_library (fortran_unit_test SHARED ${sources})
set_property (TARGET fortran_unit_test PROPERTY VERSION ${version})
set_property (TARGET fortran_unit_test PROPERTY SOVERSION 1)

add_executable (test_log "${PROJECT_SOURCE_DIR}/src/test/test_log.F90")
target_link_libraries (test_log fortran_unit_test)

add_executable (test_assert "${PROJECT_SOURCE_DIR}/src/test/test_assert.F90")
target_link_libraries (test_assert fortran_unit_test)

install (TARGETS fortran_unit_test EXPORT FortranUnitTestTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

file (GLOB mods "${CMAKE_BINARY_DIR}/*.mod")
install (FILES ${mods} DESTINATION include/fortran_unit_test)

include (CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranUnitTest/FortranUnitTestConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export (EXPORT FortranUnitTestTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranUnitTest/FortranUnitTestTargets.cmake"
)
configure_file (cmake/FortranUnitTestConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/FortranUnitTest/FortranUnitTestConfig.cmake"
)

set (ConfigPackageLocation lib/cmake/FortranUnitTest)
install (EXPORT FortranUnitTestTargets
  FILE FortranUnitTestTargets.cmake
  DESTINATION ${ConfigPackageLocation}
)
install (
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/FortranUnitTest/FortranUnitTestConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/FortranUnitTest/FortranUnitTestConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
