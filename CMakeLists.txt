cmake_minimum_required (VERSION 2.8)

set (CMAKE_MACOSX_RPATH ON)

project (FortranUnitTest Fortran)

set (version 0.0.1)
 
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
endif ()
set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/fortran_unit_test")

set (sources
  src/string_mod.F90
  src/log_mod.F90
  src/assert_mod.F90
  src/test_case_mod.F90
  src/unit_test.F90
)

set (UNIT_TEST_INCLUDE_DIR "${CMAKE_BINARY_DIR}/fortran_unit_test" PARENT_SCOPE)

add_library (fortran_unit_test SHARED ${sources})
set_property (TARGET fortran_unit_test PROPERTY VERSION ${version})
set_property (TARGET fortran_unit_test PROPERTY SOVERSION 1)

add_executable (test_log src/test/test_log.F90)
target_link_libraries (test_log fortran_unit_test)

add_executable (test_assert src/test/test_assert.F90)
target_link_libraries (test_assert fortran_unit_test)

install (TARGETS fortran_unit_test EXPORT FortranUnitTestTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

install (DIRECTORY "${CMAKE_BINARY_DIR}/fortran_unit_test" DESTINATION include)

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
