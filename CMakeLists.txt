cmake_minimum_required (VERSION 2.8)

set (CMAKE_MACOSX_RPATH ON)

project (fut Fortran)
 
set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")

set (src
  "${PROJECT_SOURCE_DIR}/src/string_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/log_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/assert_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/test_case_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/unit_test.F90"
)

add_library (fut SHARED ${src})

add_executable (test_log "${PROJECT_SOURCE_DIR}/src/test/test_log.F90")
target_link_libraries (test_log fut)

add_executable (test_assert "${PROJECT_SOURCE_DIR}/src/test/test_assert.F90")
target_link_libraries (test_assert fut)

install (TARGETS fut
  ARCHIVE
    DESTINATION lib
  LIBRARY
    DESTINATION lib
  PUBLIC_HEADER
    DESTINATION include
)
