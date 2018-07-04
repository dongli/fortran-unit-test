# Fortran Unit Test Library FUT

### Overview
This is a Fortran Unit Test library purely written in Fortran to encourage scientific programmer to write tests.

### Installation
A CMake-Setup is provided.

### Example

demo.F90:
```
program good_test

  use unit_test

  implicit none

  call test_case_init()

  call test_case_create('Test 1')

  ! By sending macros __FILE__ and __LINE__, report will print the file and line number where assertion fails.
  call assert_approximate(1.0, 2.0, __FILE__, __LINE__) ! line 14

  call test_case_final()

end program good_test
```

Output:
```
////////////// Report of Suite: Default test suite, Case: Test 1 ////////////////

 Test 1: 0 of 1 assertions succeed.

 Assertion #1 failed with reason: x (1.0000) =~ y (2.0000)

 Check line: /.../demo.F90:14
```

You can integrate this library into your CMake based project as:

```
...
add_subdirectory (<fortran-unit-test root>)
include_directories (${UNIT_TEST_INCLUDE_DIR})
...
target_link_libraries (<user target> fortran_unit_test ...)
```

### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v4.8.5+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-v18.4-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v17.0.2.187+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()

### License
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg)]()

Go to [Top](#top)
