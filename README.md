<a name="top"></a>
# Fortran Unit Test Library FUT

### Content

+ [Overview](#overview)

+ [Installation](#installation)

+ [Example](#example)

+ [Supported Data Types](#supported-data-types)

+ [Compiler Support](#compiler-support)

+ [License](#license)

### Overview
This is a Fortran Unit Test library purely written in Fortran to encourage scientific programmer to write tests.

<sub>Go to [Top](#top)</sub>

### Installation
A CMake-Setup is provided.

<sub>Go to [Top](#top)</sub>

### Example

demo.F90:
```fortran
program good_test

  use unit_test

  implicit none
  
  type(test_suite_type)  specific_suite
  
  ! example with default suite
  call test_case_init()

  call test_case_create('Test 1')

  ! By sending macros __FILE__ and __LINE__, report will print the file and line number where assertion fails.
  call assert_approximate(1.0, 2.0, __FILE__, __LINE__) ! line 14

  call test_suite_report()
  call test_case_final()
  
  ! example with specific suite
  specific_suite%name = 'my specific test suite'
  call test_case_create('Specific Test 1', specific_suite)
  ! suite = SUITE need in this case (cause optional argument eps, file_name, line_number is missing)
  call assert_approximate(1.0, 2.0, suite=specific_suite)
  
  call test_case_create('Specific Test 2', specific_suite)
  ! suite = SUITE need in this case (cause optional argument eps is missing)
  call assert_equal(1.0, 2.0, __FILE__, __LINE__,  suite=specific_suite)
  
  call test_case_create('Specific Test 3', specific_suite)
  call assert_approximate(1.0, 2.0, __FILE__, __LINE__, 1E-0, specific_suite)
  
  ! report a test_case
  call test_case_report('Specific Test 2', specific_suite)
  
  ! report the complete suite
  call test_suite_report(specific_suite)
  call test_case_final(specific_suite)

end program good_test
```

Output:
```txt
///////////////////// Report of Suite: Default test suite ///////////////////////

 +-> Details:
 |   |
 |   +-> Test 1: 1 of 1 assertions succeed.
 |   |
 |
 +-> Summary:
 |   +-> Default test suite: 1 of 1 assertions succeed.

////////////////////////////////////////////////////////////////////////////////


//////// Report of Suite: my specific test suite, Case: Specific Test 2 /////////

 +-> Specific Test 2: 0 of 1 assertions succeed.
 |   |
 |   +-> Assertion #1 failed with reason: x ( 1.000) == y ( 2.000)
 |   +-> Check line: test_assert.F90:29

/////////////////// Report of Suite: my specific test suite /////////////////////

 +-> Details:
 |   |
 |   +-> Specific Test 1: 1 of 1 assertions succeed.
 |   |
 |   +-> Specific Test 2: 0 of 1 assertions succeed.
 |   |   |
 |   |   +-> Assertion #1 failed with reason: x ( 1.000) == y ( 2.000)
 |   |   +-> Check line: test_assert.F90:29
 |   |
 |   +-> Specific Test 3: 0 of 1 assertions succeed.
 |   |   |
 |   |   +-> Assertion #1 failed with reason: x ( 1.000) =~ y ( 2.000)
 |   |   +-> Check line: test_assert.F90:32
 |   |
 |
 +-> Summary:
 |   +-> my specific test suite: 1 of 3 assertions succeed.

////////////////////////////////////////////////////////////////////////////////

```

You can integrate this library into your CMake based project as:

```
...
add_subdirectory (<fortran-unit-test root>)
include_directories (${UNIT_TEST_INCLUDE_DIR})
...
target_link_libraries (<user target> fortran_unit_test ...)
```

<sub>Go to [Top](#top)</sub>

### Supported Data Types
+ assert_equal()
  + single data type
    * [x] `integer(1)`;
    * [x] `integer(2)`;
    * [x] `integer(4)`;
    * [x] `integer(8)`;
    * [x] `real(4)`;
    * [x] `real(8)`;
    * [x] `character(*)`;
  + vector data type
    * [x] `integer(1), dimension(:)`;
    * [x] `integer(2), dimension(:)`;
    * [x] `integer(4), dimension(:)`;
    * [x] `integer(8), dimension(:)`;
    * [x] `real(4), dimension(:)`;
    * [x] `real(8), dimension(:)`;
    * [x] `character(*), dimension(:)`;
  + array data type
    * [x] `integer(1), dimension(:, :)`;
    * [x] `integer(2), dimension(:, :)`;
    * [x] `integer(4), dimension(:, :)`;
    * [x] `integer(8), dimension(:, :)`;
    * [x] `real(4), dimension(:, :)`;
    * [x] `real(8), dimension(:, :)`;
    * [x] `character(*), dimension(:, :)`;
+ assert_approximate()
  + single data type
    * [x] `real(4)`;
    * [x] `real(8)`;
  + vector data type
    * [x] `real(4), dimension(:)`;
    * [x] `real(8), dimension(:)`;
  + array data type
    * [x] `real(4), dimension(:, :)`;
    * [x] `real(8), dimension(:, :)`;
+ assert_great_then()
  + single data type
    * [x] `integer(1)`;
    * [x] `integer(2)`;
    * [x] `integer(4)`;
    * [x] `integer(8)`;
    * [x] `real(4)`;
    * [x] `real(8)`;
  + vector data type
    * [x] `integer(1), dimension(:)`;
    * [x] `integer(2), dimension(:)`;
    * [x] `integer(4), dimension(:)`;
    * [x] `integer(8), dimension(:)`;
    * [x] `real(4), dimension(:)`;
    * [x] `real(8), dimension(:)`;
  + array data type
    * [x] `integer(1), dimension(:, :)`;
    * [x] `integer(2), dimension(:, :)`;
    * [x] `integer(4), dimension(:, :)`;
    * [x] `integer(8), dimension(:, :)`;
    * [x] `real(4), dimension(:, :)`;
    * [x] `real(8), dimension(:, :)`;

<sub>Go to [Top](#top)</sub>

### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v4.8.5+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-v18.4+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v17.0.2.187+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()

<sub>Go to [Top](#top)</sub>

### License
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg)]()

<sub>Go to [Top](#top)</sub>
