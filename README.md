Introduction
============

This is a Fortran Unit Test library to encourage scientific programmer to write tests!

Current example:

```
program good_test

  use unit_test

  implicit none

  call test_case_init()

  call test_case_create('Test 1')

  call assert_approximate(1.0, 2.0)

  call test_cast_final()

end program good_test
```

Output:

```

 //////////////////////////////////// ERROR ////////////////////////////////////

 Test case: Test 1:

 Reason: x (1.0000) =~ y (2.0000) failed!

STOP 1
```
