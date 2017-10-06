program test_assert

  use unit_test

  call test_case_init()

  call test_case_create('Test 1')

  call assert_approximate(1.0, 2.0)

  call test_case_final()

end program test_assert
