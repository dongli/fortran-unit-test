program test_assert

  use unit_test
  
  type(test_suite_type) :: test_suite_approximate
  type(test_suite_type) :: test_suite_boolean
  type(test_suite_type) :: test_suite_equal
  type(test_suite_type) :: test_suite_great_than
  
  ! assert_approximate
  call test_case_create('Test Suite: approximate', test_suite_approximate)
  
  ! -> real4
  call assert_approximate(1.0, 2.0, suite = test_suite_approximate)
  call assert_approximate(1.0, 1.0, suite = test_suite_approximate)
  call assert_approximate(1.0, 1.10, 0.1, suite = test_suite_approximate)
  call assert_approximate(1.0, 1.01, 0.1, suite = test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.10], suite = test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.00000000001], suite = test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.10], 0.1, suite = test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.01], 0.1, suite = test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]), suite = test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.00000000001, 1.0, 1.0], [2, 2]), suite = test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]), 0.1, suite = test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.01, 1.0, 1.0], [2, 2]), 0.1, suite = test_suite_approximate)
  
  ! -> real8
  call assert_approximate(1.0D0, 1.1D0, suite = test_suite_approximate)
  call assert_approximate(1.0D0, 1.00000000001D0, suite = test_suite_approximate)
  call assert_approximate(1.0D0, 1.1D0, 0.1D0, suite = test_suite_approximate)
  call assert_approximate(1.0D0, 1.00000000001D0, 0.1D0, suite = test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], suite = test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.00000000001D0], suite = test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], 0.1D0, suite = test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.01D0], 0.1D0, suite = test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]), suite = test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.000000000001D0, 1.0D0, 1.0D0], [2, 2]), suite = test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]), 0.1D0, suite = test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.01D0, 1.0D0, 1.0D0], [2, 2]), 0.1D0, suite = test_suite_approximate)
  
  call test_case_report('Test Suite: approximate', test_suite_approximate)
  call test_case_final(test_suite_approximate)
  
  ! assert_false/_true
  call test_case_create('Test Suite: boolean', test_suite_boolean)
  
  call assert_false(.true., suite = test_suite_boolean)
  call assert_false(.false., suite = test_suite_boolean)
  
  call assert_true(.false., suite = test_suite_boolean)
  call assert_true(.true., suite = test_suite_boolean)
  
  call test_case_report('Test Suite: boolean', test_suite_boolean)
  call test_case_final(test_suite_boolean)
  
  ! assert_equal
  call test_case_create('Test Suite: equal', test_suite_equal)
  
  ! -> string
  call assert_equal('abc', 'abcd', suite = test_suite_equal)
  call assert_equal('abc', 'abc', suite = test_suite_equal)
  call assert_equal(['abc', 'abc'], ['abc', 'abd'], suite = test_suite_equal)
  call assert_equal(['abc', 'abc'], ['abc', 'abc'], suite = test_suite_equal)
  
  ! -> integer
  call assert_equal(1, 2, suite = test_suite_equal)
  call assert_equal(3, 3, suite = test_suite_equal)
  call assert_equal([1, 1, 1, 1], [1, 2, 1, 1], suite = test_suite_equal)
  call assert_equal([1, 1, 1, 1], [1, 1, 1, 1], suite = test_suite_equal)
  call assert_equal(reshape([1, 1, 1, 1], [2, 2]), reshape([1, 2, 1, 1], [2, 2]), suite = test_suite_equal)
  call assert_equal(reshape([1, 1, 1, 1], [2, 2]), reshape([1, 1, 1, 1], [2, 2]), suite = test_suite_equal)
  
  ! -> real4
  call assert_equal(1.0, 2.0, suite = test_suite_equal)
  call assert_equal(3.0, 3.0, suite = test_suite_equal)
  call assert_equal([1.0, 1.0], [1.0, 2.0], suite = test_suite_equal)
  call assert_equal([1.0, 1.0], [1.0, 1.0], suite = test_suite_equal)
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 2.0, 1.0, 1.0], [2, 2]), suite = test_suite_equal)
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), suite = test_suite_equal)
  
  ! -> real8
  call assert_equal(1.0D0, 2.0D0, suite = test_suite_equal)
  call assert_equal(3.0D0, 3.0D0, suite = test_suite_equal)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 2.0D0], suite = test_suite_equal)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 1.0D0], suite = test_suite_equal)
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 2.0D0, 1.0D0, 1.0D0], [2, 2]), suite = test_suite_equal)
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), suite = test_suite_equal)
  
  call test_case_report('Test Suite: equal', test_suite_equal)
  call test_case_final(test_suite_equal)

  ! assert_great_than
  call test_case_create('Test Suite: great_then', test_suite_great_than)
  
  ! -> integer
  call assert_great_than(1, 1, suite = test_suite_great_than)
  call assert_great_than(2, 1, suite = test_suite_great_than)
  
  ! -> real4
  call assert_great_than(1.0, 1.0, suite = test_suite_great_than)
  call assert_great_than(2.0, 1.0, suite = test_suite_great_than)
  
  ! -> real8
  call assert_great_than(1.0D0, 1.0D0, suite = test_suite_great_than)
  call assert_great_than(2.0D0, 1.0D0, suite = test_suite_great_than)
  
  call test_case_report('Test Suite: great_then', test_suite_great_than)
  call test_case_final(test_suite_great_than)

end program test_assert
