program test_assert

  use unit_test
  
  type(test_suite_type) :: test_suite_approximate
  type(test_suite_type) :: test_suite_boolean
  type(test_suite_type) :: test_suite_equal
  type(test_suite_type) :: test_suite_great_than
  
  ! assert_approximate  
  test_suite_approximate%name = 'Approximate'
  
  ! -> real4
  call test_case_create('real4', test_suite_approximate)
  call assert_approximate(1.0, 2.0, suite=test_suite_approximate)
  call assert_approximate(1.0, 1.0, suite=test_suite_approximate)
  call assert_approximate(1.0, 1.10, eps=0.1, suite=test_suite_approximate)
  call assert_approximate(1.0, 1.01, eps=0.1, suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.10], suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.00000000001], suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.10], eps=0.1, suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.01], eps=0.1, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.00000000001, 1.0, 1.0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]), eps=0.1, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.01, 1.0, 1.0], [2, 2]), eps=0.1, suite=test_suite_approximate)
  
  ! -> real8
  call test_case_create('real8', test_suite_approximate)
  call assert_approximate(1.0D0, 1.1D0, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.00000000001D0, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.1D0, eps=0.1D0, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.00000000001D0, eps=0.1D0, suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.00000000001D0], suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], eps=0.1D0, suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.01D0], eps=0.1D0, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.000000000001D0, 1.0D0, 1.0D0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]), eps=0.1D0, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.01D0, 1.0D0, 1.0D0], [2, 2]), eps=0.1D0, suite=test_suite_approximate)
  
  ! -> report
  call test_suite_report(test_suite_approximate)
  call test_case_final(test_suite_approximate)
  
  ! assert_false/_true
  test_suite_boolean%name = 'Boolean'
  
  ! -> false
  call test_case_create('false', test_suite_boolean)
  call assert_false(.true., suite=test_suite_boolean)
  call assert_false(.false., suite=test_suite_boolean)
  
  ! -> true
  call test_case_create('true', test_suite_boolean)
  call assert_true(.false., suite=test_suite_boolean)
  call assert_true(.true., suite=test_suite_boolean)
  
  ! report
  call test_suite_report(test_suite_boolean)
  call test_case_final(test_suite_boolean)
  
  ! assert_equal  
  test_suite_equal%name = 'Equal'
  
  ! -> string
  call test_case_create('string', test_suite_equal)
  call assert_equal('abc', 'abcd', suite=test_suite_equal)
  call assert_equal('abc', 'abc', suite=test_suite_equal)
  call assert_equal(['abc', 'abc'], ['abc', 'abd'], suite=test_suite_equal)
  call assert_equal(['abc', 'abc'], ['abc', 'abc'], suite=test_suite_equal)
  
  ! -> integer
  call test_case_create('integer', test_suite_equal)
  call assert_equal(1, 2, suite=test_suite_equal)
  call assert_equal(3, 3, suite=test_suite_equal)
  call assert_equal([1, 1, 1, 1], [1, 2, 1, 1], suite=test_suite_equal)
  call assert_equal([1, 1, 1, 1], [1, 1, 1, 1], suite=test_suite_equal)
  call assert_equal(reshape([1, 1, 1, 1], [2, 2]), reshape([1, 2, 1, 1], [2, 2]), suite=test_suite_equal)
  call assert_equal(reshape([1, 1, 1, 1], [2, 2]), reshape([1, 1, 1, 1], [2, 2]), suite=test_suite_equal)
  
  ! -> real4
  call test_case_create('real4', test_suite_equal)
  call assert_equal(1.0, 2.0, suite=test_suite_equal)
  call assert_equal(3.0, 3.0, suite=test_suite_equal)
  call assert_equal([1.0, 1.0], [1.0, 2.0], suite=test_suite_equal)
  call assert_equal([1.0, 1.0], [1.0, 1.0], suite=test_suite_equal)
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 2.0, 1.0, 1.0], [2, 2]), suite=test_suite_equal)
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), suite=test_suite_equal)
  
  ! -> real8
  call test_case_create('real8', test_suite_equal)
  call assert_equal(1.0D0, 2.0D0, suite=test_suite_equal)
  call assert_equal(3.0D0, 3.0D0, suite=test_suite_equal)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 2.0D0], suite=test_suite_equal)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 1.0D0], suite=test_suite_equal)
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 2.0D0, 1.0D0, 1.0D0], [2, 2]), suite=test_suite_equal)
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), suite=test_suite_equal)
  
  call test_suite_report(test_suite_equal)
  call test_case_final(test_suite_equal)

  ! assert_great_than
  test_suite_great_than%name = 'great_then'
  
  ! -> integer
  call test_case_create('integer', test_suite_great_than)
  call assert_great_than(1, 1, suite=test_suite_great_than)
  call assert_great_than(2, 1, suite=test_suite_great_than)
  
  ! -> real4
  call test_case_create('real4', test_suite_great_than)
  call assert_great_than(1.0, 1.0, suite=test_suite_great_than)
  call assert_great_than(2.0, 1.0, suite=test_suite_great_than)
  
  ! -> real8
  call test_case_create('real8', test_suite_great_than)
  call assert_great_than(1.0D0, 1.0D0, __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(2.0D0, 1.0D0, __FILE__, __LINE__, test_suite_great_than)
  
  ! report
  call test_suite_report(test_suite_great_than)
  call test_case_final(test_suite_great_than)

end program test_assert
