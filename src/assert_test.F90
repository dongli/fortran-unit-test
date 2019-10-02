program test_assert
  ! program to test assert routines
  ! uneven assertions fail and even assertions pass

  use unit_test

  type(test_suite_type) :: test_suite_approximate
  type(test_suite_type) :: test_suite_boolean
  type(test_suite_type) :: test_suite_equal
  type(test_suite_type) :: test_suite_great_than

  ! test assert_approximate routines
  call test_suite_init('Approximate', test_suite_approximate)

  ! -> real
  call test_case_create('real', test_suite_approximate)
  call assert_approximate(1.0, 2.0, suite=test_suite_approximate)
  call assert_approximate(1.0, 1.0, suite=test_suite_approximate)
  call assert_approximate(1.0, 1.10, eps=0.01, suite=test_suite_approximate)
  call assert_approximate(1.0, 1.01, eps=0.01, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.1D0, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.00000000001D0, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.1D0, eps=0.01D0, suite=test_suite_approximate)
  call assert_approximate(1.0D0, 1.00000000001D0, eps=0.01D0, suite=test_suite_approximate)

  call assert_approximate([1.0, 1.0], [1.0, 1.10], suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.00000000001], suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.10], eps=0.01, suite=test_suite_approximate)
  call assert_approximate([1.0, 1.0], [1.0, 1.01], eps=0.01, suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.00000000001D0], suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], eps=0.01D0, suite=test_suite_approximate)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.01D0], eps=0.01D0, suite=test_suite_approximate)

  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.00000000001, 1.0, 1.0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.10, 1.0, 1.0], [2, 2]), eps=0.01, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.01, 1.0, 1.0], [2, 2]), eps=0.01, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.000000000001D0, 1.0D0, 1.0D0], [2, 2]), suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.10D0, 1.0D0, 1.0D0], [2, 2]), eps=0.01D0, suite=test_suite_approximate)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.01D0, 1.0D0, 1.0D0], [2, 2]), eps=0.01D0, suite=test_suite_approximate)

  call test_suite_report(test_suite_approximate)
  call test_suite_final(test_suite_approximate)

  ! test assert_false/_true routines
  call test_suite_init('Boolean', test_suite_boolean)

  ! -> false
  call test_case_create('false', test_suite_boolean)
  call assert_false(.true., __FILE__, __LINE__, test_suite_boolean)
  call assert_false(.false., __FILE__, __LINE__, test_suite_boolean)

  ! -> true
  call test_case_create('true', test_suite_boolean)
  call assert_true(.false., __FILE__, __LINE__, test_suite_boolean)
  call assert_true(.true., __FILE__, __LINE__, test_suite_boolean)

  call test_suite_report(test_suite_boolean)
  call test_suite_final(test_suite_boolean)

  ! test assert_equal routines
  call test_suite_init('Equal', test_suite_equal)

  ! -> string
  call test_case_create('string', test_suite_equal)
  call assert_equal('abc', 'abcd', __FILE__, __LINE__, test_suite_equal)
  call assert_equal('abc', 'abc', __FILE__, __LINE__, test_suite_equal)

  call assert_equal(['abc', 'abc'], ['abc', 'abd'], __FILE__, __LINE__, test_suite_equal)
  call assert_equal(['abc', 'abc'], ['abc', 'abc'], __FILE__, __LINE__, test_suite_equal)

  call assert_equal(reshape(['abc', 'abc', 'abc', 'abc'], [2, 2]), reshape(['abc', 'abd', 'abc', 'abd'], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape(['abc', 'abc', 'abc', 'abc'], [2, 2]), reshape(['abc', 'abc', 'abc', 'abc'], [2, 2]), __FILE__, __LINE__, test_suite_equal)

  ! -> integer (int8->kind=1, int16->kind=2, int32->kind=4, int64->kind=8)
  call test_case_create('integer', test_suite_equal)
  call assert_equal(int(1, 1), int(2, 1), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(3, 1), int(3, 1), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(1, 2), int(2, 2), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(3, 2), int(3, 2), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(1, 4), int(2, 4), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(3, 4), int(3, 4), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(1, 8), int(2, 8), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(int(3, 8), int(3, 8), __FILE__, __LINE__, test_suite_equal)

  call assert_equal([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [int(1, 1), int(2, 1), int(1, 1), int(1, 1)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [int(1, 1), int(1, 1), int(1, 1), int(1, 1)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [int(1, 2), int(2, 2), int(1, 2), int(1, 2)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [int(1, 2), int(1, 2), int(1, 2), int(1, 2)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [int(1, 4), int(2, 4), int(1, 4), int(1, 4)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [int(1, 4), int(1, 4), int(1, 4), int(1, 4)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [int(1, 8), int(2, 8), int(1, 8), int(1, 8)], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [int(1, 8), int(1, 8), int(1, 8), int(1, 8)], __FILE__, __LINE__, test_suite_equal)

  call assert_equal(reshape([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [2, 2]), &
      & reshape([int(1, 1), int(2, 1), int(1, 1), int(1, 1)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [2, 2]), &
      & reshape([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [2, 2]), &
      & reshape([int(1, 2), int(2, 2), int(1, 2), int(1, 2)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [2, 2]), &
      & reshape([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [2, 2]), &
      & reshape([int(1, 4), int(2, 4), int(1, 4), int(1, 4)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [2, 2]), &
      & reshape([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [2, 2]), &
      & reshape([int(1, 8), int(2, 8), int(1, 8), int(1, 8)], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [2, 2]), &
      & reshape([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [2, 2]), __FILE__, __LINE__, test_suite_equal)

  ! -> real (real4, real8)
  call test_case_create('real', test_suite_equal)
  call assert_equal(1.0, 2.0, __FILE__, __LINE__, test_suite_equal)
  call assert_equal(3.0, 3.0, __FILE__, __LINE__, test_suite_equal)
  call assert_equal(1.0D0, 2.0D0, __FILE__, __LINE__, test_suite_equal)
  call assert_equal(3.0D0, 3.0D0, __FILE__, __LINE__, test_suite_equal)

  call assert_equal([1.0, 1.0], [1.0, 2.0], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([1.0, 1.0], [1.0, 1.0], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 2.0D0], __FILE__, __LINE__, test_suite_equal)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 1.0D0], __FILE__, __LINE__, test_suite_equal)

  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 2.0, 1.0, 1.0], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 2.0D0, 1.0D0, 1.0D0], [2, 2]), __FILE__, __LINE__, test_suite_equal)
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), __FILE__, __LINE__, test_suite_equal)

  call test_suite_report(test_suite_equal)
  call test_suite_final(test_suite_equal)

  ! test assert_great_than routines
  call test_suite_init('great_then', test_suite_great_than)

  ! -> integer (int8->kind=1, int16->kind=2, int32->kind=4, int64->kind=8)
  call test_case_create('integer', test_suite_great_than)
  call assert_great_than(int(1, 1), int(1, 1), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(2, 1), int(1, 1), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(1, 2), int(1, 2), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(2, 2), int(1, 2), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(1, 4), int(1, 4), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(2, 4), int(1, 4), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(1, 8), int(1, 8), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(int(2, 8), int(1, 8), __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [int(0, 1), int(2, 1), int(0, 1), int(0, 1)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [int(0, 1), int(0, 1), int(0, 1), int(0, 1)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [int(0, 2), int(2, 2), int(0, 2), int(0, 2)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [int(0, 2), int(0, 2), int(0, 2), int(0, 2)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [int(0, 4), int(2, 4), int(0, 4), int(0, 4)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [int(0, 4), int(0, 4), int(0, 4), int(0, 4)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [int(0, 8), int(2, 8), int(0, 8), int(0, 8)], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [int(0, 8), int(0, 8), int(0, 8), int(0, 8)], __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than(reshape([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [2, 2]), &
      & reshape([int(0, 1), int(2, 1), int(0, 1), int(0, 1)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([int(1, 1), int(1, 1), int(1, 1), int(1, 1)], [2, 2]), &
      & reshape([int(0, 1), int(0, 1), int(0, 1), int(0, 1)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than(reshape([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [2, 2]), &
      & reshape([int(0, 2), int(2, 2), int(0, 2), int(0, 2)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([int(1, 2), int(1, 2), int(1, 2), int(1, 2)], [2, 2]), &
      & reshape([int(0, 2), int(0, 2), int(0, 2), int(0, 2)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than(reshape([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [2, 2]), &
      & reshape([int(0, 4), int(2, 4), int(0, 4), int(0, 4)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([int(1, 4), int(1, 4), int(1, 4), int(1, 4)], [2, 2]), &
      & reshape([int(0, 4), int(0, 4), int(0, 4), int(0, 4)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than(reshape([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [2, 2]), &
      & reshape([int(0, 8), int(2, 8), int(0, 8), int(0, 8)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([int(1, 8), int(1, 8), int(1, 8), int(1, 8)], [2, 2]), &
      & reshape([int(0, 8), int(0, 8), int(0, 8), int(0, 8)], [2, 2]), __FILE__, __LINE__, test_suite_great_than)

  ! -> real (real4, real8)
  call test_case_create('real', test_suite_great_than)
  call assert_great_than(1.0, 1.0, __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(2.0, 1.0, __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(1.0D0, 1.0D0, __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(2.0D0, 1.0D0, __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than([1.0, 1.0], [0.0, 2.0], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([1.0, 1.0], [0.0, 0.0], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([1.0D0, 1.0D0], [0.0D0, 2.0D0], __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than([1.0D0, 1.0D0], [0.0D0, 0.0D0], __FILE__, __LINE__, test_suite_great_than)

  call assert_great_than(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([0.0, 2.0, 0.0, 0.0], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([0.0, 0.0, 0.0, 0.0], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([0.0D0, 2.0D0, 0.0D0, 0.0D0], [2, 2]), __FILE__, __LINE__, test_suite_great_than)
  call assert_great_than(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([0.0D0, 0.0D0, 0.0D0, 0.0D0], [2, 2]), __FILE__, __LINE__, test_suite_great_than)

  call test_suite_report(test_suite_great_than)
  call test_suite_final(test_suite_great_than)

end program test_assert
