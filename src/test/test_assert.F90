program test_assert

  use unit_test

  call test_case_init()
  call test_case_create('Test 1')
  
  ! assert_approximate
  ! -> real4
  call assert_approximate(1.0, 2.0)
  call assert_approximate(1.0, 1.0)
  call assert_approximate(1.0, 1.10, 0.1)
  call assert_approximate(1.0, 1.01, 0.1)
  call assert_approximate([1.0, 1.0], [1.0, 1.10])
  call assert_approximate([1.0, 1.0], [1.0, 1.00000000001])
  call assert_approximate([1.0, 1.0], [1.0, 1.10], 0.1)
  call assert_approximate([1.0, 1.0], [1.0, 1.01], 0.1)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]))
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.00000000001, 1.0, 1.0], [2, 2]))
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.1, 1.0, 1.0], [2, 2]), 0.1)
  call assert_approximate(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.01, 1.0, 1.0], [2, 2]), 0.1)
  
  ! -> real8
  call assert_approximate(1.0D0, 1.1D0)
  call assert_approximate(1.0D0, 1.00000000001D0)
  call assert_approximate(1.0D0, 1.1D0, 0.1D0)
  call assert_approximate(1.0D0, 1.00000000001D0, 0.1D0)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0])
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.00000000001D0])
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.1D0], 0.1D0)
  call assert_approximate([1.0D0, 1.0D0], [1.0D0, 1.01D0], 0.1D0)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]))
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.000000000001D0, 1.0D0, 1.0D0], [2, 2]))
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.1D0, 1.0D0, 1.0D0], [2, 2]), 0.1D0)
  call assert_approximate(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.01D0, 1.0D0, 1.0D0], [2, 2]), 0.1D0)

  ! assert_false/_true
  call assert_false(.true.)
  call assert_false(.false.)
  
  call assert_true(.false.)
  call assert_true(.true.)

  ! assert_equal
  ! -> string
  call assert_equal('abc', 'abcd')
  call assert_equal('abc', 'abc')
  call assert_equal(['abc', 'abc'], ['abc', 'abd'])
  call assert_equal(['abc', 'abc'], ['abc', 'abc'])
  
  ! -> integer
  call assert_equal(1, 2)
  call assert_equal(3, 3)
  call assert_equal([1, 1, 1, 1], [1, 2, 1, 1])
  call assert_equal([1, 1, 1, 1], [1, 1, 1, 1])
  call assert_equal(reshape([1, 1, 1, 1], [2, 2]), reshape([1, 2, 1, 1], [2, 2]))
  call assert_equal(reshape([1, 1, 1, 1], [2, 2]), reshape([1, 1, 1, 1], [2, 2]))
  
  ! -> real4
  call assert_equal(1.0, 2.0)
  call assert_equal(3.0, 3.0)
  call assert_equal([1.0, 1.0], [1.0, 2.0])
  call assert_equal([1.0, 1.0], [1.0, 1.0])
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 2.0, 1.0, 1.0], [2, 2]))
  call assert_equal(reshape([1.0, 1.0, 1.0, 1.0], [2, 2]), reshape([1.0, 1.0, 1.0, 1.0], [2, 2]))
  
  ! -> real8
  call assert_equal(1.0D0, 2.0D0)
  call assert_equal(3.0D0, 3.0D0)
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 2.0D0])
  call assert_equal([1.0D0, 1.0D0], [1.0D0, 1.0D0])
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 2.0D0, 1.0D0, 1.0D0], [2, 2]))
  call assert_equal(reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]), reshape([1.0D0, 1.0D0, 1.0D0, 1.0D0], [2, 2]))

  ! assert_great_than
  ! -> integer
  call assert_great_than(1, 1)
  call assert_great_than(2, 1)
  
  ! -> real4
  call assert_great_than(1.0, 1.0)
  call assert_great_than(2.0, 1.0)
  
  ! -> real8
  call assert_great_than(1.0D0, 1.0D0)
  call assert_great_than(2.0D0, 1.0D0)

  call test_case_report('Test 1')
  call test_case_final()

end program test_assert
