module assert_mod

  use log_mod
  use string_mod
  use test_case_mod

  implicit none

  interface assert_equal
    module procedure assert_equal_integer
    module procedure assert_equal_real4
    module procedure assert_equal_real8
    module procedure assert_equal_string
    module procedure assert_equal_integer_vec
    module procedure assert_equal_real4_vec
    module procedure assert_equal_real8_vec
    module procedure assert_equal_string_vec
    module procedure assert_equal_integer_array
    module procedure assert_equal_real4_array
    module procedure assert_equal_real8_array
  end interface assert_equal

  interface assert_approximate
    module procedure assert_approximate_real4
    module procedure assert_approximate_real8
    module procedure assert_approximate_real4_vec
    module procedure assert_approximate_real8_vec
    module procedure assert_approximate_real4_array
    module procedure assert_approximate_real8_array
  end interface assert_approximate

  interface assert_great_than
    module procedure assert_great_than_integer
    module procedure assert_great_than_real4
    module procedure assert_great_than_real8
  end interface assert_great_than

contains

  subroutine assert_equal_integer(x, y, file_name, line_number, suite)

    integer, intent(in) :: x
    integer, intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_integer

  subroutine assert_equal_real4(x, y, file_name, line_number, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_real4

  subroutine assert_equal_real8(x, y, file_name, line_number, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_real8

  subroutine assert_equal_string(x, y, file_name, line_number, suite)

    character(*), intent(in) :: x
    character(*), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', trim(x) == trim(y), x, y, file_name, line_number, suite)

  end subroutine assert_equal_string

  subroutine assert_equal_integer_vec(x, y, file_name, line_number, suite)
 
    integer, intent(in) :: x(:)
    integer, intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc, i
    
    if(all(x == y)) then 
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) == y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if
 
    call test_case_append_assert('==', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)
 
  end subroutine assert_equal_integer_vec

  subroutine assert_equal_real4_vec(x, y, file_name, line_number, suite)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc, i
    
    if (all(x == y)) then 
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) == y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if
 
    call test_case_append_assert('==', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)
 
  end subroutine assert_equal_real4_vec

  subroutine assert_equal_real8_vec(x, y, file_name, line_number, suite)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if (all(x == y)) then 
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) == y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if
 
    call test_case_append_assert('==', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)
 
  end subroutine assert_equal_real8_vec

  subroutine assert_equal_string_vec(x, y, file_name, line_number, suite)
 
    character(*), intent(in) :: x(:)
    character(*), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc, i
    
    if (all(x == y)) then 
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) == y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if
 
    call test_case_append_assert('==', passed, x(loc), y(loc), file_name, line_number, suite)
 
  end subroutine assert_equal_string_vec

  subroutine assert_equal_integer_array(x, y, file_name, line_number, suite)
 
    integer, intent(in) :: x(:,:)
    integer, intent(in) :: y(:,:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc_i, loc_j, i, j
    
    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (.not. x(i, j) == y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if
 
    call test_case_append_assert('==', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)
 
  end subroutine assert_equal_integer_array

  subroutine assert_equal_real4_array(x, y, file_name, line_number, suite)
 
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc_i, loc_j, i, j
    
    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (.not. x(i, j) == y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if
 
    call test_case_append_assert('==', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)
 
  end subroutine assert_equal_real4_array

  subroutine assert_equal_real8_array(x, y, file_name, line_number, suite)
 
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc_i, loc_j, i, j
    
    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (.not. x(i, j) == y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('==', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)
 
  end subroutine assert_equal_real8_array

  subroutine assert_approximate_real4(x, y, file_name, line_number, eps, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('=~', abs(x-y) < merge(eps, 1.0e-10, present(eps)), to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_approximate_real4

  subroutine assert_approximate_real8(x, y, file_name, line_number, eps, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('=~', abs(x-y) < merge(eps, 1.0d-10, present(eps)), to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_approximate_real8

  subroutine assert_approximate_real4_vec(x, y, file_name, line_number, eps, suite)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc, i

    passed = .true.
    loc = lbound(x, 1)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
      do i = lbound(x, 1), ubound(x, 1)
        if (.not. abs(x(i) - y(i)) < merge(eps, 1.0e-10, present(eps))) then
          loc = i
          passed = .false.
          exit
        end if
      end do
    end if
 
    call test_case_append_assert('=~', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)
 
  end subroutine assert_approximate_real4_vec

  subroutine assert_approximate_real8_vec(x, y, file_name, line_number, eps, suite)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc, i
    
    passed = .true.
    loc = lbound(x, 1)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
      do i = lbound(x, 1), ubound(x, 1)
        if (.not. abs(x(i) - y(i)) < merge(eps, 1.0d-10, present(eps))) then
          loc = i
          passed = .false.
          exit
        end if
      end do
    end if
 
    call test_case_append_assert('=~', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)
 
  end subroutine assert_approximate_real8_vec

  subroutine assert_approximate_real4_array(x, y, file_name, line_number, eps, suite)
 
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc_i, loc_j, i, j
    
    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (.not. abs(x(i, j) - y(i, j)) < merge(eps, 1.0e-10, present(eps))) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if
 
    call test_case_append_assert('=~', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)
 
  end subroutine assert_approximate_real4_array

  subroutine assert_approximate_real8_array(x, y, file_name, line_number, eps, suite)
 
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite
    
    logical :: passed
    integer :: loc_i, loc_j, i, j
    
    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (.not. abs(x(i, j) - y(i, j)) < merge(eps, 1.0d-10, present(eps))) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if
 
    call test_case_append_assert('=~', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)
 
  end subroutine assert_approximate_real8_array

  subroutine assert_great_than_integer(x, y, file_name, line_number, suite)

    integer, intent(in) :: x
    integer, intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_integer

  subroutine assert_great_than_real4(x, y, file_name, line_number, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_real4

  subroutine assert_great_than_real8(x, y, file_name, line_number, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_real8

  subroutine assert_true(x, file_name, line_number, suite)

    logical, intent(in) :: x
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('true', x, to_string(x), 'N/A', file_name, line_number, suite = suite)

  end subroutine assert_true

  subroutine assert_false(x, file_name, line_number, suite)

    logical, intent(in) :: x
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('false', .not. x, to_string(x), 'N/A', file_name, line_number, suite = suite)

  end subroutine assert_false

  subroutine assert_failure(file_name, line_number, suite)

    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('failure', .false., 'N/A', 'N/A', file_name, line_number, suite = suite)

  end subroutine assert_failure

end module assert_mod
