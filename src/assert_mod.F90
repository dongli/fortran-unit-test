module assert_mod

  use log_mod
  use string_mod
  use test_case_mod

  implicit none

  character(12), parameter :: unknown_file = 'Unknown file'
  integer, parameter :: unknown_line = 0

  interface assert_equal
    module procedure assert_equal_integer_1
    module procedure assert_equal_integer_2
    module procedure assert_equal_real4_1
    module procedure assert_equal_real4_2
    module procedure assert_equal_real8_1
    module procedure assert_equal_real8_2
    module procedure assert_equal_string_1
    module procedure assert_equal_string_2
    module procedure assert_equal_integer_vec_1
    module procedure assert_equal_integer_vec_2
    module procedure assert_equal_real4_vec_1
    module procedure assert_equal_real4_vec_2
    module procedure assert_equal_real8_vec_1
    module procedure assert_equal_real8_vec_2
    module procedure assert_equal_string_vec_1
    module procedure assert_equal_string_vec_2
    module procedure assert_equal_integer_array_1
    module procedure assert_equal_integer_array_2
    module procedure assert_equal_real4_array_1
    module procedure assert_equal_real4_array_2
    module procedure assert_equal_real8_array_1
    module procedure assert_equal_real8_array_2
  end interface assert_equal

  interface assert_approximate
    module procedure assert_approximate_real4_1
    module procedure assert_approximate_real4_2
    module procedure assert_approximate_real8_1
    module procedure assert_approximate_real8_2
    module procedure assert_approximate_real4_vec_1
    module procedure assert_approximate_real4_vec_2
    module procedure assert_approximate_real8_vec_1
    module procedure assert_approximate_real8_vec_2
    module procedure assert_approximate_real4_array_1
    module procedure assert_approximate_real4_array_2
    module procedure assert_approximate_real8_array_1
    module procedure assert_approximate_real8_array_2
  end interface assert_approximate

  interface assert_great_than
    module procedure assert_great_than_integer_1
    module procedure assert_great_than_integer_2
    module procedure assert_great_than_real4_1
    module procedure assert_great_than_real4_2
    module procedure assert_great_than_real8_1
    module procedure assert_great_than_real8_2
  end interface assert_great_than

  interface assert_true
    module procedure assert_true_1
    module procedure assert_true_2
  end interface assert_true

  interface assert_false
    module procedure assert_false_1
    module procedure assert_false_2
  end interface assert_false

  interface assert_failure
    module procedure assert_failure_1
    module procedure assert_failure_2
  end interface assert_failure

contains

  subroutine assert_equal_integer_1(x, y, suite)

    integer, intent(in) :: x
    integer, intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_integer_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_integer_1

  subroutine assert_equal_integer_2(x, y, file_name, line_number, suite)

    integer, intent(in) :: x
    integer, intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_integer_2

  subroutine assert_equal_real4_1(x, y, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_real4_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_real4_1

  subroutine assert_equal_real4_2(x, y, file_name, line_number, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_real4_2

  subroutine assert_equal_real8_1(x, y, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_real8_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_real8_1

  subroutine assert_equal_real8_2(x, y, file_name, line_number, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_real8_2

  subroutine assert_equal_string_1(x, y, suite)

    character(*), intent(in) :: x
    character(*), intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_string_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_string_1

  subroutine assert_equal_string_2(x, y, file_name, line_number, suite)

    character(*), intent(in) :: x
    character(*), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', trim(x) == trim(y), x, y, file_name, line_number, suite)

  end subroutine assert_equal_string_2

  subroutine assert_equal_integer_vec_1(x, y, suite)
 
    integer, intent(in) :: x(:)
    integer, intent(in) :: y(:)
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_integer_vec_2(x, y, unknown_file, unknown_line, suite)
 
  end subroutine assert_equal_integer_vec_1

  subroutine assert_equal_integer_vec_2(x, y, file_name, line_number, suite)
 
    integer, intent(in) :: x(:)
    integer, intent(in) :: y(:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_integer_vec_2

  subroutine assert_equal_real4_vec_1(x, y, suite)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_real4_vec_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_real4_vec_1

  subroutine assert_equal_real4_vec_2(x, y, file_name, line_number, suite)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_real4_vec_2

  subroutine assert_equal_real8_vec_1(x, y, suite)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    type(test_suite_type), intent(in), optional :: suite
 
    call assert_equal_real8_vec_2(x, y, unknown_file, unknown_line, suite)
 
  end subroutine assert_equal_real8_vec_1

  subroutine assert_equal_real8_vec_2(x, y, file_name, line_number, suite)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_real8_vec_2

  subroutine assert_equal_string_vec_1(x, y, suite)
 
    character(*), intent(in) :: x(:)
    character(*), intent(in) :: y(:)
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_string_vec_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_string_vec_1

  subroutine assert_equal_string_vec_2(x, y, file_name, line_number, suite)
 
    character(*), intent(in) :: x(:)
    character(*), intent(in) :: y(:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_string_vec_2

  subroutine assert_equal_integer_array_1(x, y, suite)
 
    integer, intent(in) :: x(:,:)
    integer, intent(in) :: y(:,:)
    type(test_suite_type), intent(in), optional :: suite
    
    call assert_equal_integer_array_2(x, y, unknown_file, unknown_line, suite)
 
  end subroutine assert_equal_integer_array_1

  subroutine assert_equal_integer_array_2(x, y, file_name, line_number, suite)
 
    integer, intent(in) :: x(:,:)
    integer, intent(in) :: y(:,:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_integer_array_2

  subroutine assert_equal_real4_array_1(x, y, suite)
 
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_real4_array_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_real4_array_1

  subroutine assert_equal_real4_array_2(x, y, file_name, line_number, suite)
 
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_real4_array_2

  subroutine assert_equal_real8_array_1(x, y, suite)
 
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    type(test_suite_type), intent(in), optional :: suite

    call assert_equal_real8_array_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_equal_real8_array_1

  subroutine assert_equal_real8_array_2(x, y, file_name, line_number, suite)
 
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_equal_real8_array_2

  subroutine assert_approximate_real4_1(x, y, eps, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call assert_approximate_real4_2(x, y, unknown_file, unknown_line, eps, suite)

  end subroutine assert_approximate_real4_1

  subroutine assert_approximate_real4_2(x, y, file_name, line_number, eps, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('=~', abs(x-y) < merge(eps, 1.0e-10, present(eps)), to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_approximate_real4_2

  subroutine assert_approximate_real8_1(x, y, eps, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call assert_approximate_real8_2(x, y, unknown_file, unknown_line, eps, suite)

  end subroutine assert_approximate_real8_1

  subroutine assert_approximate_real8_2(x, y, file_name, line_number, eps, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('=~', abs(x-y) < merge(eps, 1.0d-10, present(eps)), to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_approximate_real8_2

  subroutine assert_approximate_real4_vec_1(x, y, eps, suite)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call assert_approximate_real4_vec_2(x, y, unknown_file, unknown_line, eps, suite)

  end subroutine assert_approximate_real4_vec_1

  subroutine assert_approximate_real4_vec_2(x, y, file_name, line_number, eps, suite)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_approximate_real4_vec_2

  subroutine assert_approximate_real8_vec_1(x, y, eps, suite)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call assert_approximate_real8_vec_2(x, y, unknown_file, unknown_line, eps, suite)
 
  end subroutine assert_approximate_real8_vec_1

  subroutine assert_approximate_real8_vec_2(x, y, file_name, line_number, eps, suite)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_approximate_real8_vec_2

  subroutine assert_approximate_real4_array_1(x, y, eps, suite)
 
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call assert_approximate_real4_array_2(x, y, unknown_file, unknown_line, eps, suite)
 
  end subroutine assert_approximate_real4_array_1

  subroutine assert_approximate_real4_array_2(x, y, file_name, line_number, eps, suite)
 
    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_approximate_real4_array_2

  subroutine assert_approximate_real8_array_1(x, y, eps, suite)
 
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    call assert_approximate_real8_array_2(x, y, unknown_file, unknown_line, eps, suite)
 
  end subroutine assert_approximate_real8_array_1

  subroutine assert_approximate_real8_array_2(x, y, file_name, line_number, eps, suite)
 
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
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
 
  end subroutine assert_approximate_real8_array_2

  subroutine assert_great_than_integer_1(x, y, suite)

    integer, intent(in) :: x
    integer, intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_great_than_integer_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_great_than_integer_1

  subroutine assert_great_than_integer_2(x, y, file_name, line_number, suite)

    integer, intent(in) :: x
    integer, intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_integer_2

  subroutine assert_great_than_real4_1(x, y, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_great_than_real4_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_great_than_real4_1

  subroutine assert_great_than_real4_2(x, y, file_name, line_number, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_real4_2

  subroutine assert_great_than_real8_1(x, y, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    type(test_suite_type), intent(in), optional :: suite

    call assert_great_than_real8_2(x, y, unknown_file, unknown_line, suite)

  end subroutine assert_great_than_real8_1

  subroutine assert_great_than_real8_2(x, y, file_name, line_number, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_real8_2

  subroutine assert_true_1(x, suite)

    logical, intent(in) :: x
    type(test_suite_type), intent(in), optional :: suite

    call assert_true_2(x, unknown_file, unknown_line, suite)

  end subroutine assert_true_1

  subroutine assert_true_2(x, file_name, line_number, suite)

    logical, intent(in) :: x
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('true', x, to_string(x), 'N/A', file_name, line_number, suite = suite)

  end subroutine assert_true_2

  subroutine assert_false_1(x, suite)

    logical, intent(in) :: x
    type(test_suite_type), intent(in), optional :: suite

    call assert_false_2(x, unknown_file, unknown_line, suite)

  end subroutine assert_false_1

  subroutine assert_false_2(x, file_name, line_number, suite)

    logical, intent(in) :: x
    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('false', .not. x, to_string(x), 'N/A', file_name, line_number, suite = suite)

  end subroutine assert_false_2

  subroutine assert_failure_1(suite)

    type(test_suite_type), intent(in), optional :: suite

    call assert_failure_2(unknown_file, unknown_line, suite)

  end subroutine assert_failure_1

  subroutine assert_failure_2(file_name, line_number, suite)

    character(*), intent(in) :: file_name
    integer, intent(in) :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('failure', .false., 'N/A', 'N/A', file_name, line_number, suite = suite)

  end subroutine assert_failure_2

end module assert_mod
