module assert_mod

  use test_common_mod
  use test_case_mod

  implicit none

  private

  public assert_equal
  public assert_approximate
  public assert_great_than
  public assert_true
  public assert_false
  public assert_failure

  interface assert_equal
    module procedure assert_equal_integer1
    module procedure assert_equal_integer2
    module procedure assert_equal_integer4
    module procedure assert_equal_integer8
    module procedure assert_equal_real4
    module procedure assert_equal_real8
    module procedure assert_equal_string
    module procedure assert_equal_integer1_vec
    module procedure assert_equal_integer2_vec
    module procedure assert_equal_integer4_vec
    module procedure assert_equal_integer8_vec
    module procedure assert_equal_real4_vec
    module procedure assert_equal_real8_vec
    module procedure assert_equal_string_vec
    module procedure assert_equal_integer1_array
    module procedure assert_equal_integer2_array
    module procedure assert_equal_integer4_array
    module procedure assert_equal_integer8_array
    module procedure assert_equal_real4_array
    module procedure assert_equal_real8_array
    module procedure assert_equal_string_array
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
    module procedure assert_great_than_integer1
    module procedure assert_great_than_integer2
    module procedure assert_great_than_integer4
    module procedure assert_great_than_integer8
    module procedure assert_great_than_real4
    module procedure assert_great_than_real8
    module procedure assert_great_than_integer1_vec
    module procedure assert_great_than_integer2_vec
    module procedure assert_great_than_integer4_vec
    module procedure assert_great_than_integer8_vec
    module procedure assert_great_than_real4_vec
    module procedure assert_great_than_real8_vec
    module procedure assert_great_than_integer1_array
    module procedure assert_great_than_integer2_array
    module procedure assert_great_than_integer4_array
    module procedure assert_great_than_integer8_array
    module procedure assert_great_than_real4_array
    module procedure assert_great_than_real8_array
  end interface assert_great_than

  interface get_relative_difference
    module procedure get_relative_difference_real4
    module procedure get_relative_difference_real8
  end interface

  real(8), parameter :: eps_default_kind8 = 1d-3
  real(4), parameter :: eps_default_kind4 = 1e-3

contains

  subroutine assert_equal_integer1(x, y, file_name, line_number, suite)

    integer(1), intent(in) :: x
    integer(1), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_integer1

  subroutine assert_equal_integer2(x, y, file_name, line_number, suite)

    integer(2), intent(in) :: x
    integer(2), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_integer2

  subroutine assert_equal_integer4(x, y, file_name, line_number, suite)

    integer(4), intent(in) :: x
    integer(4), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_integer4

  subroutine assert_equal_integer8(x, y, file_name, line_number, suite)

    integer(8), intent(in) :: x
    integer(8), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('==', x == y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_equal_integer8

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

  subroutine assert_equal_integer1_vec(x, y, file_name, line_number, suite)

    integer(1), intent(in) :: x(:)
    integer(1), intent(in) :: y(:)
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

  end subroutine assert_equal_integer1_vec

  subroutine assert_equal_integer2_vec(x, y, file_name, line_number, suite)

    integer(2), intent(in) :: x(:)
    integer(2), intent(in) :: y(:)
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

  end subroutine assert_equal_integer2_vec

  subroutine assert_equal_integer4_vec(x, y, file_name, line_number, suite)

    integer(4), intent(in) :: x(:)
    integer(4), intent(in) :: y(:)
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

  end subroutine assert_equal_integer4_vec

  subroutine assert_equal_integer8_vec(x, y, file_name, line_number, suite)

    integer(8), intent(in) :: x(:)
    integer(8), intent(in) :: y(:)
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

  end subroutine assert_equal_integer8_vec

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

  subroutine assert_equal_integer1_array(x, y, file_name, line_number, suite)

    integer(1), intent(in) :: x(:,:)
    integer(1), intent(in) :: y(:,:)
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

  end subroutine assert_equal_integer1_array

  subroutine assert_equal_integer2_array(x, y, file_name, line_number, suite)

    integer(2), intent(in) :: x(:,:)
    integer(2), intent(in) :: y(:,:)
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

  end subroutine assert_equal_integer2_array

  subroutine assert_equal_integer4_array(x, y, file_name, line_number, suite)

    integer(4), intent(in) :: x(:,:)
    integer(4), intent(in) :: y(:,:)
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

  end subroutine assert_equal_integer4_array

  subroutine assert_equal_integer8_array(x, y, file_name, line_number, suite)

    integer(8), intent(in) :: x(:,:)
    integer(8), intent(in) :: y(:,:)
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

  end subroutine assert_equal_integer8_array

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

  subroutine assert_equal_string_array(x, y, file_name, line_number, suite)

    character(*), intent(in) :: x(:, :)
    character(*), intent(in) :: y(:, :)
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

    call test_case_append_assert('==', passed, x(loc_i, loc_j), y(loc_i, loc_j), file_name, line_number, suite)

  end subroutine assert_equal_string_array

  subroutine assert_approximate_real4(x, y, file_name, line_number, eps, suite)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    real(4) :: eps_

    eps_ = merge(eps, eps_default_kind4, present(eps))

    if (x == y) then
      passed = .true.
    else if (x == 0.0E0 .OR. y == 0.0E0) then
      passed = abs(x - y) <  eps_**2
    else
      passed = abs(x - y) / get_relative_difference(x, y) < eps_
    end if

    call test_case_append_assert('=~', passed, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_approximate_real4

  subroutine assert_approximate_real8(x, y, file_name, line_number, eps, suite)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    real(8) :: eps_

    eps_ = merge(eps, eps_default_kind8, present(eps))

    if (x == y) then
      passed = .true.
    else if (x == 0.0D0 .OR. y == 0.0D0) then
      passed = abs(x - y) <  eps_**2
    else
      passed = abs(x - y) / get_relative_difference(x, y) < eps_
    end if

    call test_case_append_assert('=~', passed, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_approximate_real8

  subroutine assert_approximate_real4_vec(x, y, z, file_name, line_number, eps, suite)

    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    real(4), intent(out), optional :: z(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i
    real(4) :: eps_

    eps_ = merge(eps, eps_default_kind4, present(eps))

    passed = .true.
    loc = lbound(x, 1)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
      do i = lbound(x, 1), ubound(x, 1)
        if (x(i) == y(i)) then
          cycle
        else if (x(i) == 0.0E0 .OR. y(i) == 0.0E0) then
          if (.not. abs(x(i) - y(i)) <  eps_**2) then
            loc = i
            passed = .false.
            exit
          else
            cycle
          end if
        else
          if (.not. abs(x(i) - y(i)) / get_relative_difference(x(i), y(i)) < eps_) then
            loc = i
            passed = .false.
            exit
          else
            cycle
          end if
        end if
      end do
    end if

    call test_case_append_assert('=~', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_approximate_real4_vec

  subroutine assert_approximate_real8_vec(x, y, z, file_name, line_number, eps, suite)

    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    real(8), intent(out), optional :: z(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i
    real(8) :: eps_

    eps_ = merge(eps, eps_default_kind8, present(eps))

    passed = .true.
    loc = lbound(x, 1)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
      do i = lbound(x, 1), ubound(x, 1)
        if (x(i) == y(i)) then
          if (present(z)) then
            z(i) = 0.0D0
          else
            cycle
          end if
        else if (x(i) == 0.0D0 .OR. y(i) == 0.0D0) then
          if (.not. abs(x(i) - y(i)) <  eps_**2) then
            loc = i
            passed = .false.
            if (present(z)) then
              z(i) = abs(x(i) - y(i))
            else
              exit
            end if
          else
            if (present(z)) then
              z(i) = abs(x(i) - y(i))
            else
              cycle
            end if
          end if
        else
          if (.not. abs(x(i) - y(i)) / get_relative_difference(x(i), y(i)) < eps_) then
            loc = i
            passed = .false.
            if (present(z)) then
              z(i) = abs(x(i) - y(i)) / get_relative_difference(x(i), y(i))
            else
              exit
            end if
          else
            if (present(z)) then
              z(i) = abs(x(i) - y(i)) / get_relative_difference(x(i), y(i))
            else
              cycle
            end if
          end if
        end if
      end do
    end if

    call test_case_append_assert('=~', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_approximate_real8_vec

  subroutine assert_approximate_real4_array(x, y, z, file_name, line_number, eps, suite)

    real(4), intent(in) :: x(:, :)
    real(4), intent(in) :: y(:, :)
    real(4), intent(out), optional :: z(:, :)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(4), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc_i, loc_j, i, j
    real(4) :: eps_

    eps_ = merge(eps, eps_default_kind4, present(eps))

    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (x(i, j) == y(i, j)) then
            if (present(z)) then
              z(i, j) = 0.0E0
            else
              cycle
            end if
          else if (x(i, j) == 0.0E0 .OR. y(i, j) == 0.0E0) then
            if (.not. abs(x(i, j) - y(i, j)) <  eps_**2) then
              loc_i = i
              loc_j = j
              passed = .false.
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j))
              else
                exit
              end if
            else
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j))
              else
                cycle
              end if
            end if
          else
            if (.not. abs(x(i, j) - y(i, j)) / get_relative_difference(x(i, j), y(i, j)) < eps_) then
              loc_i = i
              loc_j = j
              passed = .false.
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j)) / get_relative_difference(x(i, j), y(i, j))
              else
                exit
              end if
            else
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j)) / get_relative_difference(x(i, j), y(i, j))
              else
                cycle
              end if
            end if
          end if
        end do
      end do
    end if

    call test_case_append_assert('=~', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_approximate_real4_array

  subroutine assert_approximate_real8_array(x, y, z, file_name, line_number, eps, suite)

    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: y(:, :)
    real(8), intent(out), optional :: z(:, :)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    real(8), intent(in), optional :: eps
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc_i, loc_j, i, j
    real(8) :: eps_

    eps_ = merge(eps, eps_default_kind8, present(eps))

    passed = .true.
    loc_i = lbound(x, 1)
    loc_j = lbound(x, 2)
    if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1) .and. &
      lbound(x, 2) == lbound(y, 2) .and. ubound(x, 2) == ubound(y, 2)) then
      do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
          if (x(i, j) == y(i, j)) then
            if (present(z)) then
              z(i, j) = 0.0D0
            else
              cycle
            end if
          else if (x(i, j) == 0.0D0 .OR. y(i, j) == 0.0D0) then
            if (.not. abs(x(i, j) - y(i, j)) <  eps_**2) then
              loc_i = i
              loc_j = j
              passed = .false.
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j))
              else
                exit
              end if
            else
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j))
              else
                cycle
              end if
            end if
          else
            if (.not. abs(x(i, j) - y(i, j)) / get_relative_difference(x(i, j), y(i, j)) < eps_) then
              loc_i = i
              loc_j = j
              passed = .false.
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j)) / get_relative_difference(x(i, j), y(i, j))
              else
                exit
              end if
            else
              if (present(z)) then
                z(i, j) = abs(x(i, j) - y(i, j)) / get_relative_difference(x(i, j), y(i, j))
              else
                cycle
              end if
            end if
          end if
        end do
      end do
    end if

    call test_case_append_assert('=~', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_approximate_real8_array

  subroutine assert_great_than_integer1(x, y, file_name, line_number, suite)

    integer(1), intent(in) :: x
    integer(1), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_integer1

  subroutine assert_great_than_integer2(x, y, file_name, line_number, suite)

    integer(2), intent(in) :: x
    integer(2), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_integer2

  subroutine assert_great_than_integer4(x, y, file_name, line_number, suite)

    integer(4), intent(in) :: x
    integer(4), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_integer4

  subroutine assert_great_than_integer8(x, y, file_name, line_number, suite)

    integer(8), intent(in) :: x
    integer(8), intent(in) :: y
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    call test_case_append_assert('>', x > y, to_string(x), to_string(y), file_name, line_number, suite)

  end subroutine assert_great_than_integer8

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

  subroutine assert_great_than_integer1_vec(x, y, file_name, line_number, suite)

    integer(1), intent(in) :: x(:)
    integer(1), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if(all(x > y)) then
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) > y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if

    call test_case_append_assert('>', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_great_than_integer1_vec

  subroutine assert_great_than_integer2_vec(x, y, file_name, line_number, suite)

    integer(2), intent(in) :: x(:)
    integer(2), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if(all(x > y)) then
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) > y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if

    call test_case_append_assert('>', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_great_than_integer2_vec

  subroutine assert_great_than_integer4_vec(x, y, file_name, line_number, suite)

    integer(4), intent(in) :: x(:)
    integer(4), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if(all(x > y)) then
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) > y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if

    call test_case_append_assert('>', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_great_than_integer4_vec

  subroutine assert_great_than_integer8_vec(x, y, file_name, line_number, suite)

    integer(8), intent(in) :: x(:)
    integer(8), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if(all(x > y)) then
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) > y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if

    call test_case_append_assert('>', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_great_than_integer8_vec

  subroutine assert_great_than_real4_vec(x, y, file_name, line_number, suite)

    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if(all(x > y)) then
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) > y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if

    call test_case_append_assert('>', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_great_than_real4_vec

  subroutine assert_great_than_real8_vec(x, y, file_name, line_number, suite)

    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), intent(in), optional :: suite

    logical :: passed
    integer :: loc, i

    if(all(x > y)) then
      passed = .true.
      loc = lbound(x, 1)
    else
      passed = .false.
      if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
        do i = lbound(x, 1), ubound(x, 1)
          if(.not. x(i) > y(i)) then
            loc = i
            exit
          end if
        end do
      end if
    end if

    call test_case_append_assert('>', passed, to_string(x(loc)), to_string(y(loc)), file_name, line_number, suite)

  end subroutine assert_great_than_real8_vec

  subroutine assert_great_than_integer1_array(x, y, file_name, line_number, suite)

    integer(1), intent(in) :: x(:,:)
    integer(1), intent(in) :: y(:,:)
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
          if (.not. x(i, j) > y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('>', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_great_than_integer1_array

  subroutine assert_great_than_integer2_array(x, y, file_name, line_number, suite)

    integer(2), intent(in) :: x(:,:)
    integer(2), intent(in) :: y(:,:)
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
          if (.not. x(i, j) > y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('>', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_great_than_integer2_array

  subroutine assert_great_than_integer4_array(x, y, file_name, line_number, suite)

    integer(4), intent(in) :: x(:,:)
    integer(4), intent(in) :: y(:,:)
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
          if (.not. x(i, j) > y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('>', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_great_than_integer4_array

  subroutine assert_great_than_integer8_array(x, y, file_name, line_number, suite)

    integer(8), intent(in) :: x(:,:)
    integer(8), intent(in) :: y(:,:)
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
          if (.not. x(i, j) > y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('>', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_great_than_integer8_array

  subroutine assert_great_than_real4_array(x, y, file_name, line_number, suite)

    real(4), intent(in) :: x(:,:)
    real(4), intent(in) :: y(:,:)
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
          if (.not. x(i, j) > y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('>', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_great_than_real4_array

  subroutine assert_great_than_real8_array(x, y, file_name, line_number, suite)

    real(8), intent(in) :: x(:,:)
    real(8), intent(in) :: y(:,:)
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
          if (.not. x(i, j) > y(i, j)) then
            loc_i = i
            loc_j = j
            passed = .false.
            exit
          end if
        end do
      end do
    end if

    call test_case_append_assert('>', passed, to_string(x(loc_i, loc_j)), to_string(y(loc_i, loc_j)), file_name, line_number, suite)

  end subroutine assert_great_than_real8_array

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

  real(4) function get_relative_difference_real4(x, y, case)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    integer, optional, intent(in) :: case

    select case (merge(case, 1, present(case)))

    case (1)
      get_relative_difference_real4 = abs(max(abs(x), abs(y)))

    case (2)
      get_relative_difference_real4 = abs(max(x, y))

    case (3)
      get_relative_difference_real4 = abs(min(abs(x), abs(y)))

    case (4)
      get_relative_difference_real4 = abs(min(x, y))

    case (5)
      get_relative_difference_real4 = abs((x + y) / 2)

    case (6)
      get_relative_difference_real4 = abs((abs(x) + abs(y)) / 2)

    end select

  end function get_relative_difference_real4

  real(8) function get_relative_difference_real8(x, y, case)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    integer, optional, intent(in) :: case

    select case (merge(case, 1, present(case)))

    case (1)
      get_relative_difference_real8 = abs(max(abs(x), abs(y)))

    case (2)
      get_relative_difference_real8 = abs(max(x, y))

    case (3)
      get_relative_difference_real8 = abs(min(abs(x), abs(y)))

    case (4)
      get_relative_difference_real8 = abs(min(x, y))

    case (5)
      get_relative_difference_real8 = abs((x + y) / 2)

    case (6)
      get_relative_difference_real8 = abs((abs(x) + abs(y)) / 2)

    end select

  end function get_relative_difference_real8

end module assert_mod
