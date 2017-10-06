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
  end interface assert_equal

  interface assert_approximate
    module procedure assert_approximate_real4
    module procedure assert_approximate_real8
  end interface assert_approximate

  interface assert_great_than
    module procedure assert_great_than_integer
    module procedure assert_approximate_real4
    module procedure assert_approximate_real8
  end interface assert_great_than

contains

  subroutine assert_equal_integer(x, y)

    integer, intent(in) :: x
    integer, intent(in) :: y

    if (x /= y) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') /= y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_equal_integer

  subroutine assert_equal_real4(x, y)

    real(4), intent(in) :: x
    real(4), intent(in) :: y

    if (x /= y) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') /= y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_equal_real4

  subroutine assert_equal_real8(x, y)

    real(8), intent(in) :: x
    real(8), intent(in) :: y

    if (x /= y) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') /= y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_equal_real8

  subroutine assert_equal_string(x, y)

    character(*), intent(in) :: x
    character(*), intent(in) :: y

    if (trim(x) /= trim(y)) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(x) // ') /= y (' // trim(y) // ') failed!')
    end if

  end subroutine assert_equal_string

  subroutine assert_approximate_real4(x, y, eps)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    real(4), intent(in), optional :: eps

    if (abs(x-y) > merge(eps, 1.0e-10, present(eps))) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') =~ y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_approximate_real4

  subroutine assert_approximate_real8(x, y, eps)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    real(8), intent(in), optional :: eps

    if (abs(x-y) > merge(eps, 1.0d-10, present(eps))) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') =~ y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_approximate_real8

  subroutine assert_great_than_integer(x, y)

    integer, intent(in) :: x
    integer, intent(in) :: y

    if (x <= y) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') /= y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_great_than_integer

  subroutine assert_great_than_real4(x, y)

    real(4), intent(in) :: x
    real(4), intent(in) :: y

    if (x <= y) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') /= y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_great_than_real4

  subroutine assert_great_than_real8(x, y)

    real(8), intent(in) :: x
    real(8), intent(in) :: y

    if (x <= y) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: ' // &
        'x (' // trim(to_string(x)) // ') /= y (' // trim(to_string(y)) // ') failed!')
    end if

  end subroutine assert_great_than_real8

  subroutine assert_true(x)

    logical, intent(in) :: x

    if (.not. x) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: x is false!')
    end if

  end subroutine assert_true

  subroutine assert_false(x)

    logical, intent(in) :: x

    if (x) then
      call log_error('Test case: ' // curr_test_case%name, 'Reason: x is true!')
    end if

  end subroutine assert_false

end module assert_mod
