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
  end interface assert_equal

  interface assert_approximate
    module procedure assert_approximate_real4
    module procedure assert_approximate_real8
    module procedure assert_approximate_real4_vec
    module procedure assert_approximate_real8_vec
  end interface assert_approximate

  interface assert_great_than
    module procedure assert_great_than_integer
    module procedure assert_great_than_real4
    module procedure assert_great_than_real8
  end interface assert_great_than

contains

  subroutine assert_equal_integer(x, y)

    integer, intent(in) :: x
    integer, intent(in) :: y

    call test_case_append_assert('==', x == y, to_string(x), to_string(y))

  end subroutine assert_equal_integer

  subroutine assert_equal_real4(x, y)

    real(4), intent(in) :: x
    real(4), intent(in) :: y

    call test_case_append_assert('==', x == y, to_string(x), to_string(y))

  end subroutine assert_equal_real4

  subroutine assert_equal_real8(x, y)

    real(8), intent(in) :: x
    real(8), intent(in) :: y

    call test_case_append_assert('==', x == y, to_string(x), to_string(y))

  end subroutine assert_equal_real8

  subroutine assert_equal_string(x, y)

    character(*), intent(in) :: x
    character(*), intent(in) :: y

    call test_case_append_assert('==', trim(x) == trim(y), x, y)

  end subroutine assert_equal_string
  
  subroutine assert_equal_integer_vec(x, y)
 
    integer, intent(in) :: x(:)
    integer, intent(in) :: y(:)
    
    logical				:: passed
    integer				:: loc, i
    
    if(all(x == y)) then 
        passed = .true.
        loc = lbound(x, 1)
    else
        passed = .false.
        if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
            do i = lbound(x, 1), ubound(x, 1)
                if(x(i) /= y(i)) then
                    loc = i
                    exit
                end if
            end do
        end if
    end	if
 
    call test_case_append_assert('==', passed, to_string(x(loc)), to_string(y(loc)))
 
  end subroutine assert_equal_integer_vec
  
  subroutine assert_equal_real4_vec(x, y)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    
    logical				:: passed
    integer				:: loc, i
    
    if (all(x == y)) then 
        passed = .true.
        loc = lbound(x, 1)
    else
        passed = .false.
        if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
            do i = lbound(x, 1), ubound(x, 1)
                if(x(i) /= y(i)) then
                    loc = i
                    exit
                end if
            end do
        end if
    end	if
 
    call test_case_append_assert('==', passed, to_string(x(loc)), to_string(y(loc)))
 
  end subroutine assert_equal_real4_vec
  
  subroutine assert_equal_real8_vec(x, y)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    
    logical				:: passed
    integer				:: loc, i
    
    if (all(x == y)) then 
        passed = .true.
        loc = lbound(x, 1)
    else
        passed = .false.
        if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
            do i = lbound(x, 1), ubound(x, 1)
                if(x(i) /= y(i)) then
                    loc = i
                    exit
                end if
            end do
        end if
    end	if
 
    call test_case_append_assert('==', passed, to_string(x(loc)), to_string(y(loc)))
 
  end subroutine assert_equal_real8_vec
  
  subroutine assert_equal_string_vec(x, y)
 
    character(*), intent(in) :: x(:)
    character(*), intent(in) :: y(:)
    
    logical				:: passed
    integer				:: loc, i
    
    if (all(x == y)) then 
        passed = .true.
        loc = lbound(x, 1)
    else
        passed = .false.
        if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
            do i = lbound(x, 1), ubound(x, 1)
                if(x(i) /= y(i)) then
                    loc = i
                    exit
                end if
            end do
        end if
    end	if
 
    call test_case_append_assert('==', passed, x(loc), y(loc))
 
  end subroutine assert_equal_string_vec 

  subroutine assert_approximate_real4(x, y, eps)

    real(4), intent(in) :: x
    real(4), intent(in) :: y
    real(4), intent(in), optional :: eps

    call test_case_append_assert('=~', abs(x-y) < merge(eps, 1.0e-10, present(eps)), to_string(x), to_string(y))

  end subroutine assert_approximate_real4

  subroutine assert_approximate_real8(x, y, eps)

    real(8), intent(in) :: x
    real(8), intent(in) :: y
    real(8), intent(in), optional :: eps

    call test_case_append_assert('=~', abs(x-y) < merge(eps, 1.0d-10, present(eps)), to_string(x), to_string(y))

  end subroutine assert_approximate_real8
  
  subroutine assert_approximate_real4_vec(x, y, eps)
 
    real(4), intent(in) :: x(:)
    real(4), intent(in) :: y(:)
    real(4), intent(in), optional :: eps
    
    logical				:: passed
    integer				:: loc, i
    
    if (all(abs(x - y) < merge(eps, 1.0d-10, present(eps)))) then 
        passed = .true.
        loc = lbound(x, 1)
    else
        passed = .false.
        if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
            do i = lbound(x, 1), ubound(x, 1)
                if(x(i) /= y(i)) then
                    loc = i
                    exit
                end if
            end do
        end if
    end	if
 
    call test_case_append_assert('=~', passed, to_string(x(loc)), to_string(y(loc)))
 
  end subroutine assert_approximate_real4_vec
  
  subroutine assert_approximate_real8_vec(x, y, eps)
 
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: y(:)
    real(8), intent(in), optional :: eps
    
    logical				:: passed
    integer				:: loc, i
    
    if (all(abs(x - y) < merge(eps, 1.0d-10, present(eps)))) then 
        passed = .true.
        loc = lbound(x, 1)
    else
        passed = .false.
        if (lbound(x, 1) == lbound(y, 1) .and. ubound(x, 1) == ubound(y, 1)) then
            do i = lbound(x, 1), ubound(x, 1)
                if(x(i) /= y(i)) then
                    loc = i
                    exit
                end if
            end do
        end if
    end	if
 
    call test_case_append_assert('=~', passed, to_string(x(loc)), to_string(y(loc)))
 
  end subroutine assert_approximate_real8_vec

  subroutine assert_great_than_integer(x, y)

    integer, intent(in) :: x
    integer, intent(in) :: y

    call test_case_append_assert('>', x > y, to_string(x), to_string(y))

  end subroutine assert_great_than_integer

  subroutine assert_great_than_real4(x, y)

    real(4), intent(in) :: x
    real(4), intent(in) :: y

    call test_case_append_assert('>', x > y, to_string(x), to_string(y))

  end subroutine assert_great_than_real4

  subroutine assert_great_than_real8(x, y)

    real(8), intent(in) :: x
    real(8), intent(in) :: y

    call test_case_append_assert('>', x > y, to_string(x), to_string(y))

  end subroutine assert_great_than_real8

  subroutine assert_true(x)

    logical, intent(in) :: x

    call test_case_append_assert('true', x, to_string(x))

  end subroutine assert_true

  subroutine assert_false(x)

    logical, intent(in) :: x

    call test_case_append_assert('false', .not. x, to_string(x))

  end subroutine assert_false

end module assert_mod
