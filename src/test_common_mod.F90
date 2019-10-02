module test_common_mod

  implicit none

  integer, parameter :: log_err_unit = 0
  integer, parameter :: log_out_unit = 6

  type assert_result_type
    integer id
    character(30) :: assert_operator
    character(256) :: left_operand
    character(256) :: right_operand = ''
    character(256) :: file_name = 'Unknown file'
    integer :: line_number = -1
    logical :: passed = .false.
    type(assert_result_type), pointer :: next => null()
  end type assert_result_type

  type test_case_type
    character(256) :: name = 'A test case'
    type(test_case_type), pointer :: next => null()
    integer :: num_assert = 0
    integer :: num_succeed_assert = 0
    type(assert_result_type), pointer :: assert_result_head => null()
    type(assert_result_type), pointer :: assert_result_tail => null()
  end type test_case_type

  type test_suite_type
    character(256) :: name = 'A test suite'
    integer :: num_test_case = 0
    type(test_case_type), pointer :: test_case_head => null()
    type(test_case_type), pointer :: test_case_tail => null()
  end type test_suite_type

  type(test_suite_type), target :: default_test_suite

  interface to_string
    module procedure integer1_to_string
    module procedure integer2_to_string
    module procedure integer4_to_string
    module procedure integer8_to_string
    module procedure real4_to_string
    module procedure real8_to_string
    module procedure logical_to_string
  end interface to_string

contains

  function integer1_to_string(x) result(res)

    integer(1), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer1_to_string
  
  function integer2_to_string(x) result(res)

    integer(2), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer2_to_string
  
  function integer4_to_string(x) result(res)

    integer(4), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer4_to_string
  
  function integer8_to_string(x) result(res)

    integer(8), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = trim(tmp)

  end function integer8_to_string

  function real4_to_string(x, decimal_width) result(res)

    real(4), intent(in) :: x
    integer, intent(in), optional :: decimal_width
    character(:), allocatable :: res

    integer w, y
    character(10) fmt
    character(range(x)+2) tmp

    if (present(decimal_width)) then
      w = decimal_width
    else
      w = 4
    end if
    write(fmt, "('(g', i0, '.', i0, ')')") w+6, w
    y = int(x)
    write(tmp, fmt) x
    res = trim(tmp)

  end function real4_to_string

  function real8_to_string(x, decimal_width) result(res)

    real(8), intent(in) :: x
    integer, intent(in), optional :: decimal_width
    character(:), allocatable :: res

    integer w, y
    character(10) fmt
    character(range(x)+2) tmp

    if (present(decimal_width)) then
      w = decimal_width
    else
      w = 4
    end if
    write(fmt, "('(g', i0, '.', i0, ')')") w+6, w
    y = int(x)
    write(tmp, fmt) x
    res = trim(tmp)

  end function real8_to_string

  function logical_to_string(x) result(res)

    logical, intent(in) :: x
    character(:), allocatable :: res

    res = trim(merge('true ', 'false', x))

  end function logical_to_string
 
  subroutine write_header(log_unit, log_title)

    integer, intent(in) :: log_unit
    character(*), intent(in) :: log_title

    integer columns, i, pos1, pos2

    columns = 80 ! TODO: How to set this variable according to the actual terminal width?

    write(log_unit, *)
    pos1 = (columns-len(log_title)-2)*0.5
    do i = 1, pos1
      write(log_unit, '(a)', advance='no') '/'
    end do
    write(log_unit, "(' ', a, ' ')", advance='no') trim(log_title)
    pos2 = pos1+len(log_title)+2
    do i = pos2, columns
      write(log_unit, '(a)', advance='no') '/'
    end do
    write(log_unit, *)
    write(log_unit, *)

  end subroutine write_header

  subroutine write_footer(log_unit)
  
    integer, intent(in) :: log_unit
  
    integer columns, i
  
    columns = 80 ! TODO: How to set this variable according to the actual terminal width?
    write(log_unit, *)
  
    do i = 1, columns
      write(log_unit, '(a)', advance='no') '/'
    end do
    write(log_unit, *)
    write(log_unit, *)
    
  end subroutine write_footer

end module test_common_mod
