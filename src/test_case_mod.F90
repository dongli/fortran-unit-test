module test_case_mod

  use log_mod
  use string_mod

  implicit none

  private

  public test_case_init
  public test_case_create
  public test_case_append_assert
  public test_case_report
  public test_case_final

  type assert_result_type
    integer id
    character(30) assert_operator
    character(256) left_operand
    character(256) :: right_operand = ''
    logical passed
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

  integer num_test_case
  type(test_case_type), pointer :: test_case_head => null()
  type(test_case_type), pointer :: test_case_tail => null()
  type(test_case_type), pointer :: curr_test_case => null()

contains

  subroutine test_case_init()

    num_test_case = 0

  end subroutine test_case_init

  subroutine test_case_final()

    type(test_case_type), pointer :: test_case1, test_case2
    type(assert_result_type), pointer :: assert_result1, assert_result2

    test_case1 => test_case_head
    do while (associated(test_case1))
      test_case2 => test_case1%next
      assert_result1 => test_case1%assert_result_head
      do while (associated(assert_result1))
        assert_result2 => assert_result1%next
        deallocate(assert_result1)
        assert_result1 => assert_result2
      end do
      deallocate(test_case1)
      test_case1 => test_case2
    end do

  end subroutine test_case_final

  subroutine test_case_create(name)

    character(*), intent(in) :: name

    if (.not. associated(test_case_head)) then
      allocate(test_case_head)
      test_case_tail => test_case_head
    else
      allocate(test_case_tail%next)
      test_case_tail => test_case_tail%next
    end if
    test_case_tail%name = name
    curr_test_case => test_case_tail

    num_test_case = num_test_case + 1

  end subroutine test_case_create

  subroutine test_case_report(name)

    character(*), intent(in) :: name

    type(test_case_type), pointer :: test_case
    type(assert_result_type), pointer :: assert_result

    test_case => get_test_case(name)

    call log_header(log_out_unit, 'Test case report')
    write(log_out_unit, *) trim(test_case%name) // ': ', trim(to_string(test_case%num_succeed_assert)), ' of ' // &
      trim(to_string(test_case%num_assert)) // ' assertions succeed.'
    write(log_out_unit, *)

    assert_result => test_case%assert_result_head
    do while (associated(assert_result))
      if (.not. assert_result%passed) then
        if (assert_result%right_operand /= '') then
          write(log_err_unit, *) 'Assertion #' // trim(to_string(assert_result%id)) // ' failed with reason: ' // &
            'x (' // trim(assert_result%left_operand) // ') ' // trim(assert_result%assert_operator) // &
            ' y (' // trim(assert_result%right_operand) // ')'
        else
          write(log_err_unit, *) 'Assertion #' // trim(to_string(assert_result%id)) // ' failed with reason: ' // &
            'x is not ' // trim(assert_result%assert_operator) // '!'
        end if
        write(6, *)
      end if
      assert_result => assert_result%next
    end do

  end subroutine test_case_report

  subroutine test_case_append_assert(assert_operator, passed, left_operand, right_operand)

    character(*), intent(in) :: assert_operator
    logical, intent(in) :: passed
    character(*), intent(in) :: left_operand
    character(*), intent(in), optional :: right_operand

    if (.not. associated(curr_test_case%assert_result_head)) then
      allocate(curr_test_case%assert_result_head)
      curr_test_case%assert_result_tail => curr_test_case%assert_result_head
    else
      allocate(curr_test_case%assert_result_tail%next)
      curr_test_case%assert_result_tail => curr_test_case%assert_result_tail%next
    end if
    curr_test_case%assert_result_tail%assert_operator = assert_operator
    curr_test_case%assert_result_tail%passed = passed
    curr_test_case%assert_result_tail%left_operand = left_operand
    if (present(right_operand)) curr_test_case%assert_result_tail%right_operand = right_operand
    curr_test_case%num_assert = curr_test_case%num_assert + 1
    if (passed) curr_test_case%num_succeed_assert = curr_test_case%num_succeed_assert + 1
    curr_test_case%assert_result_tail%id = curr_test_case%num_assert

  end subroutine test_case_append_assert

  function get_test_case(name) result(res)

    character(*), intent(in) :: name
    type(test_case_type), pointer :: res

    integer i

    res => test_case_head
    do i = 1, num_test_case
      if (res%name == name) then
        exit
      end if
      res => res%next
    end do

  end function get_test_case

end module test_case_mod
