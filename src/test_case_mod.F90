module test_case_mod

  use test_common_mod

  implicit none

  private

  public test_case_create
  public test_case_append_assert
  public test_case_report

contains

  subroutine test_case_create(name, suite)

    character(*), intent(in) :: name
    type(test_suite_type), intent(in), target, optional :: suite
    type(test_suite_type), pointer :: dummy_suite

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    if (.not. associated(dummy_suite%test_case_head)) then
      allocate(dummy_suite%test_case_head)
      dummy_suite%test_case_tail => dummy_suite%test_case_head
    else
      allocate(dummy_suite%test_case_tail%next)
      dummy_suite%test_case_tail => dummy_suite%test_case_tail%next
    end if
    dummy_suite%test_case_tail%name = name

    dummy_suite%num_test_case = dummy_suite%num_test_case + 1

  end subroutine test_case_create

  subroutine test_case_report(name, suite)

    character(*), intent(in) :: name
    type(test_suite_type), intent(in), optional, target :: suite
    type(test_suite_type), pointer :: dummy_suite
    type(test_case_type), pointer :: test_case
    type(assert_result_type), pointer :: assert_result

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    test_case => get_test_case(name, dummy_suite)

    call write_header(log_out_unit, 'Report of Suite: '//trim(dummy_suite%name)//', Case: '//trim(test_case%name))
    write(log_out_unit, *) '+-> ' // trim(test_case%name) // ': ', to_string(test_case%num_succeed_assert), ' of ' // &
      to_string(test_case%num_assert) // ' assertions succeed.'

    assert_result => test_case%assert_result_head
    do while (associated(assert_result))
      if (.not. assert_result%passed) then
          write(log_err_unit, *) '|   |'
        if (assert_result%right_operand /= '') then
          write(log_err_unit, *) '|   +-> Assertion #' // to_string(assert_result%id) // ' failed with reason: ' // &
            'x (' // trim(assert_result%left_operand) // ') ' // trim(assert_result%assert_operator) // &
            ' y (' // trim(assert_result%right_operand) // ')'
          write(log_err_unit, *) '|   +-> Check line: ', trim(assert_result%file_name), ':', to_string(assert_result%line_number)
        else
          write(log_err_unit, *) '|   +-> Assertion #' // to_string(assert_result%id) // ' failed with reason: ' // &
            'x is not ' // trim(assert_result%assert_operator) // '!'
        end if
      end if
      assert_result => assert_result%next
    end do

  end subroutine test_case_report

  subroutine test_case_append_assert(assert_operator, passed, left_operand, right_operand, file_name, line_number, suite)

    character(*), intent(in) :: assert_operator
    logical, intent(in) :: passed
    character(*), intent(in) :: left_operand
    character(*), intent(in), optional :: right_operand
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: line_number
    type(test_suite_type), target, optional :: suite

    type(test_suite_type), pointer :: dummy_suite

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    if (.not. associated(dummy_suite%test_case_tail)) call test_case_create('default')

    if (.not. associated(dummy_suite%test_case_tail%assert_result_head)) then
      allocate(dummy_suite%test_case_tail%assert_result_head)
      dummy_suite%test_case_tail%assert_result_tail => dummy_suite%test_case_tail%assert_result_head
    else
      allocate(dummy_suite%test_case_tail%assert_result_tail%next)
      dummy_suite%test_case_tail%assert_result_tail => dummy_suite%test_case_tail%assert_result_tail%next
    end if
    dummy_suite%test_case_tail%assert_result_tail%assert_operator = assert_operator
    dummy_suite%test_case_tail%assert_result_tail%passed = passed
    dummy_suite%test_case_tail%assert_result_tail%left_operand = left_operand
    if (present(file_name)) dummy_suite%test_case_tail%assert_result_tail%file_name = file_name
    if (present(line_number)) dummy_suite%test_case_tail%assert_result_tail%line_number = line_number
    if (present(right_operand)) dummy_suite%test_case_tail%assert_result_tail%right_operand = right_operand
    dummy_suite%test_case_tail%num_assert = dummy_suite%test_case_tail%num_assert + 1
    if (passed) dummy_suite%test_case_tail%num_succeed_assert = dummy_suite%test_case_tail%num_succeed_assert + 1
    dummy_suite%test_case_tail%assert_result_tail%id = dummy_suite%test_case_tail%num_assert

  end subroutine test_case_append_assert

  function get_test_case(name, suite) result(res)

    integer i
    character(*), intent(in) :: name
    type(test_suite_type), target, optional :: suite
    type(test_case_type), pointer :: res
    type(test_suite_type), pointer :: dummy_suite

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    res => dummy_suite%test_case_head
    do i = 1, dummy_suite%num_test_case
      if (res%name == name) then
        exit
      end if
      res => res%next
    end do

  end function get_test_case

end module test_case_mod
