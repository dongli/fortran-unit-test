module test_case_mod

  use log_mod
  use string_mod

  implicit none

  private

  public test_case_init
  public test_case_create
  public test_case_append_assert
  public test_case_report
  public test_suite_report
  public test_case_final
  public test_suite_type

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
    character(256) :: name = "A test suite"
    integer :: num_test_case = 0
    type(test_case_type), pointer :: test_case_head => null()
    type(test_case_type), pointer :: test_case_tail => null()
  end type test_suite_type

  type(test_suite_type), target, private :: default_test_suite

contains

  subroutine test_case_init()

    default_test_suite%name = "Default test suite"

  end subroutine test_case_init

  subroutine test_case_final(suite)

    type(test_suite_type), optional, target :: suite
    type(test_suite_type), pointer :: dummy_suite
    type(test_case_type), pointer :: test_case1, test_case2
    type(assert_result_type), pointer :: assert_result1, assert_result2

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    test_case1 => dummy_suite%test_case_head
    do while (associated(test_case1))
      test_case2 => test_case1%next
      assert_result1 => test_case1%assert_result_head
      do while (associated(assert_result1))
        assert_result2 => assert_result1%next
        deallocate(assert_result1)
        assert_result1 => assert_result2
      end do
      nullify(test_case1%assert_result_tail)
      deallocate(test_case1)
      test_case1 => test_case2
    end do
    nullify(dummy_suite%test_case_tail)

  end subroutine test_case_final

  subroutine test_case_create(name, suite)

    character(*), intent(in) :: name
    type(test_suite_type), target, optional :: suite
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
    type(test_suite_type), optional, target :: suite
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

    call log_header(log_out_unit, 'Report of Suite: '//trim(dummy_suite%name)//', Case: '//trim(test_case%name))
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
          write(log_err_unit, *) 'Check line: ', trim(assert_result%file_name), ':', trim(to_string(assert_result%line_number))
        else
          write(log_err_unit, *) 'Assertion #' // trim(to_string(assert_result%id)) // ' failed with reason: ' // &
            'x is not ' // trim(assert_result%assert_operator) // '!'
        end if
        write(log_err_unit, *)
      end if
      assert_result => assert_result%next
    end do

  end subroutine test_case_report
  
  subroutine test_suite_report(suite)
  
    type(test_suite_type), intent(in), optional, target :: suite
    type(test_suite_type), pointer :: dummy_suite
    type(test_case_type), pointer :: test_case
    type(assert_result_type), pointer :: assert_result
    integer :: num_test_case
    integer :: num_all_succeed_assert
    integer :: num_all_assert

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if
    
    call log_header(log_out_unit, 'Report of Suite: '//trim(dummy_suite%name))
  
    num_test_case = 0
    num_all_succeed_assert = 0
    num_all_assert = 0
  
    ! prints all test cases
    if (associated(dummy_suite%test_case_head)) then
      write(log_out_unit, *) '-> Details:'
    
      test_case => dummy_suite%test_case_head
      do while (associated(test_case))
        num_test_case = num_test_case + 1
      
        write(log_out_unit, *) trim(test_case%name) // ': ', trim(to_string(test_case%num_succeed_assert)), ' of ' // &
          trim(to_string(test_case%num_assert)) // ' assertions succeed.'
      
        num_all_succeed_assert = num_all_succeed_assert + test_case%num_succeed_assert
        num_all_assert = num_all_assert + test_case%num_assert
        
        assert_result => test_case%assert_result_head
        do while (associated(assert_result))
          if (.not. assert_result%passed) then
            if (assert_result%right_operand /= '') then
              write(log_err_unit, *) 'Assertion #' // trim(to_string(assert_result%id)) // ' failed with reason: ' // &
                'x (' // trim(assert_result%left_operand) // ') ' // trim(assert_result%assert_operator) // &
                ' y (' // trim(assert_result%right_operand) // ')'
              write(log_err_unit, *) 'Check line: ', trim(assert_result%file_name), ':', trim(to_string(assert_result%line_number))
            else
              write(log_err_unit, *) 'Assertion #' // trim(to_string(assert_result%id)) // ' failed with reason: ' // &
                'x is not ' // trim(assert_result%assert_operator) // '!'
            end if
            write(log_err_unit, *)
          end if
          assert_result => assert_result%next
        end do
            
        test_case => test_case%next
      end do
    end if
  
    write(log_out_unit, *)
    write(log_out_unit, *) '-> Summary:'  
    write(log_out_unit, *) trim(dummy_suite%name) // ': ', trim(to_string(num_all_succeed_assert)), ' of ' // &
      trim(to_string(num_all_assert)) // ' assertions succeed.'
  
    call log_footer(log_out_unit)
    
  end subroutine test_suite_report

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
    type(test_case_type), pointer :: res
    type(test_suite_type), target, optional :: suite
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
