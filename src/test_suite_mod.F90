module test_suite_mod

  use test_common_mod

  implicit none

  private

  public test_suite_init
  public test_suite_final
  public test_suite_report
  public test_suite_type
  public test_suite_get_assert_results

contains

  subroutine test_suite_init(name, suite)

    character(*), intent(in) :: name
    type(test_suite_type), intent(in), optional, target :: suite

    type(test_suite_type), pointer :: dummy_suite

    if (present(suite)) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    dummy_suite%name = name

  end subroutine test_suite_init

  subroutine test_suite_final(suite)

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

  end subroutine test_suite_final

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

    call write_header(log_out_unit, 'Report of Suite: ' // trim(dummy_suite%name))

    num_test_case = 0
    num_all_succeed_assert = 0
    num_all_assert = 0

    ! prints all test cases
    if (associated(dummy_suite%test_case_head)) then
      write(log_out_unit, *) '+-> Details:'
      write(log_out_unit, *) '|   |'

      test_case => dummy_suite%test_case_head
      do while (associated(test_case))
        num_test_case = num_test_case + 1

        write(log_out_unit, *) '|   +-> ' // trim(test_case%name) // ': ', to_string(test_case%num_succeed_assert), ' of ' // &
          to_string(test_case%num_assert) // ' assertions succeed.'

        num_all_succeed_assert = num_all_succeed_assert + test_case%num_succeed_assert
        num_all_assert = num_all_assert + test_case%num_assert

        assert_result => test_case%assert_result_head

        do while (associated(assert_result))
          if (.not. assert_result%passed) then
            write(log_err_unit, *) '|   |   |'
            if (assert_result%right_operand /= '') then
              write(log_err_unit, *) '|   |   +-> Assertion #' // to_string(assert_result%id) // ' failed with reason: ' // &
                'x (' // trim(assert_result%left_operand) // ') ' // trim(assert_result%assert_operator) // &
                ' y (' // trim(assert_result%right_operand) // ')'
              write(log_err_unit, *) '|   |   +-> Check line: ', trim(assert_result%file_name), ':', to_string(assert_result%line_number)
            else
              write(log_err_unit, *) '|   |   +-> Assertion #' // to_string(assert_result%id) // ' failed with reason: ' // &
                'x is not ' // trim(assert_result%assert_operator) // '!'
            end if
          end if
          assert_result => assert_result%next
        end do

        test_case => test_case%next
        write(log_err_unit, *) '|   |'
      end do
    end if

    write(log_out_unit, *) '|'
    write(log_out_unit, *) '+-> Summary:'
    write(log_out_unit, *) '|   +-> ' // trim(dummy_suite%name) // ': ', to_string(num_all_succeed_assert), ' of ' // &
      to_string(num_all_assert) // ' assertions succeed.'

    call write_footer(log_out_unit)

  end subroutine test_suite_report

  function test_suite_get_assert_results(suite) result(res)

    type(test_suite_type), target, optional :: suite
    logical, allocatable :: res(:)

    type(test_suite_type), pointer :: dummy_suite
    type(test_case_type), pointer :: dummy_case
    type(assert_result_type), pointer :: dummy_assert_result
    integer :: num_assert, i, j, k

    ! if no suite parameter was passed, use default test suite
    if (present(suite) ) then
      dummy_suite => suite
    else
      dummy_suite => default_test_suite
    end if

    num_assert = 0
    dummy_case => dummy_suite%test_case_head
    num_assert = num_assert + dummy_case%num_assert
    do i = 1, dummy_suite%num_test_case - 1
      dummy_case => dummy_case%next
      num_assert = num_assert + dummy_case%num_assert
    end do

    allocate(res(num_assert))
    res = .false.
    dummy_case => dummy_suite%test_case_head
    k = 0
    do i = 1, dummy_suite%num_test_case
      dummy_assert_result => dummy_case%assert_result_head
      k = k + 1
      res(k) = dummy_assert_result%passed
      do j = 1, dummy_case%num_assert - 1
        dummy_assert_result => dummy_assert_result%next
        k = k + 1
        res(k) = dummy_assert_result%passed
      end do
      dummy_case => dummy_case%next
    end do

  end function test_suite_get_assert_results


end module test_suite_mod
