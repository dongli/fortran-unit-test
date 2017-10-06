module test_case_mod

  implicit none

  type test_case_type
    character(256) :: name = 'A test case'
    type(test_case_type), pointer :: next
  end type test_case_type

  type(test_case_type), pointer :: test_case_head
  type(test_case_type), pointer :: test_case_tail
  type(test_case_type), pointer :: curr_test_case

contains

  subroutine test_case_init()

    nullify(test_case_head)
    nullify(test_case_tail)

  end subroutine test_case_init

  subroutine test_case_final()

    type(test_case_type), pointer :: test_case1, test_case2

    test_case1 => test_case_head
    do while (associated(test_case1))
      test_case2 => test_case1%next
      nullify(test_case1)
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
    nullify(test_case_tail%next)
    test_case_tail%name = name
    curr_test_case => test_case_tail

  end subroutine test_case_create

end module test_case_mod
