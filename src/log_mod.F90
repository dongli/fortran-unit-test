module log_mod

  use string_mod

  implicit none

  private

  public log_error
  public log_table
  public log_header
  public log_footer
  public log_err_unit
  public log_out_unit

  integer :: log_err_unit = 0
  integer :: log_out_unit = 6

  interface log_table
    module procedure log_table_1d_real4
    module procedure log_table_1d_real8
  end interface log_table

contains

  subroutine log_error(caller, message)

    character(*), intent(in) :: caller
    character(*), intent(in) :: message

    !write(log_err_unit, *) '[Error]: ', trim(caller), ': ', trim(message)
    !stop 1

    ! call log_header(log_err_unit, 'ERROR')
    write(log_err_unit, *) trim(caller), ': '
    write(log_err_unit, *)
    write(log_err_unit, *) trim(message)
    write(log_err_unit, *)
    stop 1

  end subroutine log_error

  subroutine log_table_1d_real4(caller, array)

    character(*), intent(in) :: caller
    real(4), intent(in) :: array(:)

    integer i

    call log_header(log_out_unit, 'TABLE')
    write(log_err_unit, *) trim(caller), ': '
    write(log_err_unit, *)
    do i = 1, size(array)
      write(log_out_unit, "(i8, '.', 10x, a)") i, trim(to_string(array(i)))
    end do
    write(log_out_unit, *)

  end subroutine log_table_1d_real4

  subroutine log_table_1d_real8(caller, array)

    character(*), intent(in) :: caller
    real(8), intent(in) :: array(:)

    integer i

    call log_header(log_out_unit, 'TABLE')
    write(log_err_unit, *) trim(caller), ': '
    write(log_err_unit, *)
    do i = 1, size(array)
      write(log_out_unit, "(i8, '.', 10x, a)") i, trim(to_string(array(i)))
    end do
    write(log_out_unit, *)

  end subroutine log_table_1d_real8

  subroutine log_header(log_unit, log_title)

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

  end subroutine log_header
  
  subroutine log_footer(log_unit)
  
    integer, intent(in) :: log_unit
  
    integer columns, i
  
    columns = 80 ! TODO: How to set this variable according to the actual terminal width?
    write(log_unit, *)
  
    do i = 1, columns
      write(log_unit, '(a)', advance='no') '/'
    end do
    write(log_unit, *)
    write(log_unit, *)
    
  end subroutine log_footer

end module log_mod
