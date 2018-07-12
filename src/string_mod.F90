module string_mod

  implicit none

  private

  public to_string

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
    res = tmp

  end function integer1_to_string
  
  function integer2_to_string(x) result(res)

    integer(2), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = tmp

  end function integer2_to_string
  
  function integer4_to_string(x) result(res)

    integer(4), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = tmp

  end function integer4_to_string
  
  function integer8_to_string(x) result(res)

    integer(8), intent(in) :: x
    character(:), allocatable :: res

    character(range(x)+2) tmp

    write(tmp, '(i0)') x
    res = tmp

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

    res = merge('true ', 'false', x)

  end function logical_to_string

end module string_mod
