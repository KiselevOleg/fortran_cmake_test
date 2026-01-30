program functions_test
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)
! private
! public::abs_x_test_positive_x
! public::abs_x_test_negative_x
!
! public::neg_x_test_positive_x
! public::neg_x_test_negative_x

  print *, "MODULE FUNCTIONS"
  if (.not. abs_x_test_positive_x()) then
    print *, "faild: abs_x_test_positive_x"
    stop 1
  end if
  if (.not. abs_x_test_negative_x()) then
    print *, "faild: abs_x_test_negative_x"
    stop 1
  end if

  if (.not. neg_x_test_positive_x()) then
    print *, "faild: neg_x_test_positive_x"
    stop 1
  end if
  if ( .not. neg_x_test_negative_x()) then
    print *, "faild: neg_x_test_negative_x"
    stop 1
  end if

  contains

  logical function abs_x_test_positive_x() result(res)
  use functions, only:abs_x
  implicit none (type, external)
    real(dp) :: x

    x = 1d0
    res = abs_x(x) == 1d0
  end function abs_x_test_positive_x
  logical function abs_x_test_negative_x() result(res)
  use functions, only:abs_x
  implicit none (type, external)
    real(dp) :: x

    x = - 1d0
    res = abs_x(x) == 1d0
  end function abs_x_test_negative_x

  logical function neg_x_test_positive_x() result(res)
  use functions, only:neg_x
  implicit none (type, external)
    real(dp) :: x

    x = 1d0
    res = neg_x(x) == - 1d0
  end function neg_x_test_positive_x
  logical function neg_x_test_negative_x() result(res)
  use functions, only:neg_x
  implicit none (type, external)
    real(dp) :: x

    x = - 1d0
    res = neg_x(x) == 1d0
  end function neg_x_test_negative_x
end program functions_test
