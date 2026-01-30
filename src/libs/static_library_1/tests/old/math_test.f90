program math_test
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)
! private
! public::max_new_test_common
! public::min_new_test_common

  print *, "MODULE MATH"
  if (.not. max_new_test_common()) then
    print *, "faild: max_new_test_common"
    stop 1
  end if
  if (.not. min_new_test_common()) then
    print * , "faild: min_new_test_common"
    stop 1
  end if

  contains

  logical function max_new_test_common() result(res)
  use math, only:max_new
  implicit none (type, external)
    real(dp) :: a, b

    a = 1d0
    b = - 3d0

    res = max_new(a, b) == 1d0
  end function max_new_test_common

  logical function min_new_test_common() result(res)
  use math, only:min_new
  implicit none (type, external)
    real(dp) :: a, b

    a = 1d0
    b = - 3d0

    res = min_new(a, b) == - 3d0
  end function min_new_test_common
end program math_test
