program main
use static_library_1___math, only:max_new, min_new
use static_library_1___functions, only:abs_x, neg_x
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)
  print *, max_new(1d0, 2d0)
  print *, min_new(1d0, 2d0)
  print *, abs_x([1d0, 2d0, - 1d0])
  print *, neg_x([1d0, 2d0, - 1d0])

  block
    complex(dp) :: a
    real(dp) :: b
    a = (1d0, 2d0)
    b = real(a)

    print *, a, b
  end block

  print *, sin((- 1d0, 0d0)) - sin((- 1d0, - 3d0)), sin((0d0, 1d0)) - sin((0d0, 2d0))

  ! call test()

  ! call assert(1.0d0 >= 0.0_dp)

  ! print *
  ! print *
  ! print *
  ! pause "print Enter to continue..."
end program main
