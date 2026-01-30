!> @author Haart
!!
!! a test module with simple functions
!! for example of a modern fortran project
module functions
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)
private
  public :: abs_x, neg_x

  character(len = *), parameter :: module_name = "static_library_1___functions"

  interface
    !> count |x|
    pure elemental real(dp) module function abs_x(x) result(res)
    implicit none (type, external)
      !> the first value
      real(dp), intent (in) :: x
    end function abs_x
    !> count -x
    pure elemental real(dp) module function neg_x(x) result(res)
    implicit none (type, external)
      !> the first value
      real(dp), intent (in) :: x
    end function neg_x
  end interface
end module functions
