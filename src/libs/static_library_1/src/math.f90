!> @author Haart
!!
!! a test module with simple math operations
!! for example of a modern fortran project
module math
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)
private
  public :: max_new, min_new

  character(len = *), parameter :: module_name = "static_library_1___math"

  interface
    !> max of a, b
    pure elemental real(dp) module function max_new(a, b) result(res)
    implicit none (type, external)
      !> the first value
      real(dp), intent (in) :: a
      !> the second value
      real(dp), intent (in) :: b
    end function max_new

    !> min of a, b
    pure elemental real(dp) module function min_new(a, b) result(res)
    implicit none (type, external)
      !> the first value
      real(dp), intent (in) :: a
      !> the second value
      real(dp), intent (in) :: b
    end function min_new
  end interface
end module math
