!> @author Haart
!>
!> a module for integration a complex function by custom built path
!> with explicit description of path parts
!> for not standart complex integration
module integrator
use system___dynamic_array, only: dynamic_array_real64, dynamic_array_real64_constructor, &
  dynamic_array_int32, dynamic_array_int32_constructor, &
  dynamic_array_complex64, dynamic_array_complex64_constructor
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)
private
  character(len = *), parameter :: module_name = "integral_generator___integrator"

  public :: integrator_obj, integrator_builder_obj
  public :: integrator_builder_constructor
  public :: &
    PATH_PART_TYPE_PARALLEL_X, &
    PATH_PART_TYPE_PARALLEL_Y
  public :: &
    INTEGRATION_TYPE_GAUSS3_WITH_AUTO_RECALCULATION, &
    INTEGRATION_TYPE_GAUSS4_WITH_AUTO_RECALCULATION, &
    INTEGRATION_TYPE_GAUSS5_WITH_AUTO_RECALCULATION, &
    INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION, &
    INTEGRATION_TYPE_GAUSS7_WITH_AUTO_RECALCULATION, &
    INTEGRATION_TYPE_GAUSS8_WITH_AUTO_RECALCULATION

  public :: test

  integer(i4), parameter :: PATH_PART_TYPE_PARALLEL_X = 0
  integer(i4), parameter :: PATH_PART_TYPE_PARALLEL_Y = 1

  integer(i4), parameter :: INTEGRATION_TYPE_GAUSS3_WITH_AUTO_RECALCULATION = 0
  integer(i4), parameter :: INTEGRATION_TYPE_GAUSS4_WITH_AUTO_RECALCULATION = 1
  integer(i4), parameter :: INTEGRATION_TYPE_GAUSS5_WITH_AUTO_RECALCULATION = 2
  integer(i4), parameter :: INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION = 3
  integer(i4), parameter :: INTEGRATION_TYPE_GAUSS7_WITH_AUTO_RECALCULATION = 4
  integer(i4), parameter :: INTEGRATION_TYPE_GAUSS8_WITH_AUTO_RECALCULATION = 5

  type :: integrator_builder_obj
  private
    type(dynamic_array_complex64) :: start_points
    type(dynamic_array_complex64) :: end_points
    type(dynamic_array_int32) :: path_type
    type(dynamic_array_int32) :: strategies

    type(dynamic_array_real64) :: min_deltas
    type(dynamic_array_real64) :: max_deltas

    real(dp) :: eps

    real(dp) :: max_abs_point_coordinate_value
  contains
    procedure, pass :: build

    procedure, pass :: add_new_path_part
    procedure, pass :: start_point
    procedure, pass :: end_point
    procedure, pass :: part_integration_strategy
    procedure, pass :: min_delta
    procedure, pass :: max_delta

    procedure, pass :: set_accuration
  end type integrator_builder_obj

  type :: integrator_obj
  private
    complex(dp), allocatable :: start_points(:)
    complex(dp), allocatable :: end_points(:)
    integer(i4), allocatable :: path_type(:)
    integer(i4), allocatable :: strategies(:)

    real(dp), allocatable :: min_deltas(:)
    real(dp), allocatable :: max_deltas(:)

    real(dp) :: eps
  contains
    procedure, pass :: integrate
  end type integrator_obj

  interface
    module subroutine test()
    implicit none (type, external)
    end subroutine test

    pure elemental &
    type(integrator_builder_obj) module function integrator_builder_constructor() result(this)
    implicit none (type, external)
    end function integrator_builder_constructor

    !> build an integrator object
    pure type(integrator_obj) module function build(this) result(res)
    implicit none (type, external)
      class(integrator_builder_obj), intent (in) :: this
    end function build

    !> add description of a new path part
    module subroutine add_new_path_part(this, PATH_PART_TYPE)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      integer(i4), intent(in) :: PATH_PART_TYPE
    end subroutine add_new_path_part
    !> add a start point of this intagration part
    !> if it is not setted a start point is setted at the end point of the last part
    module subroutine start_point(this, value)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      complex(dp), intent(in) :: value
    end subroutine start_point
    !> add an end point of this intagration part
    module subroutine end_point(this, value)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      complex(dp), intent(in) :: value
    end subroutine end_point
    !> add integration strategy for this intagration part
    !> if it is not setted then the last part value is used
    module subroutine part_integration_strategy(this, INTEGRATION_TYPE)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      integer(i4), intent(in) :: INTEGRATION_TYPE
    end subroutine part_integration_strategy
    !> add min delta of integration step for this intagration part
    !> if it is not setted then the last part value is used
    module subroutine min_delta(this, value)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      real(dp), intent(in) :: value
    end subroutine min_delta
    !> add max delta of integration step for this intagration part
    !> if it is not setted then the last part value is used
    module subroutine max_delta(this, value)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      real(dp), intent(in) :: value
    end subroutine max_delta


    !> set an integration strategy
    !> if it is not setted then the last strategy is used
    module subroutine set_accuration(this, eps)
    implicit none (type, external)
      class(integrator_builder_obj), intent (inout) :: this
      real(dp), intent(in) :: eps
    end subroutine set_accuration
  end interface

  interface
    complex(dp) module function integrate(this, func) result(res)
    implicit none (type, external)
      class(integrator_obj), intent (in) :: this
      procedure(integration_function_type) :: func
    end function integrate
  end interface

  abstract interface
    complex(dp) function integration_function_type(x)
    import :: dp
    implicit none (type, external)
      complex(dp), intent(in) :: x
    end function integration_function_type
  end interface
end module integrator
