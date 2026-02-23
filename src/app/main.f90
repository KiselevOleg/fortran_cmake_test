program main
use integral_generator___integration, only: &
  integrator_obj, integrator_builder_obj, integrator_builder_constructor, &

  template_recalculation => integration_template_recalculation_constructor, &

  integrated_function_type, &
  path_part_parallel_Im => integration_path_part_parallel_Im_constructor, &
  path_part_parallel_Re => integration_path_part_parallel_Re_constructor, &

  element_gauss3 => integration_element_gauss3_constructor, &
  element_gauss4 => integration_element_gauss4_constructor, &
  element_gauss5 => integration_element_gauss5_constructor, &
  element_gauss6 => integration_element_gauss6_constructor, &
  element_gauss7 => integration_element_gauss7_constructor, &
  element_gauss8 => integration_element_gauss8_constructor
use integral_generator_old___integrator, only: &
  integrator_obj_old => integrator_obj, integrator_builder_obj_old => integrator_builder_obj, &
  integrator_builder_constructor_old => integrator_builder_constructor, &

  PATH_PART_TYPE_PARALLEL_X, &
  PATH_PART_TYPE_PARALLEL_Y, &

  INTEGRATION_TYPE_GAUSS3_WITH_AUTO_RECALCULATION, &
  INTEGRATION_TYPE_GAUSS4_WITH_AUTO_RECALCULATION, &
  INTEGRATION_TYPE_GAUSS5_WITH_AUTO_RECALCULATION, &
  INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION, &
  INTEGRATION_TYPE_GAUSS7_WITH_AUTO_RECALCULATION, &
  INTEGRATION_TYPE_GAUSS8_WITH_AUTO_RECALCULATION
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, qp => real128, &
  i1 => int8, i2 => int16, i4 => int32, i8 => int64
implicit none (type, external)

  ! option(ENABLE_PREPROCESSOR_STATE "Enable processing preprocessor directives" OFF)
  ! option(ENABLE_NATIVE_COMPILATION "Enable compilation only for this system (Release only)" OFF)

  ! option(ENABLE_OPENMP "Enable OpenMP (does not work with sanitizers)" OFF)
  ! option(ENABLE_LTO "Enable LTO (IPO)" ON)
  ! option(ENABLE_SANITIZERS "Enable sanitizers (Debug only)(cancels openmp)" OFF)
  !   integer(i4), parameter :: steps = 100000
  !   integer(i4), parameter :: run_count = 10
  ! Release
  !   new 1.07s
  !   old 0.76s
  ! Debug
  !   new 1.58s
  !   old 0.83s
  !   integer(i4), parameter :: steps = 100000 * 100
  !   integer(i4), parameter :: run_count = 10 / 2
  ! Release
  !   new 104.25s
  !   old 75.55s

  ! option(ENABLE_PREPROCESSOR_STATE "Enable processing preprocessor directives" OFF)
  ! option(ENABLE_NATIVE_COMPILATION "Enable compilation only for this system (Release only)" ON)

  ! option(ENABLE_OPENMP "Enable OpenMP (does not work with sanitizers)" OFF)
  ! option(ENABLE_LTO "Enable LTO (IPO)" ON)
  ! option(ENABLE_SANITIZERS "Enable sanitizers (Debug only)(cancels openmp)" OFF)
  !   integer(i4), parameter :: steps = 100000 * 100
  !   integer(i4), parameter :: run_count = 10 / 2
  ! Release
  !   new 104.58s
  !   old 76.32s

  integer(i4), parameter :: steps = 100000! * 100

  integer(i4), parameter :: run_count = 10 / 2

  integer(i4) :: i

  do i = 1, run_count
    block
      real(dp) :: start_time, end_time
      integer(i4) :: i
      complex(dp) :: res

      type(integrator_obj) :: integrator

      block
        type(integrator_builder_obj) :: builder
        builder = integrator_builder_constructor()
        call builder%add_new_path_part( &
          path_part_parallel_Re(builder%last_start((0d0, 0d0)), 3.14159265358979d0) &
        )
        call builder%part_integration_strategy( &
          template_recalculation(), &
          element_gauss6() &
        )
        call builder%set_accuration(1d-3)
        integrator = builder%build()
      end block

      call cpu_time(start_time)

      res = 0d0
      do i = 1, steps
        res = res + integrator%integrate(func)
      end do

      call cpu_time(end_time)

      print *, "new", end_time - start_time, res
    end block

    block
      real(dp) :: start_time, end_time
      integer(i4) :: i
      complex(dp) :: res

      type(integrator_obj_old) :: integrator

      block
        type(integrator_builder_obj_old) :: builder
        builder = integrator_builder_constructor_old()
        call builder%add_new_path_part(PATH_PART_TYPE_PARALLEL_X)
        call builder%start_point((0d0, 0d0))
        call builder%end_point((3.14159265358979d0, 0d0))
        call builder%part_integration_strategy(INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
        call builder%set_accuration(1d-3)
        integrator = builder%build()
      end block

      call cpu_time(start_time)

      res = 0d0
      do i = 1, steps
        res = res + integrator%integrate(func)
      end do

      call cpu_time(end_time)

      print *, "old", end_time - start_time, res
    end block
  end do

  contains
  complex(dp) function func(x) result(res)
  implicit none (type, external)
    complex(dp), intent(in) :: x

    res = sin(x)
  end function func
end program main
