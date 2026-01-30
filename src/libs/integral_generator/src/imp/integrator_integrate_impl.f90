submodule(integrator) integrator_integrate_impl
use system___assert, only: error_assert, error_not_assert, &
  warning_assert, warning_not_assert, &
  equals
implicit none (type, external)

  !integrate !do a loop on parts with run integrate_path_part

  !integrate_path_part([complex(dp) a, complex(dp) b], func)
  !  do projection_func and run recalculation_template_method(...)

  !recalculation_template_method([real(dp) a,real(dp) b] gause_element)

  !gause_elements([real(dp) a,real(dp) b], projection_func)
  !  use domain(n) and koef(n) as local parameters

  abstract interface
    complex(dp) function integration_function_projection_type(x)
    import :: dp
    implicit none (type, external)
      real(dp), intent(in) :: x
    end function integration_function_projection_type
    pure complex(dp) function projection_normilized_delta_type(x, dx)
    import :: dp
    implicit none (type, external)
      real(dp), intent(in) :: x
      real(dp), intent(in) :: dx
    end function projection_normilized_delta_type
    complex(dp) function integrate_element_type(func, a, b)
    import :: dp
    implicit none (type, external)
      procedure(integration_function_projection_type) :: func
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
    end function integrate_element_type
  end interface

  contains

  module procedure integrate
    integer(i4) :: i

    res = 0d0

    do i = 1, size(this%start_points)
      res = res + integrate_path_part(&
        a = this%start_points(i), b = this%end_points(i), &
        eps = this%eps, func = func, &
        PATH_PART_TYPE = this%path_type(i), INTEGRATION_TYPE = this%strategies(i), &
        min_delta = this%min_deltas(i), max_delta = this%max_deltas(i) &
      )
    end do
  end procedure integrate

  module procedure test
    type(integrator_obj) :: integrator

    real(dp), parameter :: eps = 1d-6

    block
      type(integrator_builder_obj) :: builder
      builder = integrator_builder_constructor()
      call builder%add_new_path_part(PATH_PART_TYPE_PARALLEL_X)
      call builder%part_integration_strategy(INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
      call builder%set_accuration(eps)

      call builder%start_point((- 1d0, - 1d0))
      call builder%end_point((1d0, - 1d0))

      integrator = builder%build()

      print *, - (cos((1d0, - 1d0)) - cos((- 1d0, - 1d0)))
      print *, integrator%integrate(func_sin)
      print *
    end block
    block
      type(integrator_builder_obj) :: builder
      builder = integrator_builder_constructor()
      call builder%add_new_path_part(PATH_PART_TYPE_PARALLEL_Y)
      call builder%part_integration_strategy(INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
      call builder%set_accuration(eps)

      call builder%start_point((1d0, - 1d0))
      call builder%end_point((1d0, 1d0))

      integrator = builder%build()

      print *, - (cos((1d0, 1d0)) - cos((1d0, - 1d0)))
      print *, integrator%integrate(func_sin)
      print *
    end block
    block
      type(integrator_builder_obj) :: builder
      builder = integrator_builder_constructor()
      call builder%add_new_path_part(PATH_PART_TYPE_PARALLEL_X)
      call builder%part_integration_strategy(INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
      call builder%set_accuration(eps)

      call builder%start_point((1d0, 1d0))
      call builder%end_point((- 1d0, 1d0))

      integrator = builder%build()

      print *, - (cos((- 1d0, 1d0)) - cos((1d0, 1d0)))
      print *, integrator%integrate(func_sin)
      print *
    end block
    block
      type(integrator_builder_obj) :: builder
      builder = integrator_builder_constructor()
      call builder%add_new_path_part(PATH_PART_TYPE_PARALLEL_Y)
      call builder%part_integration_strategy(INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
      call builder%set_accuration(eps)

      call builder%start_point((- 1d0, 1d0))
      call builder%end_point((- 1d0, - 1d0))

      integrator = builder%build()

      print *, - (cos((- 1d0, - 1d0)) - cos((- 1d0, 1d0)))
      print *, integrator%integrate(func_sin)
      print *
    end block
  contains
    complex(dp) function func_sin(x) result(res)
    implicit none (type, external)
      complex(dp), intent(in) :: x

      res = sin(x)
    end function func_sin
  end procedure test

  ! module procedure test
  !   type(integrator_obj) :: integrator

  !   block
  !     type(integrator_builder_obj) :: builder
  !     builder = integrator_builder_constructor()
  !     call builder%add_new_path_part(PATH_PART_TYPE_PARALLEL_X)
  !     call builder%start_point((0d0, 0d0))
  !     call builder%end_point((3.14159265358979d0, 0d0))
  !     call builder%part_integration_strategy(INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
  !     call builder%set_accuration(1d-3)
  !     integrator = builder%build()
  !   end block

  !   print *, "start integrating"
  !   print *, integrator%integrate(func)
  ! contains
  !   complex(dp) function func(x) result(res)
  !   implicit none (type, external)
  !     complex(dp), intent(in) :: x

  !     res = sin(x)
  !   end function func
  ! end procedure test

  ! module procedure test
  !   real(dp) :: a, b, eps

  !   real(dp) :: h
  !   integer(i4) :: n
  !   integer(i4) :: i
  !   complex(dp) :: res

  !   integer(i4) :: count

  !   eps = 1d-6

  !   a = 0d0; b = 3.14159265358979d0
  !   a = a; b = b + 4 * b
  !   a = - 40d0; b = 40d0
  !   a = 0d0; b = 2d0

  !   count = 0

  !   n = 30
  !   res = 0d0

  !   h = (b - a) / n
  !   do i = 1, N
  !     ! print *, a+h*(i-1), a+h*i
  !     res = res + gauss6_element(func, a + h * (i - 1), a + h * i)
  !     ! res = res + func(a + h * (i - 1)) * h
  !   end do

  !   print *, res, count

  !   res = 2d0

  !   do i = 1, 4

  !   if (i == 1) eps = 1d-1
  !   if (i == 2) eps = 1d-3
  !   if (i == 3) eps = 1d-6
  !   if (i == 4) eps = 1d-9

  !   print *
  !   print *, "eps", eps

  !   count = 0
  !   print *, abs(recalculation_template_method(func, a, b, eps, gauss3_element) - res)
  !   print *, "3", count
  !   count = 0
  !   print *, abs(recalculation_template_method(func, a, b, eps, gauss4_element) - res)
  !   print *, "4", count
  !   count = 0
  !   print *, abs(recalculation_template_method(func, a, b, eps, gauss5_element) - res)
  !   print *, "5", count
  !   count = 0
  !   print *, abs(recalculation_template_method(func, a, b, eps, gauss6_element) - res)
  !   print *, "6", count
  !   count = 0
  !   print *, abs(recalculation_template_method(func, a, b, eps, gauss7_element) - res)
  !   print *, "7", count
  !   count = 0
  !   print *, abs(recalculation_template_method(func, a, b, eps, gauss8_element) - res)
  !   print *, "8", count

  !   end do

  !   contains

  !   complex(dp) function func(x) result(res)
  !   implicit none (type, external)
  !     real(dp), intent(in) :: x

  !     count = count + 1

  !     res = sin(x)
  !     res = exp(- abs(x))
  !     res = x ** 7 / 16d0
  !   end function func
  ! end procedure test















  complex(dp) function integrate_path_part(&
    a, b, &
    eps, func, &
    PATH_PART_TYPE, INTEGRATION_TYPE, &
    min_delta, max_delta &
  ) result(res)
  implicit none (type, external)
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b
    real(dp), intent(in) :: eps
    procedure(integration_function_type) :: func
    integer(i4), intent(in) :: PATH_PART_TYPE
    integer(i4), intent(in) :: INTEGRATION_TYPE
    real(dp), intent(in) :: min_delta
    real(dp), intent(in) :: max_delta

    res = recalculation_template_method(&
      func = projection_function, &
      a = start_point_projection(a, b), b = end_point_projection(a, b), eps = eps, &
      integrate_element = integration_element(), &
      !integrate_element = gauss6_element, &
      integrate_projection_norm_delta = projection_normilized_delta, &
      min_delta = min_delta, max_delta = max_delta &
    )

    contains

    complex(dp) function projection_function(x) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: x

      select case (PATH_PART_TYPE)
      case (PATH_PART_TYPE_PARALLEL_X)
        res = projection_function_creator_for_PARALLEL_X(x = x, a = a, b = b, func = func)
      case (PATH_PART_TYPE_PARALLEL_Y)
        res = projection_function_creator_for_PARALLEL_Y(x = x, a = a, b = b, func = func)
      case default
        call error_assert(location = module_name // &
          ".integrate_path_part", &
          message = "unknown PATH_PART_TYPE", &
          condition = .false. &
        )
      end select
    end function projection_function
    pure complex(dp) function projection_normilized_delta(x, dx) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: x
      real(dp), intent(in) :: dx

      select case (PATH_PART_TYPE)
      case (PATH_PART_TYPE_PARALLEL_X)
        res = projection_normilized_delta_for_PARALLEL_X(x = x, dx = dx, a = a, b = b)
      case (PATH_PART_TYPE_PARALLEL_Y)
        res = projection_normilized_delta_for_PARALLEL_Y(x = x, dx = dx, a = a, b = b)
      case default
        call error_assert(location = module_name // &
          ".integrate_path_part", &
          message = "unknown PATH_PART_TYPE", &
          condition = .false. &
        )
      end select
    end function projection_normilized_delta
    pure real(dp) function start_point_projection(a, b) result(res)
    implicit none (type, external)
      complex(dp), intent(in) :: a
      complex(dp), intent(in) :: b

      select case (PATH_PART_TYPE)
      case (PATH_PART_TYPE_PARALLEL_X)
        res = projection_start_for_PARALLEL_X(a, b)
      case (PATH_PART_TYPE_PARALLEL_Y)
        res = projection_start_for_PARALLEL_Y(a, b)
      case default
        call error_assert(location = module_name // &
          ".integrate_path_part", &
          message = "unknown PATH_PART_TYPE", &
          condition = .false. &
        )
      end select
    end function start_point_projection
    pure real(dp) function end_point_projection(a, b) result(res)
    implicit none (type, external)
      complex(dp), intent(in) :: a
      complex(dp), intent(in) :: b

      select case (PATH_PART_TYPE)
      case (PATH_PART_TYPE_PARALLEL_X)
        res = projection_end_for_PARALLEL_X(a, b)
      case (PATH_PART_TYPE_PARALLEL_Y)
        res = projection_end_for_PARALLEL_Y(a, b)
      case default
        call error_assert(location = module_name // &
          ".integrate_path_part", &
          message = "unknown PATH_PART_TYPE", &
          condition = .false. &
        )
      end select
    end function end_point_projection





    pure function integration_element() result(res)
    implicit none (type, external)
      procedure(integrate_element_type), pointer :: res

      res => null()

      select case (INTEGRATION_TYPE)
      case (INTEGRATION_TYPE_GAUSS3_WITH_AUTO_RECALCULATION)
        res => gauss3_element
      case (INTEGRATION_TYPE_GAUSS4_WITH_AUTO_RECALCULATION)
        res => gauss4_element
      case (INTEGRATION_TYPE_GAUSS5_WITH_AUTO_RECALCULATION)
        res => gauss5_element
      case (INTEGRATION_TYPE_GAUSS6_WITH_AUTO_RECALCULATION)
        res => gauss6_element
      case (INTEGRATION_TYPE_GAUSS7_WITH_AUTO_RECALCULATION)
        res => gauss7_element
      case (INTEGRATION_TYPE_GAUSS8_WITH_AUTO_RECALCULATION)
        res => gauss8_element
      case default
        call error_assert(location = module_name // &
          ".integrate_path_part", &
          message = "unknown INTEGRATION_TYPE", &
          condition = .false. &
        )
      end select
    end function integration_element
  end function integrate_path_part















! ==========================================================
! projection_function_creater
! ==========================================================
  complex(dp) function projection_function_creator_for_PARALLEL_X(x, a, b, func) result(res)
  implicit none (type, external)
    real(dp), intent(in) :: x
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b
    procedure(integration_function_type) :: func

    complex(dp) :: v

    v = b
    v = x + aimag(a) * (0d0, 1d0)

    res = func(v)
  end function projection_function_creator_for_PARALLEL_X
  pure complex(dp) function projection_normilized_delta_for_PARALLEL_X(x, dx, a, b) result(res)
  implicit none (type, external)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: dx
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b

    res = x
    res = dx
    res = (1d0, 0d0)
    if (real(a) > real(b)) res = - res
  end function projection_normilized_delta_for_PARALLEL_X
  pure elemental real(dp) function projection_start_for_PARALLEL_X(a, b) result(res)
  implicit none (type, external)
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b

    res = min(real(a), real(b))
  end function projection_start_for_PARALLEL_X
  pure elemental real(dp) function projection_end_for_PARALLEL_X(a, b) result(res)
  implicit none (type, external)
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b

    res = max(real(a), real(b))
  end function projection_end_for_PARALLEL_X



  complex(dp) function projection_function_creator_for_PARALLEL_Y(x, a, b, func) result(res)
  implicit none (type, external)
    real(dp), intent(in) :: x
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b
    procedure(integration_function_type) :: func

    complex(dp) :: v

    v = b

    v = x * (0d0, 1d0) + real(a)

    res = func(v)
  end function projection_function_creator_for_PARALLEL_Y
  pure complex(dp) function projection_normilized_delta_for_PARALLEL_Y(x, dx, a, b) result(res)
  implicit none (type, external)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: dx
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b

    res = x
    res = dx
    res = (0d0, 1d0)
    if (aimag(a) > aimag(b)) res = - res
  end function projection_normilized_delta_for_PARALLEL_Y
  pure elemental real(dp) function projection_start_for_PARALLEL_Y(a, b) result(res)
  implicit none (type, external)
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b

    res = min(aimag(a), aimag(b))
  end function projection_start_for_PARALLEL_Y
  pure elemental real(dp) function projection_end_for_PARALLEL_Y(a, b) result(res)
  implicit none (type, external)
    complex(dp), intent(in) :: a
    complex(dp), intent(in) :: b

    res = max(aimag(a), aimag(b))
  end function projection_end_for_PARALLEL_Y















! ==========================================================
! integration_template_method
! ==========================================================
  complex(dp) function recalculation_template_method(&
    func, &
    a, b, eps, &
    integrate_element, integrate_projection_norm_delta, &
    min_delta, max_delta &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in) :: eps
    procedure(integrate_element_type) :: integrate_element
    procedure(projection_normilized_delta_type), optional :: integrate_projection_norm_delta
    real(dp), intent(in) :: min_delta
    real(dp), intent(in) :: max_delta

    real(dp) :: max_delta_half

    call error_assert(location = module_name // &
      ".recalculation_template_method", &
      message = "a <= b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )
    call error_assert(location = module_name // &
      ".recalculation_template_method", &
      message = "eps >= 1d-9", &
      condition = eps >= 1d-9 &
    )
    call error_assert(location = module_name // &
      ".recalculation_template_method", &
      message = "0d0 < min_delta .and. min_delta < max_delta", &
      condition = 0d0 < min_delta .and. min_delta < max_delta &
    )

    max_delta_half = max_delta * 0.5d0

    res = 0d0

    block
      real(dp) :: current_eps
      real(dp) :: x, h
      complex(dp) :: int_elem_1, int_elem_2

      x = a
      ! h = min(1d-3, eps)
      h = min_delta
      do while (x < b)
        int_elem_1 = integrate_element(func, x, x + h + h)
        int_elem_2 = integrate_element(func, x, x + h)
        int_elem_2 = int_elem_2 + integrate_element(func, x + h, x + h + h)
        current_eps = abs(int_elem_2 - int_elem_1) * 1.5d0

        do while (h >= min_delta .and. current_eps > eps)
          h = h * 0.5d0

          int_elem_1 = integrate_element(func, x, x + h + h)
          int_elem_2 = integrate_element(func, x, x + h)
          int_elem_2 = int_elem_2 + integrate_element(func, x + h, x + h + h)
          current_eps = abs(int_elem_2 - int_elem_1) * 1.5d0
        end do

        if (x + h + h > b) int_elem_2 = integrate_element(func, x, b)
        if (present(integrate_projection_norm_delta)) then
          res = res + int_elem_2 * integrate_projection_norm_delta(x = x, dx = h + h)
        else
          res = res + int_elem_2
        end if
        x = x + h + h

        if (current_eps < eps + eps + eps) h = min(max_delta_half, h + h)
        if (abs(x - b) <= 1d-7) exit
      end do
    end block
  end function recalculation_template_method

















! ==========================================================
! integration_element
! ==========================================================
  complex(dp) function gauss3_element(&
    func, a, b &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    real(dp), parameter :: domain(2) = [0.0000000000d0, 0.7745966692d0]
    real(dp), parameter :: koef(2) = [0.8888888888d0, 0.5555555556d0]

    call error_assert(location = module_name // &
      ".gauss3_element", &
      message = "a < b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )

    res = 0d0
    res = res + koef(2) * func(unnorm(a, b, - domain(2)))
    res = res + koef(1) * func(unnorm(a, b, domain(1)))
    res = res + koef(2) * func(unnorm(a, b, domain(2)))
    res = res * 0.5d0 * (b - a)

    contains
    pure elemental real(dp) function unnorm(a, b, v) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
      real(dp), intent(in) :: v

      res = (v + 1d0) * 0.5d0 * (b - a) + a
    end function unnorm
  end function gauss3_element
  complex(dp) function gauss4_element(&
    func, a, b &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    real(dp), parameter :: domain(2) = [0.8611363115d0, 0.3399810436d0]
    real(dp), parameter :: koef(2) = [0.3478548451d0, 0.6521451549d0]

    call error_assert(location = module_name // &
      ".gauss4_element", &
      message = "a < b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )

    res = 0d0
    res = res + koef(2) * func(unnorm(a, b, - domain(2)))
    res = res + koef(1) * func(unnorm(a, b, - domain(1)))
    res = res + koef(1) * func(unnorm(a, b, domain(1)))
    res = res + koef(2) * func(unnorm(a, b, domain(2)))
    res = res * 0.5d0 * (b - a)

    contains
    pure elemental real(dp) function unnorm(a, b, v) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
      real(dp), intent(in) :: v

      res = (v + 1d0) * 0.5d0 * (b - a) + a
    end function unnorm
  end function gauss4_element
  complex(dp) function gauss5_element(&
    func, a, b &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    real(dp), parameter :: domain(3) = [0.0000000000d0, 0.9061798459d0, 0.5384693101d0]
    real(dp), parameter :: koef(3) = [0.5688888888d0, 0.2369268851d0, 0.4786286705d0]

    call error_assert(location = module_name // &
      ".gauss5_element", &
      message = "a < b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )

    res = 0d0
    res = res + koef(3) * func(unnorm(a, b, - domain(3)))
    res = res + koef(2) * func(unnorm(a, b, - domain(2)))
    res = res + koef(1) * func(unnorm(a, b, domain(1)))
    res = res + koef(2) * func(unnorm(a, b, domain(2)))
    res = res + koef(3) * func(unnorm(a, b, domain(3)))
    res = res * 0.5d0 * (b - a)

    contains
    pure elemental real(dp) function unnorm(a, b, v) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
      real(dp), intent(in) :: v

      res = (v + 1d0) * 0.5d0 * (b - a) + a
    end function unnorm
  end function gauss5_element
  complex(dp) function gauss6_element(&
    func, a, b &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    real(dp), parameter :: domain(3) = [0.9324695142d0, 0.6612093864d0, 0.2386191861d0]
    real(dp), parameter :: koef(3) = [0.1713244924d0, 0.3607615730d0, 0.4679139346d0]

    call error_assert(location = module_name // &
      ".gauss6_element", &
      message = "a < b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )

    res = 0d0
    res = res + koef(3) * func(unnorm(a, b, - domain(3)))
    res = res + koef(2) * func(unnorm(a, b, - domain(2)))
    res = res + koef(1) * func(unnorm(a, b, - domain(1)))
    res = res + koef(1) * func(unnorm(a, b, domain(1)))
    res = res + koef(2) * func(unnorm(a, b, domain(2)))
    res = res + koef(3) * func(unnorm(a, b, domain(3)))
    res = res * 0.5d0 * (b - a)

    contains
    pure elemental real(dp) function unnorm(a, b, v) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
      real(dp), intent(in) :: v

      res = (v + 1d0) * 0.5d0 * (b - a) + a
    end function unnorm
  end function gauss6_element
  complex(dp) function gauss7_element(&
    func, a, b &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    real(dp), parameter :: domain(4) = [&
      0.0000000000d0, 0.405845151377d0, 0.741531185599d0, 0.949107912343d0 &
    ]
    real(dp), parameter :: koef(4) = [&
      0.417959183673d0, 0.381830050505d0, 0.279705391489d0, 0.129484966169d0 &
    ]

    call error_assert(location = module_name // &
      ".gauss7_element", &
      message = "a < b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )

    res = 0d0
    res = res + koef(4) * func(unnorm(a, b, - domain(4)))
    res = res + koef(3) * func(unnorm(a, b, - domain(3)))
    res = res + koef(2) * func(unnorm(a, b, - domain(2)))
    res = res + koef(1) * func(unnorm(a, b, domain(1)))
    res = res + koef(2) * func(unnorm(a, b, domain(2)))
    res = res + koef(3) * func(unnorm(a, b, domain(3)))
    res = res + koef(4) * func(unnorm(a, b, domain(4)))
    res = res * 0.5d0 * (b - a)

    contains
    pure elemental real(dp) function unnorm(a, b, v) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
      real(dp), intent(in) :: v

      res = (v + 1d0) * 0.5d0 * (b - a) + a
    end function unnorm
  end function gauss7_element
  complex(dp) function gauss8_element(&
    func, a, b &
  ) result(res)
  implicit none (type, external)
    procedure(integration_function_projection_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    real(dp), parameter :: domain(4) = [&
      0.1834346425d0, 0.5255324099d0, 0.7966664774d0, 0.9602898565d0 &
    ]
    real(dp), parameter :: koef(4) = [&
      0.3626837834d0, 0.3137066459d0, 0.2223810345d0, 0.1012285363d0 &
    ]

    call error_assert(location = module_name // &
      ".gauss8_element", &
      message = "a < b", &
      condition = .not. equals(&
        a, &
        b, &
        1d-7 &
      ) .and. a < b &
    )

    res = 0d0
    res = res + koef(4) * func(unnorm(a, b, - domain(4)))
    res = res + koef(3) * func(unnorm(a, b, - domain(3)))
    res = res + koef(2) * func(unnorm(a, b, - domain(2)))
    res = res + koef(1) * func(unnorm(a, b, - domain(1)))
    res = res + koef(1) * func(unnorm(a, b, domain(1)))
    res = res + koef(2) * func(unnorm(a, b, domain(2)))
    res = res + koef(3) * func(unnorm(a, b, domain(3)))
    res = res + koef(4) * func(unnorm(a, b, domain(4)))
    res = res * 0.5d0 * (b - a)

    contains
    pure elemental real(dp) function unnorm(a, b, v) result(res)
    implicit none (type, external)
      real(dp), intent(in) :: a
      real(dp), intent(in) :: b
      real(dp), intent(in) :: v

      res = (v + 1d0) * 0.5d0 * (b - a) + a
    end function unnorm
  end function gauss8_element
end submodule integrator_integrate_impl
