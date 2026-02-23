submodule(integration_template_recalculation) integration_template_recalculation_impl
use system___assert, only: error_assert, error_not_assert, &
  warning_assert, warning_not_assert, &
  equals
implicit none (type, external)

  real(dp), parameter :: &
    MIN_STEP_DEFAULT = 1d-5, &
    MAX_STEP_DEFAULT = 0.4d0, &
    INIT_STEP_DEFAULT = 1d-2

  contains

  module procedure integration_template_recalculation_constructor
    if (present(min_step)) then
      this%min_step = min_step
    else
      this%min_step = MIN_STEP_DEFAULT
    end if
    if (present(max_step)) then
      this%max_step = max_step
    else
      this%max_step = MAX_STEP_DEFAULT
    end if
    if (present(init_step)) then
      this%init_step = init_step
    else
      this%init_step = INIT_STEP_DEFAULT
    end if

    associate(min_step => this%min_step, max_step => this%max_step, init_step => this%init_step)
      call error_assert(location = module_name // &
        ".constructor", &
        message = "0d0 < min_step .and. min_step <= init_step .and. init_step <= max_step", &
        condition = 0d0 < min_step .and. min_step <= init_step .and. init_step <= max_step &
      )
    end associate
  end procedure integration_template_recalculation_constructor

  recursive complex(dp) module function run( &
      this, &
      func, a, b, &
      integration_element, &
      eps, &
      normilized_delta &
    ) result(res)
  implicit none (type, external)
    class(integration_template_recalculation_obj), intent(inout) :: this
    procedure(projection_function_type) :: func
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    class(integration_element_obj), intent(inout) :: integration_element
    !> required accuracy
    real(dp), intent(in) :: eps
    procedure(normilized_delta_type), optional :: normilized_delta
  !module procedure run
    real(dp) :: max_step_half

    associate(min_step => this%min_step, max_step => this%max_step, init_step => this%init_step)
      call error_assert(location = module_name // &
        ".run", &
        message = "eps >= 1d-9", &
        condition = eps >= 1d-9 &
      )

      max_step_half = max_step * 0.5d0

      res = 0d0

      block
        real(dp) :: current_eps
        real(dp) :: x, h
        complex(dp) :: int_elem_1, int_elem_2

        x = a
        h = init_step
        do while (x < b)
          int_elem_1 = integration_element%run(func, x, x + h + h)
          int_elem_2 = integration_element%run(func, x, x + h)
          int_elem_2 = int_elem_2 + integration_element%run(func, x + h, x + h + h)
          current_eps = abs(int_elem_2 - int_elem_1) * 1.5d0

          do while (h >= min_step .and. current_eps > eps)
            h = h * 0.5d0

            int_elem_1 = integration_element%run(func, x, x + h + h)
            int_elem_2 = integration_element%run(func, x, x + h)
            int_elem_2 = int_elem_2 + integration_element%run(func, x + h, x + h + h)
            current_eps = abs(int_elem_2 - int_elem_1) * 1.5d0
          end do

          if (x + h + h > b) int_elem_2 = integration_element%run(func, x, b)
          if (present(normilized_delta)) then
            res = res + int_elem_2 * normilized_delta(x = x, dx = h + h)
          else
            res = res + int_elem_2
          end if
          x = x + h + h

          if (current_eps < eps + eps + eps) h = min(max_step_half, h + h)
          if (abs(x - b) <= 1d-7) exit
        end do
      end block
    end associate
  !end procedure run
  end function run
end submodule integration_template_recalculation_impl
