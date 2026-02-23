submodule(integrator) integrator_builder_impl
use system___assert, only: error_assert, error_not_assert, &
  warning_assert, warning_not_assert, &
  equals
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_signaling_nan, &
  ieee_is_nan
implicit none (type, external)
  integer(i4), parameter :: INTEGRATION_TYPE_INVALID = - 1

  real(dp), parameter :: MIN_DELTA_DEFAULT = 1d-5, MAX_DELTA_DEFAULT = 0.4d0

  contains

  module procedure integrator_builder_constructor
    this%start_points = dynamic_array_complex64_constructor()
    this%end_points = dynamic_array_complex64_constructor()
    this%path_type = dynamic_array_int32_constructor()
    this%strategies = dynamic_array_int32_constructor()

    this%min_deltas = dynamic_array_real64_constructor()
    this%max_deltas = dynamic_array_real64_constructor()

    this%eps = 1d-3
    this%max_abs_point_coordinate_value = 1000d0
  end procedure integrator_builder_constructor

  module procedure build
    ! integer(i4) :: i

    call check_correct(this)

    associate(part_size => this%start_points%get_size())
      ! allocate(res%start_points(part_size))
      ! allocate(res%end_points(part_size))
      ! allocate(res%path_type(part_size))
      ! allocate(res%strategies(part_size))

      ! do concurrent(i = 1:part_size)
      !   res%start_points(i) = this%start_points%get_value(i)
      !   res%end_points(i) = this%end_points%get_value(i)
      !   res%path_type(i) = this%path_type%get_value(i)
      !   res%strategies(i) = this%strategies%get_value(i)
      ! end do

      res%start_points = this%start_points%get_raw_array()
      res%end_points = this%end_points%get_raw_array()
      res%path_type = this%path_type%get_raw_array()
      res%strategies = this%strategies%get_raw_array()

      res%min_deltas = this%min_deltas%get_raw_array()
      res%max_deltas = this%max_deltas%get_raw_array()
    end associate

    res%eps = this%eps
  end procedure build


  module procedure add_new_path_part
    associate(part_size => this%start_points%get_size())
      if (part_size /= 0) then
        call this%start_points%add_last(this%end_points%get_value(part_size))
        call this%end_points%add_last(this%start_points%get_value(part_size + 1))
        call this%strategies%add_last(this%strategies%get_value(part_size))

        call this%min_deltas%add_last(this%min_deltas%get_value(part_size))
        call this%max_deltas%add_last(this%max_deltas%get_value(part_size))
      else
        block
          complex(dp) :: x
          x = ieee_value(0d0, ieee_quiet_nan)
          call this%start_points%add_last(x)
          call this%end_points%add_last(x)
          call this%strategies%add_last(INTEGRATION_TYPE_INVALID)

          call this%min_deltas%add_last(MIN_DELTA_DEFAULT)
          call this%max_deltas%add_last(MAX_DELTA_DEFAULT)
        end block
      end if

      call this%path_type%add_last(PATH_PART_TYPE)
    end associate
  end procedure add_new_path_part
  module procedure start_point
    associate(part_size => this%start_points%get_size())
      call error_not_assert(location = module_name // ".start_point", &
        message = "part_size == 0", &
        condition = part_size == 0 &
      )
      call this%start_points%set_value(part_size, value)
    end associate
  end procedure start_point
  module procedure end_point
    associate(part_size => this%start_points%get_size())
      call error_not_assert(location = module_name // ".end_point", &
        message = "part_size == 0", &
        condition = part_size == 0 &
      )
      call this%end_points%set_value(part_size, value)
    end associate
  end procedure end_point
  module procedure part_integration_strategy
    associate(part_size => this%start_points%get_size())
      call error_not_assert(location = module_name // ".part_integration_strategy", &
        message = "part_size == 0", &
        condition = part_size == 0 &
      )
      call this%strategies%set_value(part_size, INTEGRATION_TYPE)
    end associate
  end procedure part_integration_strategy
  module procedure min_delta
    associate(part_size => this%start_points%get_size())
      call error_not_assert(location = module_name // ".min_delta", &
        message = "part_size == 0", &
        condition = part_size == 0 &
      )
      call this%min_deltas%set_value(part_size, value)
    end associate
  end procedure min_delta
  module procedure max_delta
    associate(part_size => this%start_points%get_size())
      call error_not_assert(location = module_name // ".max_delta", &
        message = "part_size == 0", &
        condition = part_size == 0 &
      )
      call this%max_deltas%set_value(part_size, value)
    end associate
  end procedure max_delta

  module procedure set_accuration
    associate(part_size => this%start_points%get_size())
      this%eps = eps
    end associate
  end procedure set_accuration





  pure subroutine check_correct(this)
  implicit none (type, external)
    class(integrator_builder_obj), intent (in) :: this

    integer(i4) :: i

    associate(part_size => this%start_points%get_size())
      call error_not_assert(location = module_name // ".check_correct", &
        message = "integration part size must be >= 1", &
        condition = part_size == 0 &
      )

      do concurrent(i = 1:part_size)
        block
          complex(dp) :: x, y
          x = this%start_points%get_value(i)
          call error_not_assert(location = module_name // ".check_correct", &
            message = "start and end point of an integration part must be different", &
            condition = ieee_is_nan(real(x)) .or. ieee_is_nan(aimag(x))&
          )

          y = this%end_points%get_value(i)
          select case (this%path_type%get_value(i))
          case (PATH_PART_TYPE_PARALLEL_X)
            call error_assert(location = module_name // ".check_correct", &
              message = "PATH_PART_TYPE_PARALLEL_X requires Im(start_point) == Im(end_point)", &
              condition = equals(abs(aimag(x - y)), 0d0, 1d-12) &
            )
          case (PATH_PART_TYPE_PARALLEL_Y)
            call error_assert(location = module_name // ".check_correct", &
              message = "PATH_PART_TYPE_PARALLEL_Y requires Re(start_point) == Re(end_point)", &
              condition = equals(abs(real(x - y)), 0d0, 1d-12) &
            )
          case default
          end select
        end block
        call error_not_assert(location = module_name // ".check_correct", &
          message = "start and end point of an integration part must be different", &
          condition = equals(&
            this%start_points%get_value(i), &
            this%end_points%get_value(i), &
            1d-7&
          )&
        )
        call error_not_assert(location = module_name // ".check_correct", &
          message = "start and end points are too large    " // &
            "max(|start|, |end|) > max_abs_point_coordinate_value", &
          condition = max(&
              abs(this%start_points%get_value(i)), &
              abs(this%end_points%get_value(i)) &
            ) > this%max_abs_point_coordinate_value &
        )
        call error_assert(location = module_name // ".check_correct", &
          message = "this%path_type%get_value(i) /= PATH_PART_TYPE_INVALID", &
          condition = 0 <= this%path_type%get_value(i) .and. this%path_type%get_value(i) <= 1&
        )
        call error_assert(location = module_name // ".check_correct", &
          message = "this%strategies%get_value(i) /= INTEGRATION_TYPE_INVALID", &
          condition = 0 <= this%strategies%get_value(i) .and. this%strategies%get_value(i) <= 5 &
        )

        call error_not_assert(location = module_name // ".check_correct", &
          message = "min_delta == max_delta", &
          condition = equals(&
            this%min_deltas%get_value(i), &
            this%max_deltas%get_value(i), &
            1d-7&
          )&
        )
        call error_assert(location = module_name // ".check_correct", &
          message = "min_delta > 1d-12", &
          condition = this%min_deltas%get_value(i) > 1d-12 &
        )
        call error_assert(location = module_name // ".check_correct", &
          message = "min_delta < 1d10", &
          condition = this%min_deltas%get_value(i) < 1d10 &
        )
        call error_assert(location = module_name // ".check_correct", &
          message = "max_delta > 1d-12", &
          condition = this%max_deltas%get_value(i) > 1d-12 &
        )
        call error_assert(location = module_name // ".check_correct", &
          message = "max_delta < 1d10", &
          condition = this%max_deltas%get_value(i) < 1d10 &
        )
      end do

      call error_assert(location = module_name // ".check_correct", &
        message = "eps >= 1d-9", &
        condition = this%eps >= 1d-9 &
      )
      call error_assert(location = module_name // ".check_correct", &
        message = "eps <= 1d0", &
        condition = this%eps <= 1d0 &
      )
    end associate
  end subroutine check_correct
end submodule integrator_builder_impl
