submodule(integrator) integrator_part_obj_impl
use system___assert, only: error_assert, error_not_assert, &
  warning_assert, warning_not_assert, &
  equals
implicit none (type, external)

contains
  module procedure part_obj_destructor
    if (allocated(this%path_part)) deallocate(this%path_part)
    if (allocated(this%integrate_template)) deallocate(this%integrate_template)
    if (allocated(this%integrate_element)) deallocate(this%integrate_element)
  end procedure part_obj_destructor
end submodule integrator_part_obj_impl
