submodule(functions) functions_impl
use system___assert, only: error_assert, error_not_assert, &
  warning_assert, warning_not_assert, equals
implicit none (type, external)

  contains

  module procedure abs_x
    call error_not_assert(location = module_name // ".abs_x", &
      message = "x must be not 0", &
      condition = abs(x - 0d0) < 1d-5 &
    )

    res = x
    if (x < 0d0) res = - x
  end procedure abs_x
  module procedure neg_x
        call error_not_assert(location = module_name // ".abneg_x_x", &
      message = "x must be not 0", &
      condition = abs(x - 0d0) < 1d-5 &
    )

    res = - x
  end procedure neg_x
end submodule functions_impl
