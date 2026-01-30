submodule(math) math_impl
implicit none (type, external)

  contains
  module procedure max_new
    res = b
    if (a > b) res = a
  end procedure max_new
  module procedure min_new
    res = b
    if (a < b) res = a
  end procedure min_new
end submodule math_impl
