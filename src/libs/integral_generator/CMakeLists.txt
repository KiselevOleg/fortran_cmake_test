# ==========================================================
# LIBRARY INTEGRAL_GENERATOR
# ==========================================================

set(LIBRARY_NAME "integral_generator")

set(SRC_FILES
  src/integrator.f90
  src/imp/integrator_builder_impl.f90
  src/imp/integrator_integrate_impl.f90
)

include(../../../cmake/for_libraries/generate_library_with_namespace.cmake)

target_link_libraries(${LIBRARY_NAME}
  PRIVATE
    system
)

if(ENABLE_TESTS)
  add_subdirectory(tests)
endif()
