# ==========================================================
# TESTS
# ==========================================================

add_pfunit_ctest(integrator_test
  TEST_SOURCES integrator_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
