# ==========================================================
# TESTS
# ==========================================================

add_pfunit_ctest(integration_path_part_test
  TEST_SOURCES integration_path_part_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
add_pfunit_ctest(integration_template_test
  TEST_SOURCES integration_template_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
add_pfunit_ctest(integration_element_test
  TEST_SOURCES integration_element_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
add_pfunit_ctest(integrator_test
  TEST_SOURCES integrator_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
