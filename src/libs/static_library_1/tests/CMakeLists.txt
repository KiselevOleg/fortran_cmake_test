# ==========================================================
# TESTS
# ==========================================================

add_pfunit_ctest(math_test
  TEST_SOURCES math_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
add_pfunit_ctest(functions_test
  TEST_SOURCES functions_test.pf
  LINK_LIBRARIES
    ${LIBRARY_NAME}
)
