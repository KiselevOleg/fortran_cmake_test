# ==========================================================
# TESTS
# ==========================================================

set(LIBRARY_NAME "static_library_1")

add_executable(math_test math_test.f90)
target_link_libraries(math_test PRIVATE ${LIBRARY_NAME})
add_test(NAME math_test COMMAND $<TARGET_FILE:math_test>)

add_executable(functions_test functions_test.f90)
target_link_libraries(functions_test PRIVATE ${LIBRARY_NAME})
add_test(NAME functions_test COMMAND $<TARGET_FILE:functions_test>)
