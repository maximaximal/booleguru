set(TEST_SRCS
  test_prenex_quantifier.cpp
  test_eliminate_implication.cpp
  test_output_to_qdimacs.cpp
  test_tseitin.cpp
  test_polarity_extractor.cpp
)

add_executable(booleguru-transform-test ${TEST_SRCS})

target_link_libraries(booleguru-transform-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-transform-test PRIVATE booleguru-transform booleguru-parse)

catch_discover_tests(booleguru-transform-test)
