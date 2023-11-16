set(TEST_SRCS
  test-parse-boole.cpp
  test-parse-qcir.cpp
  test-parse-aiger.cpp
  test-parse-qdimacs.cpp
  test-parse-smtlib2.cpp
  test-sexp-tracker.cpp
  test-antlr-boole-parse.cpp
)

add_executable(booleguru-parse-test ${TEST_SRCS})

target_link_libraries(booleguru-parse-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-parse-test PRIVATE booleguru-parse booleguru-cli)
target_link_libraries(booleguru-parse-test PRIVATE antlr4_static)

target_include_directories(booleguru-parse-test PRIVATE SYSTEM ${ANTLR_boole_lexer_OUTPUT_DIR})
target_include_directories(booleguru-parse-test PRIVATE SYSTEM ${ANTLR_boole_parser_OUTPUT_DIR})
target_include_directories(booleguru-parse-test PRIVATE SYSTEM ${ANTLR4_INCLUDE_DIR})

catch_discover_tests(booleguru-parse-test)
