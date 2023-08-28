antlr_target(boole_lexer ${CMAKE_CURRENT_SOURCE_DIR}/antlr/boole_lexer.g4 LEXER PACKAGE booleguru::parse::antlr)
antlr_target(boole_parser ${CMAKE_CURRENT_SOURCE_DIR}/antlr/boole_parser.g4 PARSER
  PACKAGE booleguru::parse::antlr
  DEPENDS_ANTLR boole_lexer
  COMPILE_FLAGS -lib ${ANTLR_boole_lexer_OUTPUT_DIR})

antlr_target(smtlib2_lexer ${CMAKE_CURRENT_SOURCE_DIR}/antlr/smtlib2_lexer.g4 LEXER PACKAGE booleguru::parse::antlr)
antlr_target(smtlib2_parser ${CMAKE_CURRENT_SOURCE_DIR}/antlr/smtlib2_parser.g4 PARSER
  PACKAGE booleguru::parse::antlr
  DEPENDS_ANTLR smtlib2_lexer
  COMPILE_FLAGS -lib ${ANTLR_smtlib2_lexer_OUTPUT_DIR})

antlr_target(qcir_lexer ${CMAKE_CURRENT_SOURCE_DIR}/antlr/qcir_lexer.g4 LEXER PACKAGE booleguru::parse::antlr)
antlr_target(qcir_parser ${CMAKE_CURRENT_SOURCE_DIR}/antlr/qcir_parser.g4 PARSER
  PACKAGE booleguru::parse::antlr
  DEPENDS_ANTLR qcir_lexer
  COMPILE_FLAGS -lib ${ANTLR_qcir_lexer_OUTPUT_DIR})

antlr_target(cli_lexer ${CMAKE_CURRENT_SOURCE_DIR}/antlr/cli_lexer.g4 LEXER PACKAGE booleguru::parse::antlr)
antlr_target(cli_parser ${CMAKE_CURRENT_SOURCE_DIR}/antlr/cli_parser.g4 PARSER
  PACKAGE booleguru::parse::antlr
  DEPENDS_ANTLR cli_lexer
  COMPILE_FLAGS -lib ${ANTLR_cli_lexer_OUTPUT_DIR})

add_library(booleguru-parse-antlr OBJECT
  ${ANTLR_boole_lexer_CXX_OUTPUTS}
  ${ANTLR_boole_parser_CXX_OUTPUTS}
  ${ANTLR_smtlib2_lexer_CXX_OUTPUTS}
  ${ANTLR_smtlib2_parser_CXX_OUTPUTS}
  ${ANTLR_qcir_lexer_CXX_OUTPUTS}
  ${ANTLR_qcir_parser_CXX_OUTPUTS}
  ${ANTLR_cli_lexer_CXX_OUTPUTS}
  ${ANTLR_cli_parser_CXX_OUTPUTS}
)

target_link_libraries(booleguru-parse-antlr PRIVATE fmt)
target_link_libraries(booleguru-parse-antlr PUBLIC antlr4_static)
target_link_libraries(booleguru-parse-antlr PUBLIC booleguru-expression booleguru-lua)
target_include_directories(booleguru-parse-antlr PUBLIC
  ${ANTLR4_INCLUDE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)

set(SRCS
  src/base.cpp
  src/boole.cpp
  src/qdimacs.cpp
  src/aiger.cpp
  src/smtlib2.cpp
  src/luascript.cpp
  src/cli.cpp
)

if(TARGET booleguru-py-embed)
  list(APPEND SRCS "src/pythonscript.cpp")
endif()

add_library(booleguru-parse STATIC ${SRCS})

if(TARGET booleguru-py-embed)
  target_link_libraries(booleguru-parse PRIVATE booleguru-py-embed)
  target_compile_definitions(booleguru-parse PUBLIC BOOLEGURU_PARSE_PY)
endif()

target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_boole_lexer_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_boole_parser_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_smtlib2_lexer_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_smtlib2_parser_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_qcir_lexer_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_qcir_parser_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_cli_lexer_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_cli_parser_OUTPUT_DIR})

target_include_directories(booleguru-parse PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-parse PUBLIC booleguru-expression booleguru-util booleguru-lua)

target_link_libraries(booleguru-parse PRIVATE $<TARGET_OBJECTS:booleguru-parse-antlr>)
target_link_libraries(booleguru-parse PRIVATE antlr4_static fmt::fmt)
target_include_directories(booleguru-parse PRIVATE ${ANTLR4_INCLUDE_DIR})

# Not done automatically.
add_dependencies(booleguru-parse booleguru-parse-antlr)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
