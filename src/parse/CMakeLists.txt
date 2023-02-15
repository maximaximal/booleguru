antlr_target(boole_lexer ${CMAKE_CURRENT_SOURCE_DIR}/antlr/boole_lexer.g4 LEXER PACKAGE booleguru::parse::antlr)
antlr_target(boole_parser ${CMAKE_CURRENT_SOURCE_DIR}/antlr/boole_parser.g4 PARSER
  PACKAGE booleguru::parse::antlr
  DEPENDS_ANTLR boole_lexer
  COMPILE_FLAGS -lib ${ANTLR_boole_lexer_OUTPUT_DIR})

add_library(booleguru-parse-antlr OBJECT
  ${ANTLR_boole_lexer_CXX_OUTPUTS}
  ${ANTLR_boole_parser_CXX_OUTPUTS}
)

target_link_libraries(booleguru-parse-antlr PUBLIC antlr4_static)
target_link_libraries(booleguru-parse-antlr PUBLIC booleguru-expression booleguru-lua)
target_include_directories(booleguru-parse-antlr PUBLIC ${ANTLR4_INCLUDE_DIR})

set(SRCS
  src/base.cpp
  src/boole.cpp
  src/qdimacs.cpp
)

add_library(booleguru-parse STATIC ${SRCS})

target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_boole_lexer_OUTPUT_DIR})
target_include_directories(booleguru-parse PRIVATE SYSTEM ${ANTLR_boole_parser_OUTPUT_DIR})

target_include_directories(booleguru-parse PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-parse PUBLIC booleguru-expression booleguru-util booleguru-lua)

target_link_libraries(booleguru-parse PRIVATE $<TARGET_OBJECTS:booleguru-parse-antlr>)
target_link_libraries(booleguru-parse PRIVATE antlr4_static)
target_include_directories(booleguru-parse PRIVATE ${ANTLR4_INCLUDE_DIR})

# Not done automatically.
add_dependencies(booleguru-parse booleguru-parse-antlr)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
