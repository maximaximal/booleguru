set(SRCS
  src/base.cpp
  src/boole.cpp
)

add_library(booleguru-parse STATIC ${SRCS})

target_include_directories(booleguru-parse PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-parse PUBLIC booleguru-expression booleguru-util booleguru-cl)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
