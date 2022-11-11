set(SRCS
  src/qcir.cpp
)

add_library(booleguru-serialize STATIC ${SRCS})

target_include_directories(booleguru-serialize PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-serialize PUBLIC booleguru-expression)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
