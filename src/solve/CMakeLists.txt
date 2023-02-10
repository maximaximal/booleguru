set(SRCS
  src/result.cpp
)

add_library(booleguru-solve STATIC ${SRCS})

target_include_directories(booleguru-solve PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-solve PUBLIC booleguru-expression booleguru-transform booleguru-util)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-solve PRIVATE -fsanitize=address)
endif()
