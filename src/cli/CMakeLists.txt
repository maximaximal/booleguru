set(SRCS
  src/main.cpp
)

add_executable(booleguru ${SRCS})

target_include_directories(booleguru PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru PUBLIC booleguru-expression booleguru-cl booleguru-parse booleguru-transform booleguru-serialize)

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
  target_link_libraries(booleguru PRIVATE -fsanitize=address)
endif()
