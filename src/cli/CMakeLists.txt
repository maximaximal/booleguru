set(SRCS
  src/argument.cpp
  src/input_file.cpp
  src/cli-processor.cpp
)

add_library(booleguru-cli ${SRCS})
target_include_directories(booleguru-cli PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(booleguru-cli PUBLIC booleguru-expression booleguru-parse booleguru-transform booleguru-serialize booleguru-lua)

target_link_libraries(booleguru-cli PRIVATE fmt)

if(TARGET booleguru-py-embed)
  target_link_libraries(booleguru-cli PUBLIC booleguru-py-embed)
endif()

add_executable(booleguru ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)
target_link_libraries(booleguru PUBLIC booleguru-cli)

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-cli PRIVATE -fsanitize=address)
  target_link_libraries(booleguru PRIVATE -fsanitize=address)
endif()

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
