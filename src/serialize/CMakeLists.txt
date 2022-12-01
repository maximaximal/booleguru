set(SRCS
  src/qcir.cpp
)

add_library(booleguru-serialize OBJECT ${SRCS})

target_include_directories(booleguru-serialize PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-serialize PUBLIC booleguru-expression booleguru-transform)

set_property(TARGET booleguru-serialize PROPERTY POSITION_INDEPENDENT_CODE ON)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
endif()
