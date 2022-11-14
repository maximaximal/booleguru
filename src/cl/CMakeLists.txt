set(SRCS
  src/ecl-wrapper.cpp
  src/cl-function.cpp
  src/cl-globals.cpp
)

set(LISP_SRCS
  ${CMAKE_CURRENT_SOURCE_DIR}/lisp/booleguru-cl-lisp.asd
  ${CMAKE_CURRENT_SOURCE_DIR}/lisp/booleguru-cl-lisp.lisp
)

# This is a rather big issue: :init-name does not work with ASDF and we only get
# the generated init names. Either we execute the same code again, or we just
# query the symbols and are done with it. This way seems to work well. We have
# to report this upstream though.

# Add include directories from expression.
get_target_property(BOOLEGURU_EXPRESSION_INCLUDES booleguru-expression INCLUDE_DIRECTORIES)
# Construct the compiler string for the include directories.
foreach(dir ${BOOLEGURU_EXPRESSION_INCLUDES})
  string(APPEND ECL_INCLUDES "-I${dir} ")
endforeach()

message(STATUS ${ECL_INCLUDES})

# The cut removes the mangled C++ name.
add_custom_command(
  DEPENDS ${LISP_SRCS}
  OUTPUT booleguru-cl-lisp--all-systems.a init-wrapper.h
  COMMAND ${ECL_BIN_PATH} --norc --eval "(require :asdf)" --eval "(setq compiler:*cc* \"${CMAKE_C_COMPILER} ${ECL_INCLUDES}\")" --eval "(push \"${CMAKE_CURRENT_SOURCE_DIR}/lisp/\" asdf:*central-registry*)" --eval "(asdf:make-build :booleguru-cl-lisp :type :static-library :move-here \"${CMAKE_CURRENT_BINARY_DIR}\" :init-name \"init_lib__BOOLEGURU_CL\" :monolithic t)" --eval "(quit)"
  COMMAND bash -c "printf \"#pragma once\\n #define ECL_INIT_LIB_FUNC $(strings booleguru-cl-lisp--all-systems.a | grep --max-count=1 \"^init_lib\")\\n\" > init-wrapper.h"
  VERBATIM
  COMMENT "Run ECL (Embeddable Common Lisp) to compile lispy dependencies")
# This goes in pair with `add_custom_command` above.
add_custom_target(booleguru-cl-lisp ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/booleguru-cl-lisp--all-systems.a)

add_library(booleguru-cl STATIC ${SRCS})

target_link_libraries(booleguru-cl PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/booleguru-cl-lisp--all-systems.a ${ECL_LIBRARIES})
add_dependencies(booleguru-cl booleguru-cl-lisp)

target_include_directories(booleguru-cl PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(booleguru-cl PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

target_include_directories(booleguru-cl PRIVATE SYSTEM ${ECL_INCLUDE_DIRS})

target_link_libraries(booleguru-cl PUBLIC booleguru-expression booleguru-util booleguru-transform)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
  target_link_libraries(booleguru-cl PRIVATE -fsanitize=address)
endif()
