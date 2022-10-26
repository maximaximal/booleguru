set(SRCS
  src/ecl-wrapper.cpp
)

set(LISP_SRCS
  ${CMAKE_CURRENT_SOURCE_DIR}/lisp/booleguru-cl-lisp.asd
  ${CMAKE_CURRENT_SOURCE_DIR}/lisp/booleguru-cl-lisp.lisp
)

add_custom_command(
  DEPENDS ${LISP_SRCS}
  OUTPUT booleguru-cl-lisp--all-systems.a
  COMMAND ${ECL_BIN_PATH} --norc --eval "(require :asdf)" --eval "(push \"${CMAKE_CURRENT_SOURCE_DIR}/lisp/\" asdf:*central-registry*)" --eval "(" --eval "(asdf:make-build :booleguru-cl-lisp :init-name \"init_lib__BOOLEGURU_CL\" :type :static-library :move-here \"${CMAKE_CURRENT_BINARY_DIR}\" :monolithic t)" --eval "(quit)"
  VERBATIM
  COMMENT "Run ECL (Embeddable Common Lisp) to compile lispy dependencies")
# This goes in pair with `add_custom_command` above.
add_custom_target(booleguru-cl-lisp ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/booleguru-cl-lisp--all-systems.a)

add_library(booleguru-cl STATIC ${SRCS})

target_link_libraries(booleguru-cl PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/booleguru-cl-lisp--all-systems.a ${ECL_LIBRARIES})
add_dependencies(booleguru-cl booleguru-cl-lisp)

target_include_directories(booleguru-cl PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_include_directories(booleguru-cl PRIVATE SYSTEM ${ECL_INCLUDE_DIRS})

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
