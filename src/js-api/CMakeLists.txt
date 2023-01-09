set(SRCS
  src/js-main.cpp)

add_executable(booleguru ${SRCS})

set_target_properties(booleguru PROPERTIES LINK_FLAGS "--bind -s -O3 -sASSERTIONS -s INITIAL_MEMORY=134217728 -sALLOW_MEMORY_GROWTH -sASYNCIFY -s ASYNCIFY_STACK_SIZE=6000000")

target_link_libraries(booleguru PUBLIC booleguru-parse booleguru-serialize booleguru-transform)

