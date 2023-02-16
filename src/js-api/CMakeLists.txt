set(SRCS
  src/js-main.cpp)

add_executable(booleguru ${SRCS})

set_target_properties(booleguru PROPERTIES LINK_FLAGS "--bind -O3 -fwasm-exceptions -s INITIAL_MEMORY=134217728 -sALLOW_MEMORY_GROWTH -SMODULARIZE -s 'EXPORT_NAME=\"createBooleguruModule\"'")

target_link_libraries(booleguru PUBLIC booleguru-parse booleguru-serialize booleguru-transform)
