project(booleguru)
cmake_minimum_required(VERSION 3.2)

set(CMAKE_CXX_STANDARD 23)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(ECL REQUIRED)

find_package(Catch2)
if(TARGET Catch2::Catch2)
  message(STATUS "Catch2 version " ${Catch2_VERSION} " was found! Activating tests.")
  include(CTest)
  include(Catch)
endif()

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR})

add_subdirectory(src)

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
  if(MSVC)
	target_compile_options(booleguru-expression PRIVATE /W4 /WX)
	target_compile_options(booleguru-transform PRIVATE /W4 /WX)
  else()
	target_compile_options(booleguru-expression PRIVATE -Wall -Wextra -Wpedantic -Werror -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined)
	target_compile_options(booleguru-transform PRIVATE -Wall -Wextra -Wpedantic -Werror -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined)
	target_compile_options(booleguru-parse PRIVATE -Wall -Wextra -Wpedantic -Werror -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined)
	target_compile_options(booleguru-cl PRIVATE -Wall -Wextra -Wpedantic -Werror -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined)
	target_compile_options(booleguru-serialize PRIVATE -Wall -Wextra -Wpedantic -Werror -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined)
	target_compile_options(booleguru PRIVATE -Wall -Wextra -Wpedantic -Werror -fno-omit-frame-pointer -fsanitize=address -fsanitize=undefined)
  endif()
endif()
