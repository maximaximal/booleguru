project(booleguru)
cmake_minimum_required(VERSION 3.2)

set(CMAKE_CXX_STANDARD 23)

option(TESTS "Enable Test-Target (requires Catch2)" ON)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(ECL REQUIRED)

if(TESTS)
    find_package(Catch2)
    if(TARGET Catch2::Catch2)
        message(STATUS "Catch2 version " ${Catch2_VERSION} " was found! Activating tests.")
        include(CTest)
        include(Catch)
    endif()
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR})
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_C_FLAGS_RELEASE "-O3")

if(EMSCRIPTEN)
  add_link_options("-s ALLOW_MEMORY_GROWTH=1 -s TOTAL_MEMORY=2000MB")
endif()

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
	target_link_options(booleguru PRIVATE -fsanitize=address -fsanitize=undefined)
  endif()
endif()
