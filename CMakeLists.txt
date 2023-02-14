cmake_policy(SET CMP0048 NEW)
cmake_policy(SET CMP0079 NEW)
project(booleguru
  VERSION 0.2)
cmake_minimum_required(VERSION 3.13)

set(CMAKE_CXX_STANDARD 20)

option(TESTS "Enable Test-Target (requires Catch2)" ON)

get_property(isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)

if(${CMAKE_BUILD_TYPE} MATCHES "Release")
  include(CheckIPOSupported)
  check_ipo_supported(RESULT LTO_SUPPORTED OUTPUT error)

  if( LTO_SUPPORTED )
    message(STATUS "IPO / LTO enabled")
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
  else()
    message(STATUS "IPO / LTO not supported: <${error}>")
  endif()
endif()

if(isMultiConfig)
    if(NOT "Sanitize" IN_LIST CMAKE_CONFIGURATION_TYPES)
        list(APPEND CMAKE_CONFIGURATION_TYPES Sanitize)
    endif()
else()
    set(allowedBuildTypes Sanitize Debug Release RelWithDebInfo MinSizeRel)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "${allowedBuildTypes}")

    if(CMAKE_BUILD_TYPE AND NOT CMAKE_BUILD_TYPE IN_LIST allowedBuildTypes)
        message(FATAL_ERROR "Invalid build type: ${CMAKE_BUILD_TYPE}")
    endif()
endif()

set(CMAKE_C_FLAGS_SANITIZE
    "${CMAKE_C_FLAGS_DEBUG} -fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined" CACHE STRING
    "Flags used by the C compiler for Sanitize build type or configuration." FORCE)

set(CMAKE_CXX_FLAGS_SANITIZE
    "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined" CACHE STRING
    "Flags used by the C++ compiler for Sanitize build type or configuration." FORCE)

set(CMAKE_EXE_LINKER_FLAGS_SANITIZE
    "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} -fsanitize=address -fsanitize=undefined" CACHE STRING
    "Linker flags to be used to create executables for Sanitize build type." FORCE)

set(CMAKE_SHARED_LINKER_FLAGS_SANITIZE
    "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} -fsanitize=address -fsanitize=undefined" CACHE STRING
    "Linker lags to be used to create shared libraries for Sanitize build type." FORCE)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

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

add_subdirectory(third_party)
add_subdirectory(src)

if(${CMAKE_BUILD_TYPE} MATCHES "Debug")
  if(MSVC)
	target_compile_options(booleguru-expression PRIVATE /W4 /WX)
	target_compile_options(booleguru-transform PRIVATE /W4 /WX)
	target_compile_options(booleguru-parse PRIVATE /W4 /WX)
	target_compile_options(booleguru-lua PRIVATE /W4 /WX)
	target_compile_options(booleguru-serialize PRIVATE /W4 /WX)
	target_compile_options(booleguru PRIVATE /W4 /WX)
  else()
	target_compile_options(booleguru-expression PRIVATE -Wall -Wextra -Wpedantic -Werror)
	target_compile_options(booleguru-transform PRIVATE -Wall -Wextra -Wpedantic -Werror)
	target_compile_options(booleguru-parse PRIVATE -Wall -Wextra -Wpedantic -Werror)
	target_compile_options(booleguru-lua PRIVATE -Wall -Wextra -Wpedantic -Werror)
	target_compile_options(booleguru-serialize PRIVATE -Wall -Wextra -Wpedantic -Werror)
	target_compile_options(booleguru PRIVATE -Wall -Wextra -Wpedantic -Werror)
  endif()
endif()
