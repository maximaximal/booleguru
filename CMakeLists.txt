cmake_minimum_required(VERSION 3.13)
cmake_policy(SET CMP0048 NEW)
cmake_policy(SET CMP0079 NEW)
project(booleguru
  VERSION 0.3)

add_compile_definitions(BOOLEGURU_VERSION="${CMAKE_PROJECT_VERSION}")

set(CMAKE_CXX_STANDARD 20)

option(TESTS "Enable Test-Target (requires Catch2)" ON)
option(DOCS "Enable Generation of Documentation (requires Doxygen, Sphinx, and Breathe)" ON)
option(FORCE_COLORED_OUTPUT "Always produce ANSI-colored output (GNU/Clang only)." TRUE)

get_property(isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)

# Credit to this article:
# https://medium.com/@alasher/colored-c-compiler-output-with-ninja-clang-gcc-10bfe7f2b949
if(${FORCE_COLORED_OUTPUT})
    if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
       add_compile_options(-fdiagnostics-color=always)
    elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
       add_compile_options(-fcolor-diagnostics)
    endif()
endif()

# Credit to HeavyDB's CMake scripts (Apache License 2):
# https://github.com/heavyai/heavydb/blob/master/CMakeLists.txt#L119
macro(set_alternate_linker linker)
  find_program(LINKER_EXECUTABLE ld.${USE_ALTERNATE_LINKER} ${USE_ALTERNATE_LINKER})
  if(LINKER_EXECUTABLE)
    if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" AND "${CMAKE_CXX_COMPILER_VERSION}" VERSION_LESS 12.0.0)
      add_link_options("-ld-path=${USE_ALTERNATE_LINKER}")
    elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" AND "${CMAKE_CXX_COMPILER_VERSION}" VERSION_LESS 12.1.0 AND "${USE_ALTERNATE_LINKER}" STREQUAL "mold")
      # LINKER_EXECUTABLE will be a full path to ld.mold, so we replace the end of the path, resulting in the relative
      # libexec/mold dir, and tell GCC to look there first for an override version of executables, in this case, ld
      string(REPLACE "bin/ld.mold" "libexec/mold" PATH_TO_LIBEXEC_MOLD ${LINKER_EXECUTABLE})
      add_link_options("-B${PATH_TO_LIBEXEC_MOLD}")
    else()
      add_link_options("-fuse-ld=${USE_ALTERNATE_LINKER}")
    endif()
  else()
    set(USE_ALTERNATE_LINKER "" CACHE STRING "Use alternate linker" FORCE)
  endif()
endmacro()

set(USE_ALTERNATE_LINKER "" CACHE STRING "Use alternate linker. Leave empty for system default; alternatives are 'gold', 'lld', 'bfd', 'mold'")
if(NOT "${USE_ALTERNATE_LINKER}" STREQUAL "")
  set_alternate_linker(${USE_ALTERNATE_LINKER})
endif()

if(EMSCRIPTEN)
  set(CMAKE_CXX_FLAGS "-fwasm-exceptions")
  set(CMAKE_C_FLAGS "-fwasm-exceptions")
endif()

# Here, we don't want to enable LTO for FuzzDebug, as it makes recompiles very expensive.
if(${CMAKE_BUILD_TYPE} MATCHES "Release" OR ${CMAKE_BUILD_TYPE} MATCHES "Fuzz")
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

    if(NOT "Fuzz" IN_LIST CMAKE_CONFIGURATION_TYPES)
        list(APPEND CMAKE_CONFIGURATION_TYPES Fuzz)
    endif()

    if(NOT "FuzzDebug" IN_LIST CMAKE_CONFIGURATION_TYPES)
        list(APPEND CMAKE_CONFIGURATION_TYPES FuzzDebug)
    endif()
else()
    set(allowedBuildTypes Sanitize Fuzz FuzzDebug Debug Release RelWithDebInfo MinSizeRel)
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

set(CMAKE_C_FLAGS_FUZZ
    "${CMAKE_C_FLAGS_DEBUG} -fsanitize=address -O2 -fno-omit-frame-pointer -fsanitize=undefined" CACHE STRING
    "Flags used by the C compiler for Sanitize build type or configuration." FORCE)

set(CMAKE_CXX_FLAGS_FUZZ
    "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address -O2 -fno-omit-frame-pointer -fsanitize=undefined" CACHE STRING
    "Flags used by the C++ compiler for Sanitize build type or configuration." FORCE)

set(CMAKE_EXE_LINKER_FLAGS_FUZZ
    "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} -fsanitize=address -O2 -fsanitize=undefined" CACHE STRING
    "Linker flags to be used to create executables for Sanitize build type." FORCE)

set(CMAKE_SHARED_LINKER_FLAGS_FUZZ
    "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} -fsanitize=address -O2 -fsanitize=undefined" CACHE STRING
    "Linker lags to be used to create shared libraries for Sanitize build type." FORCE)

set(CMAKE_C_FLAGS_FUZZDEBUG
    "${CMAKE_C_FLAGS_DEBUG} -fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined" CACHE STRING
    "Flags used by the C compiler for Sanitize build type or configuration." FORCE)

set(CMAKE_CXX_FLAGS_FUZZDEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined" CACHE STRING
    "Flags used by the C++ compiler for Sanitize build type or configuration." FORCE)

set(CMAKE_EXE_LINKER_FLAGS_FUZZDEBUG
    "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} -fsanitize=address -fsanitize=undefined" CACHE STRING
    "Linker flags to be used to create executables for Sanitize build type." FORCE)

set(CMAKE_SHARED_LINKER_FLAGS_FUZZDEBUG
    "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} -fsanitize=address -fsanitize=undefined" CACHE STRING
    "Linker lags to be used to create shared libraries for Sanitize build type." FORCE)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

if(${CMAKE_BUILD_TYPE} MATCHES "Fuzz" OR {CMAKE_BUILD_TYPE} MATCHES "FuzzDebug")
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message(STATUS "Compiler is Clang, the Fuzz target works.")
        if(TESTS)
            message(STATUS "Disabling TESTS because of fuzz target.")
            set(TESTS OFF)
        endif()
    else()
        message(ERROR "Compiler is not clang, but " ${CMAKE_CXX_COMPILER_ID} "! Fuzz target cannot work!")
    endif()
endif()

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

set(PYBIND11_FINDPYTHON ON)
find_package(pybind11 CONFIG QUIET)

if(pybind11_FOUND)
  message(STATUS "Pybind11 found! Python bindings enabled.")
else()
  message(STATUS "Pybind11 not found! Install the package for Python bindings.")
endif()

add_subdirectory(third_party)
add_subdirectory(src)

if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
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

if(${CMAKE_BUILD_TYPE} MATCHES "Fuzz" OR ${CMAKE_BUILD_TYPE} MATCHES "FuzzDebug")
  target_compile_options(antlr4_static PRIVATE -fsanitize=fuzzer,address,undefined)
  target_compile_options(booleguru-expression PRIVATE -fsanitize=fuzzer,address,undefined)
  target_compile_options(booleguru-transform PRIVATE -fsanitize=fuzzer,address,undefined)
  target_compile_options(booleguru-parse PRIVATE -fsanitize=fuzzer,address,undefined)
  target_compile_options(booleguru-lua PRIVATE -fsanitize=fuzzer,address,undefined)
  target_compile_options(booleguru-serialize PRIVATE -fsanitize=fuzzer,address,undefined)
  target_compile_options(booleguru-fuzz PRIVATE -fsanitize=fuzzer,address,undefined)

  target_link_options(booleguru-fuzz PRIVATE -fsanitize=fuzzer,address,undefined)
  target_link_libraries(booleguru-fuzz PRIVATE -fsanitize=fuzzer,address,undefined)
  target_link_libraries(booleguru-print-corpus PRIVATE -fsanitize=address,undefined)
endif()

include(GNUInstallDirs)

if(DOCS)
  add_subdirectory(docs)
endif()
