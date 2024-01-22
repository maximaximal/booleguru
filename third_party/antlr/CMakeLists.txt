# -*- mode:cmake -*-
cmake_minimum_required (VERSION 3.15)
# 3.14 needed because of FetchContent
# 3.15 needed to avid spew of warnings related to overriding cl command line flags

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

option(WITH_LIBCXX "Building with clang++ and libc++(in Linux). To enable with: -DWITH_LIBCXX=On" Off)
option(WITH_STATIC_CRT "(Visual C++) Enable to statically link CRT, which avoids requiring users to install the redistribution package.
 To disable with: -DWITH_STATIC_CRT=Off" On)
option(DISABLE_WARNINGS "Suppress compiler warnings for all built ANTLR targets" OFF)

cmake_policy(SET CMP0091 NEW) # Enable use of CMAKE_MSVC_RUNTIME_LIBRARY
if(WITH_STATIC_CRT)
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
else()
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
endif(WITH_STATIC_CRT)

project(LIBANTLR4)

if(APPLE)
  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
endif()

file(STRINGS "VERSION" ANTLR_VERSION)

if(WITH_DEMO)
  # Java is not necessary if building without demos.
  find_package(Java COMPONENTS Runtime REQUIRED)

  if(NOT ANTLR_JAR_LOCATION)
    message(FATAL_ERROR "Missing antlr4.jar location. You can specify it's path using: -DANTLR_JAR_LOCATION=<path>")
  else()
    get_filename_component(ANTLR_NAME ${ANTLR_JAR_LOCATION} NAME_WE)
    if(NOT EXISTS "${ANTLR_JAR_LOCATION}")
      message(FATAL_ERROR "Unable to find ${ANTLR_NAME} in ${ANTLR_JAR_LOCATION}")
    else()
      message(STATUS "Found ${ANTLR_NAME}: ${ANTLR_JAR_LOCATION}")
    endif()
  endif()
endif(WITH_DEMO)

if(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
  set(MY_CXX_WARNING_FLAGS "  /W4")

  if(DISABLE_WARNINGS)
    set(MY_CXX_WARNING_FLAGS "  /w")
  endif()
else()
  set(MY_CXX_WARNING_FLAGS "  -Wall -pedantic -W")

  if(DISABLE_WARNINGS)
    set(MY_CXX_WARNING_FLAGS "  -w")
  endif()
endif()

# Define USE_UTF8_INSTEAD_OF_CODECVT macro.
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DUSE_UTF8_INSTEAD_OF_CODECVT")

# Initialize CXXFLAGS.
if(CMAKE_VERSION VERSION_GREATER 3.1.0)
  if(NOT DEFINED CMAKE_CXX_STANDARD)
    # only set CMAKE_CXX_STANDARD if not already set
    # this allows the standard to be set by the caller, for example with -DCMAKE_CXX_STANDARD:STRING=17
    set(CMAKE_CXX_STANDARD 17)
  endif()
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
  set(CMAKE_CXX_EXTENSIONS OFF)
else()
  set(CMAKE_CXX_FLAGS                "${CMAKE_CXX_FLAGS} -std=c++17")
  set(CMAKE_CXX_FLAGS_DEBUG          "${CMAKE_CXX_FLAGS_DEBUG} -std=c++17")
  set(CMAKE_CXX_FLAGS_MINSIZEREL     "${CMAKE_CXX_FLAGS_MINSIZEREL} -std=c++17")
  set(CMAKE_CXX_FLAGS_RELEASE        "${CMAKE_CXX_FLAGS_RELEASE} -std=c++17")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -std=c++17")
endif()

set(CMAKE_CXX_FLAGS                  "${CMAKE_CXX_FLAGS} ${MY_CXX_WARNING_FLAGS}")

if(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
  set(CMAKE_CXX_FLAGS_DEBUG          "${CMAKE_CXX_FLAGS_DEBUG} /Od /Zi /MP ${MY_CXX_WARNING_FLAGS}")
  set(CMAKE_CXX_FLAGS_MINSIZEREL     "${CMAKE_CXX_FLAGS_MINSIZEREL} /O1 /Oi /Ob2 /Gy /MP /DNDEBUG ${MY_CXX_WARNING_FLAGS}")
  set(CMAKE_CXX_FLAGS_RELEASE        "${CMAKE_CXX_FLAGS_RELEASE} /O2 /Oi /Ob2 /Gy /MP /DNDEBUG ${MY_CXX_WARNING_FLGAS}")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /O2 /Oi /Ob2 /Gy /MP /Zi ${MY_CXX_WARNING_FLAGS}")
else()
  set(CMAKE_CXX_FLAGS_DEBUG          "${CMAKE_CXX_FLAGS_DEBUG} -O0 -g ${MY_CXX_WARNING_FLAGS}")
  set(CMAKE_CXX_FLAGS_MINSIZEREL     "${CMAKE_CXX_FLAGS_MINSIZEREL} -Os -DNDEBUG ${MY_CXX_WARNING_FLAGS}")
  set(CMAKE_CXX_FLAGS_RELEASE        "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG ${MY_CXX_WARNING_FLGAS}")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -O2 -g ${MY_CXX_WARNING_FLAGS}")
endif()

# Compiler-specific C++17 activation.
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU" OR CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    execute_process(
        COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE GCC_VERSION)
    # Just g++-5.0 and greater contain <codecvt> header. (test in ubuntu)
    if(NOT (GCC_VERSION VERSION_GREATER 5.0 OR GCC_VERSION VERSION_EQUAL 5.0))
        message(FATAL_ERROR "${PROJECT_NAME} requires g++ 5.0 or greater.")
    endif ()
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND ANDROID)
	# Need -Os cflag and cxxflags here to work with exception handling on armeabi.
	# see  https://github.com/android-ndk/ndk/issues/573
	# and without -stdlib=libc++ cxxflags
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17 -stdlib=libc++")
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND ( CMAKE_SYSTEM_NAME MATCHES "Linux" OR CMAKE_SYSTEM_NAME MATCHES "FreeBSD") )
    execute_process(
        COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE CLANG_VERSION)
    if(NOT (CLANG_VERSION VERSION_GREATER 4.2.1 OR CLANG_VERSION VERSION_EQUAL 4.2.1))
        message(FATAL_ERROR "${PROJECT_NAME} requires clang 4.2.1 or greater.")
    endif()
    # You can use libc++ to compile this project when g++ is NOT greater than or equal to 5.0.
    if(WITH_LIBCXX)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
    endif()
elseif(MSVC_VERSION GREATER 1800 OR MSVC_VERSION EQUAL 1800)
  # Visual Studio 2012+ supports C++17 features
elseif(CMAKE_SYSTEM_NAME MATCHES "Emscripten")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17 -stdlib=libc++")
else()
    message(FATAL_ERROR "Your C++ compiler does not support C++17.")
endif()


add_subdirectory(runtime)
if(WITH_DEMO)
 add_subdirectory(demo)
endif(WITH_DEMO)

# Deleted the INSTALL stuff.
