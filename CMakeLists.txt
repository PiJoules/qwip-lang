cmake_minimum_required(VERSION 3.4.3)
project(qwip)

option(
  SANITIZER
  "Define the sanitizer used to build qwip. Options are ADDRESS, THREAD, UNDEFINED. Defaults to an empty string to indicate no sanitizer."
  "")
option(
  TEST_WITH_VALGRIND
  "Run tests against valgrind for memory checking. Defaults to OFF."
  OFF)
option(
  CODE_COVERAGE
  "Enable code coverage reporting."
  OFF)

string(TOUPPER ${SANITIZER} SANITIZER)

find_package(LLVM REQUIRED CONFIG)
find_package(PythonInterp 2.7 REQUIRED)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set the compile flags.
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "LLVM C++ Compilation Flags: ${LLVM_DEFINITIONS}")

set(SRCS
  src/Compiler.cpp
  src/Lexer.cpp
  src/Parser.cpp)

add_library(qwip-lib ${SRCS})
target_compile_options(qwip-lib
  PUBLIC -Werror
  PUBLIC -Wall
  PUBLIC -Wextra
  PUBLIC -Wconversion
  PUBLIC -fno-exceptions
  PUBLIC -fno-rtti
  PUBLIC -std=c++17
  PUBLIC -Wno-unused-parameter  # For warnings from LLVM headers
  PUBLIC -UNDEBUG  # We will always keep assertions on!
)

# Now build our tools.
add_executable(qwip src/qwip.cpp)
target_link_libraries(qwip PUBLIC qwip-lib)
add_executable(qwip-unittests src/qwip-unittests.cpp)
target_link_libraries(qwip-unittests PUBLIC qwip-lib)

if (CODE_COVERAGE)
  message(STATUS "Building with code coverage")
  target_compile_options(qwip-lib
    PUBLIC -fprofile-arcs
    PUBLIC -ftest-coverage
    PUBLIC -g
  )
  set_property(TARGET qwip-lib APPEND_STRING PROPERTY LINK_FLAGS " -fprofile-arcs -ftest-coverage")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fprofile-arcs -ftest-coverage")
  set_property(TARGET qwip-unittests APPEND_STRING PROPERTY LINK_FLAGS " -fprofile-arcs -ftest-coverage")
endif()

if (${SANITIZER} STREQUAL "ADDRESS")
  message(STATUS "Building with ASan")
  target_compile_options(qwip-lib PUBLIC "-fsanitize=address,pointer-compare,pointer-subtract")
  set_property(TARGET qwip-lib APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=address,pointer-compare,pointer-subtract")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=address,pointer-compare,pointer-subtract")
  set_property(TARGET qwip-unittests APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=address,pointer-compare,pointer-subtract")
elseif(${SANITIZER} STREQUAL "THREAD")
  message(STATUS "Building with TSan")
  target_compile_options(qwip-lib PUBLIC "-fsanitize=thread")
  set_property(TARGET qwip-lib APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
  set_property(TARGET qwip-unittests APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
elseif(${SANITIZER} STREQUAL "UNDEFINED")
  message(STATUS "Building with UBSan")
  target_compile_options(qwip-lib PUBLIC "-fsanitize=undefined")
  set_property(TARGET qwip-lib APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=undefined")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=undefined")
  set_property(TARGET qwip-unittests APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=undefined")
endif()

# Link against the standalone libLLVM shared library
llvm_config(qwip-lib USE_SHARED)
llvm_config(qwip USE_SHARED)
llvm_config(qwip-unittests USE_SHARED)

# Testing
ENABLE_TESTING()
include(CTest)

if (${TEST_WITH_VALGRIND})
  message(STATUS "Testing with valrgind")
endif()

# Unittests
if (${TEST_WITH_VALGRIND})
  add_test("run-qwip-unittests" "valgrind" "--leak-check=full" "--error-exitcode=1" "${CMAKE_BINARY_DIR}/qwip-unittests")
else()
  add_test("run-qwip-unittests" "${CMAKE_BINARY_DIR}/qwip-unittests")
endif()

# The regular lit tests
file(GLOB files "${CMAKE_SOURCE_DIR}/tests/*.qw")
foreach(path ${files})
  get_filename_component(filename ${path} NAME)
  if (${TEST_WITH_VALGRIND})
    add_test("run-lit-test-${filename}" "python" "${CMAKE_SOURCE_DIR}/run-examples.py" "-q" "${CMAKE_BINARY_DIR}/qwip" ${path} "--valgrind")
  else()
    add_test("run-lit-test-${filename}" "python" "${CMAKE_SOURCE_DIR}/run-examples.py" "-q" "${CMAKE_BINARY_DIR}/qwip" ${path})
  endif()
endforeach()

# Examples
file(GLOB files "${CMAKE_SOURCE_DIR}/examples/*.qw")
foreach(path ${files})
  get_filename_component(filename ${path} NAME)
  if (${TEST_WITH_VALGRIND})
    add_test("run-lit-test-example-${filename}" "python" "${CMAKE_SOURCE_DIR}/run-examples.py" "-q" "${CMAKE_BINARY_DIR}/qwip" ${path} "--valgrind")
  else()
    add_test("run-lit-test-example-${filename}" "python" "${CMAKE_SOURCE_DIR}/run-examples.py" "-q" "${CMAKE_BINARY_DIR}/qwip" ${path})
  endif()
endforeach()
