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
  src/Parser.cpp
  src/qwip.cpp)

# Now build our tools.
add_executable(qwip ${SRCS})
target_compile_options(qwip
  PRIVATE -Werror
  PRIVATE -Wall
  PRIVATE -Wextra
  PRIVATE -Wconversion
  PRIVATE -fno-exceptions
  PRIVATE -fno-rtti
  PRIVATE -std=c++17
  PRIVATE -Wno-unused-parameter  # For warnings from LLVM headers
  PRIVATE -UNDEBUG  # We will always keep assertions on!
)

if (CODE_COVERAGE)
  message(STATUS "Building with code coverage")
  target_compile_options(qwip
    PRIVATE -fprofile-arcs
    PRIVATE -ftest-coverage
    PRIVATE -g
  )
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fprofile-arcs -ftest-coverage")
endif()

llvm_config(qwip USE_SHARED)  # Link against the standalone libLLVM shared library

if (${SANITIZER} STREQUAL "ADDRESS")
  message(STATUS "Building with ASan")
  target_compile_options(qwip PRIVATE "-fsanitize=address,pointer-compare,pointer-subtract")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=address,pointer-compare,pointer-subtract")
elseif(${SANITIZER} STREQUAL "THREAD")
  message(STATUS "Building with TSan")
  target_compile_options(qwip PRIVATE "-fsanitize=thread")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
elseif(${SANITIZER} STREQUAL "UNDEFINED")
  message(STATUS "Building with UBSan")
  target_compile_options(qwip PRIVATE "-fsanitize=undefined")
  set_property(TARGET qwip APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=undefined")
endif()

# Testing
ENABLE_TESTING()
include(CTest)

function(add_qwip_test_example test_name expected_output)
  if (${TEST_WITH_VALGRIND})
    add_test("build-example-${test_name}"
      valgrind --leak-check=full --error-exitcode=1 "${CMAKE_BINARY_DIR}/qwip"
      "${CMAKE_SOURCE_DIR}/examples/${test_name}.qw" "-o" "${test_name}.qw.out")
  else()
    add_test("build-example-${test_name}"
      qwip
      "${CMAKE_SOURCE_DIR}/examples/${test_name}.qw" "-o" "${test_name}.qw.out")
  endif()

  add_test("run-example-${test_name}" "python" "${CMAKE_SOURCE_DIR}/compare_outputs.py" "${CMAKE_BINARY_DIR}/${test_name}.qw.out" "${expected_output}")
endfunction()

# TODO: Update testing such that whenever we want to add a new test, we can
# just add a new test file or update another testing file instead of having to
# change the cmake file.
add_qwip_test_example(0-quickstart "global_day is Wednesday? 1\n")
add_qwip_test_example(1-empty-main "")
add_qwip_test_example(2-hello-world "Hello World!\n")
add_qwip_test_example(3-variable-declaration "Test\nTest\n")
add_qwip_test_example(4-print-format "10\n11\n")
add_qwip_test_example(5-fibonacci "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n")
add_qwip_test_example(6-structures "7\n3\n")
add_qwip_test_example(7-arrays "1\n2\n3\n4\n5\n")
