cmake_minimum_required(VERSION 3.4.3)
project(qwip)

option(TEST_WITH
	"When testing, optionally test with valgrind or the qwip-asan variant. Can be VALGRIND, ASAN, or OFF (default)."
       OFF)

find_package(LLVM REQUIRED CONFIG)
find_package(PythonInterp 2.7 REQUIRED)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set the compile flags.
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

message(STATUS "LLVM C++ Compilation Flags: ${LLVM_DEFINITIONS}")

# Now build our tools.
add_executable(qwip
  src/Compiler.cpp
  src/Lexer.cpp
  src/Parser.cpp
  src/qwip.cpp)

add_executable(qwip-asan
  src/Compiler.cpp
  src/Lexer.cpp
  src/Parser.cpp
  src/qwip.cpp)
target_compile_options(qwip-asan PRIVATE "-fsanitize=address")
set_target_properties(qwip-asan PROPERTIES LINK_FLAGS "-fsanitize=address")

# Link against the single standalone libLLVM shared library.
llvm_config(qwip USE_SHARED)
llvm_config(qwip-asan USE_SHARED)

# Testing
ENABLE_TESTING()
include(CTest)

function(add_qwip_test_example test_name expected_output)
  if(${TEST_WITH} STREQUAL "VALGRIND")
    add_test("build-example-${test_name}"
      valgrind --leak-check=full --error-exitcode=1 "${CMAKE_BINARY_DIR}/qwip"
      "${CMAKE_SOURCE_DIR}/examples/${test_name}.qw" "-o" "${test_name}.qw.out")
  elseif()
    add_test("build-example-${test_name}"
      qwip-asan
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
# TODO: Come up with a way to run the valgrind and asan tests without having
# to change the cmake incovation.
add_qwip_test_example(1-empty-main "")
add_qwip_test_example(2-hello-world "Hello World!\n")
add_qwip_test_example(3-variable-declaration "Test\nTest\n")
add_qwip_test_example(4-print-format "10\n11\n")
add_qwip_test_example(5-fibonacci "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n")
add_qwip_test_example(6-structures "7\n3\n")
