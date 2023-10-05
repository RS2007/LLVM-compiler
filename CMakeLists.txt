cmake_minimum_required(VERSION 3.8)
project(schemeLLVM)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_COMPILER "/usr/bin/clang++-13")

set(BUILD_ARCH "-m64")
set(TARGET_64 ON)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS} -I/usr/include/llvm-c-13 -I/usr/include/llvm-13)
add_definitions(${LLVM_DEFINITIONS})
file(GLOB all_SRCS
  "${PROJECT_SOURCE_DIR}/include/*.h"
  "${PROJECT_SOURCE_DIR}/include/*.hpp"
  "${PROJECT_SOURCE_DIR}/src/*.cpp"
  "${PROJECT_SOURCE_DIR}/src/*.c"
)
add_executable(exec ${all_SRCS})
set(CMAKE_CXX_FLAGS "-I/usr/include/llvm-c-13 -I/usr/include/llvm-13  -std=c++17   -fno-exceptions -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS")

include_directories(
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_SOURCE_DIR}/src
)

# Link LLVM libraries to your executable
target_include_directories(exec PRIVATE 
 /usr/include/llvm-c-13 /usr/include/llvm-13
)
target_link_libraries(exec ${llvm_libs} LLVMSupport)
target_link_options(exec PRIVATE -L/usr/lib/llvm-13/lib -lLLVM-13)
 
llvm_map_components_to_libnames(llvm_libs
  Analysis
  Core
  ExecutionEngine
  InstCombine
  Object
  OrcJIT
  RuntimeDyld
  ScalarOpts
  Support
  native
  )

set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")
