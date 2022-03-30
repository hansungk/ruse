cmake_minimum_required (VERSION 3.8)

project (ruse LANGUAGES CXX)

add_executable (ruse src/main.cc src/driver.cc src/sema.cc src/parse.cc
  src/ast.cc src/lex.cc src/source.cc src/format.cc)
target_include_directories(ruse PUBLIC .)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "DEBUG")
endif()

list(APPEND MY_COMPILE_FLAGS -Wall -Wextra -Wno-unused-parameter
  -fno-omit-frame-pointer)
list(APPEND MY_LINK_FLAGS -fno-omit-frame-pointer)

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  list(APPEND MY_COMPILE_FLAGS -fcolor-diagnostics $<$<CONFIG:DEBUG>:-ggdb>)

  if(UNIX AND NOT APPLE)
    # Link to libc++
    # list(APPEND MY_COMPILE_FLAGS -stdlib=libc++)
    # list(APPEND MY_LINK_FLAGS -stdlib=libc++ -rtlib=compiler-rt
    #   -unwindlib=libunwind -Wl,-rpath=$ENV{HOME}/build/llvm/lib)

    # Selectively use LLD on Linux
    list(APPEND MY_LINK_FLAGS -fuse-ld=lld)
  else()
    list(APPEND MY_LINK_FLAGS
      -Wl,-L$ENV{HOME}/build/llvm/lib -Wl,-rpath,$ENV{HOME}/build/llvm/lib)
  endif()
elseif (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  list(APPEND MY_COMPILE_FLAGS -fdiagnostics-color=always $<$<CONFIG:DEBUG>:-ggdb>)
endif()

if(UNIX AND NOT APPLE)
  list(APPEND MY_COMPILE_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
  list(APPEND MY_LINK_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,leak,undefined>")
else()
  # LeakSanitizer does not work on Darwin
  list(APPEND MY_COMPILE_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,undefined>")
  list(APPEND MY_LINK_FLAGS
    "$<$<CONFIG:DEBUG>:-fsanitize=address,undefined>")
endif()

target_compile_features(ruse PUBLIC cxx_std_17)
target_compile_options(ruse PRIVATE ${MY_COMPILE_FLAGS})
target_link_options(ruse PRIVATE ${MY_LINK_FLAGS})

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

configure_file(check.py ${CMAKE_CURRENT_BINARY_DIR} COPYONLY)
add_custom_target(copy-test-files ALL
  COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_SOURCE_DIR}/test
  ${CMAKE_CURRENT_BINARY_DIR}/test
  DEPENDS ruse)
