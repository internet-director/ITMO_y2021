cmake_minimum_required(VERSION 3.21)
project(intrusive_list)

set(CMAKE_CXX_STANDARD 20)

find_package(GTest REQUIRED)

set(BASE_TESTS_SOURCES tests.cpp intrusive_list.h intrusive_list.cpp)
add_executable(base-tests ${BASE_TESTS_SOURCES})
add_executable(tests ${BASE_TESTS_SOURCES} advanced_tests.cpp)

if (NOT MSVC)
  target_compile_options(tests PRIVATE -Wall -Wno-sign-compare -pedantic)
endif()

option(USE_SANITIZERS "Enable to build with undefined,leak and address sanitizers" OFF)
if (USE_SANITIZERS)
  message(STATUS "Enabling sanitizers...")
  set(COMPILE_OPTS PUBLIC -fsanitize=address,undefined,leak -fno-sanitize-recover=all)
  target_compile_options(base-tests ${COMPILE_OPTS})
  target_compile_options(tests ${COMPILE_OPTS})
  set(LINK_OPTS PUBLIC -fsanitize=address,undefined,leak)
  target_link_options(base-tests  ${LINK_OPTS})
  target_link_options(tests ${LINK_OPTS})
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  message(STATUS "Enabling libc++...")
  target_compile_options(base-tests PUBLIC -stdlib=libc++)
  target_link_options(base-tests PUBLIC -stdlib=libc++)
  target_compile_options(tests PUBLIC -stdlib=libc++)
  target_link_options(tests PUBLIC -stdlib=libc++)
endif()

if (CMAKE_BUILD_TYPE MATCHES "Debug")
  message(STATUS "Enabling _GLIBCXX_DEBUG...")
  target_compile_options(base-tests PUBLIC -D_GLIBCXX_DEBUG)
  target_compile_options(tests PUBLIC -D_GLIBCXX_DEBUG)
endif()

target_link_libraries(base-tests GTest::gtest GTest::gtest_main)
target_link_libraries(tests GTest::gtest GTest::gtest_main)
