cmake_minimum_required(VERSION 3.3.2)

# Turn on with 'cmake -Dtest=ON'.
option(test "Build all tests." OFF)

#------------------------------------------------------------------------------
# Main project
#------------------------------------------------------------------------------
project(c4)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")

include_directories(include)

file(GLOB_RECURSE SOURCES_LIB "lib/common/*.cpp")

file(GLOB_RECURSE SOURCES_MAIN "lib/main/*.cpp")

add_library(c4_lib ${SOURCES_LIB})

add_executable(c4 ${SOURCES_MAIN})

target_link_libraries(c4 c4_lib)

#------------------------------------------------------------------------------
# Tests
#------------------------------------------------------------------------------
# Based on:
# https://github.com/dmonopoly/gtest-cmake-example
#------------------------------------------------------------------------------
if (test)

  add_subdirectory(third_party/googletest-release-1.7.0)

  enable_testing()

  # Include the gtest library. gtest_SOURCE_DIR is available due to
  # 'project(gtest)' above.
  include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

  add_executable(tests test/test.cpp)

  # Standard linking to gtest stuff.
  target_link_libraries(tests gtest gtest_main)

  # Extra linking for the project.
  target_link_libraries(tests c4_lib)

endif()
