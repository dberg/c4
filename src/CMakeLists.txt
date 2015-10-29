cmake_minimum_required(VERSION 3.3.2)
project(c4)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")

include_directories(include)

file(GLOB_RECURSE SOURCES "lib/*.cpp")

add_executable(c4 ${SOURCES})
