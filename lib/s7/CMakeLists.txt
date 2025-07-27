cmake_minimum_required(VERSION 3.10)

add_library(s7 OBJECT s7.c)

target_include_directories(s7 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# target_compile_options(s7 PRIVATE -w)
