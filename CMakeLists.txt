cmake_minimum_required(VERSION 3.15)
project(SchemeMan LANGUAGES CXX)

# Enable compile_commands.json for tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# SFML graphics library
add_subdirectory(lib/sfml)

# s7 scheme interpreter
add_subdirectory(lib/s7)

# tmxlite Tiled Maps Editor file format parser
add_subdirectory(lib/tmxlite/tmxlite)
include_directories(${TMXLITE_INCLUDE_DIR})

add_subdirectory(src)
