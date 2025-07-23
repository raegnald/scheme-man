cmake_minimum_required(VERSION 3.15)
project(SchemeMan LANGUAGES CXX)

# Enable compile_commands.json for tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# SFML
add_subdirectory(lib/sfml)

find_package(PkgConfig REQUIRED)

# Find Guile using pkg-config
pkg_check_modules(GUILE REQUIRED IMPORTED_TARGET
  guile-3.0)

# tmxlite package
add_subdirectory(lib/tmxlite/tmxlite)
include_directories(${TMXLITE_INCLUDE_DIR})

add_subdirectory(src)
