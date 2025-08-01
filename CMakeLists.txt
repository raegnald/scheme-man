cmake_minimum_required(VERSION 3.15)
project(SchemeMan LANGUAGES C CXX)

# Enable compile_commands.json for tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# wxWidgets GUI library

if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(GTK3 REQUIRED gtk+-3.0)
  include_directories (${GTK3_INCLUDE_DIRS})
endif ()

set(wxBUILD_SHARED OFF)
add_subdirectory(lib/wxWidgets)

# SFML graphics library
add_subdirectory(lib/sfml)

# s7 scheme interpreter
add_subdirectory(lib/s7)

# tmxlite Tiled Maps Editor file format parser
add_subdirectory(lib/tmxlite/tmxlite)
include_directories(${TMXLITE_INCLUDE_DIR})

add_subdirectory(src)
