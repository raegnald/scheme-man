cmake_minimum_required(VERSION 3.15)
project(SchemeMan LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_BUILD_TYPE Debug)

# Enable compile_commands.json for tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find SFML using pkg-config
find_package(PkgConfig REQUIRED)
pkg_check_modules(SFML REQUIRED IMPORTED_TARGET
  sfml-system
  sfml-graphics
  sfml-window
)

# tmxlite package
add_subdirectory(lib/tmxlite/tmxlite)
# find_package(tmxlite REQUIRED)
include_directories(
  ${TMXLITE_INCLUDE_DIR}
)

# Collect source files
file(GLOB SOURCES "src/*.cpp")

# Create executable
add_executable(scman ${SOURCES})

# Link against SFML and tmxlite
target_link_libraries(scman PRIVATE
  PkgConfig::SFML
  tmxlite
)
