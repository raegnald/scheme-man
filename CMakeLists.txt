cmake_minimum_required(VERSION 3.15)
project(SchemeMan LANGUAGES C CXX)

# Enable compile_commands.json for tooling
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Dependencies

# ┌──────┐
# │ SFML │
# └──────┘
find_package(SFML CONFIG REQUIRED COMPONENTS System Window Graphics Audio Network)

# ┌───────────┐
# │ wxWidgets │
# └───────────┘
find_package(wxWidgets CONFIG REQUIRED COMPONENTS core base richtext)

if (CMAKE_SYSTEM_NAME MATCHES "Linux")
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(GTK3 QUIET gtk+-3.0)
  if (GTK3_FOUND)
    include_directories(${GTK3_INCLUDE_DIRS})
  endif()
endif()

# ┌────┐
# │ s7 │
# └────┘
add_subdirectory(lib/s7)

# ┌─────────┐
# │ tmxlite │
# └─────────┘
add_subdirectory(lib/tmxlite/tmxlite)
include_directories(${TMXLITE_INCLUDE_DIR})

# Project sources
add_subdirectory(src)
