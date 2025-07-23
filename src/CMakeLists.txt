# Set C++ standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(IS_DEBUG_BUILD CMAKE_BUILD_TYPE STREQUAL "Debug")

# Indication to the code that it is a debug build
if (${IS_DEBUG_BUILD})
  add_compile_definitions(__DEBUG__)
endif ()

# Collect source files
file(GLOB SOURCES "*.cpp")

# Create executable
add_executable(scman ${SOURCES})

# Link against SFML and tmxlite
target_link_libraries(scman PRIVATE
  sfml-system sfml-graphics sfml-window sfml-audio
  PkgConfig::GUILE
  tmxlite
)
