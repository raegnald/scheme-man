# Collect source files
file(GLOB SOURCES "src/*.cpp")

# Create executable
add_executable(scman ${SOURCES})

# Link against SFML and tmxlite
target_link_libraries(scman PRIVATE
  sfml-system sfml-graphics sfml-window sfml-audio
  PkgConfig::GUILE
  tmxlite
)
