// Entry point of the Scheme-Man executable

#include <print>
#include <filesystem>
#include <SFML/Config.hpp>
#include <s7.h>
#include "Game.hpp"

static_assert(SFML_VERSION_MAJOR == 3, "SFML version 3 required");
static_assert(__cplusplus >= 202302L, "Use C++23 or later");
static_assert(S7_MAJOR_VERSION == 11, "Invalid s7 version");

void usage(int argc, char *argv[]) {
  std::println(stderr, "Usage:");
  std::println(stderr, "  {} path/to/level/file.tmx", argv[0]);
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::println(stderr, "Error: No level file given.");
    usage(argc, argv);
    return EXIT_FAILURE;
  }

  std::filesystem::path level_file = argv[1];
  Game game{level_file};

  while (game.running()) {
    game.update();
    game.draw();
  }

  return EXIT_SUCCESS;
}
