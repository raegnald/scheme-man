#include <SFML/Config.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/Graphics/Color.hpp>
#include <SFML/Window.hpp>
#include <SFML/Window/Event.hpp>
#include <SFML/Window/Keyboard.hpp>
#include <algorithm>
#include <cstdio>
#include <optional>
#include <tmxlite/Map.hpp>
#include <cstdlib>
#include <filesystem>
#include <format>
#include "Interpolated.hpp"
#include "Level.hpp"
#include "LevelObject.hpp"
#include "HUD.hpp"

static_assert(SFML_VERSION_MAJOR == 3, "SFML version 3 required");
static_assert(__cplusplus >= 202302L, "Use C++23 or later");

constexpr auto victoryBackground = sf::Color(0xd9, 0xf9, 0xdf);
constexpr auto failureBackground = sf::Color(0xf2, 0xc0, 0xb0);

sf::Vector3f vectorFromColor(sf::Color color) {
  return static_cast<float>(1.0 / 255) *
         sf::Vector3f(color.r, color.g, color.b);
}

sf::Color colorFromVector(sf::Vector3f v) {
  return sf::Color(std::clamp(static_cast<int>(255 * v.x), 0, 255),
                   std::clamp(static_cast<int>(255 * v.y), 0, 255),
                   std::clamp(static_cast<int>(255 * v.z), 0, 255));
}

constexpr sf::Vector2u default_window_size{600, 450};
constexpr sf::Vector2u minimum_window_size{300, 225};

constexpr auto default_window_title = "Scheme-Man";

struct Game {
private:
  sf::RenderWindow window;
  sf::View view;

  sf::Clock clock;

  sf::Vector2f oldPos;
  bool mouse_clicked = false;
  bool gameEnd = false;

  Level level;
  HUD hud_overlay{&level};

  Interpolated<sf::Vector3f> currentBackground;
  Interpolated<sf::Vector2f> viewCenter{sf::Vector2f(0, 0)};

  Coin testObject{sf::Vector2f(0, 0), &level.geometry};

  void handleMouseDrag(const std::optional<sf::Event> &event) {
    if (const auto *mouse = event->getIf<sf::Event::MouseButtonPressed>()) {
      mouse_clicked = true;
      oldPos = window.mapPixelToCoords(mouse->position);
    }

    if (event->is<sf::Event::MouseButtonReleased>())
      mouse_clicked = false;

    if (mouse_clicked) {
      if (const auto *dragged = event->getIf<sf::Event::MouseMoved>()) {
        const auto newPos = window.mapPixelToCoords(dragged->position);
        const auto deltaPos = oldPos - newPos;

        viewCenter.setTarget(view.getCenter() + deltaPos);
        window.setView(view);
      }
    }
  }

  void handleMouseScroll(const std::optional<sf::Event> &event) {
    if (const auto *scroll = event->getIf<sf::Event::MouseWheelScrolled>()) {
      // Reset acceleration when scrolling in different direction so
      // that it feels more snappy
      if (scroll->delta * level.geometry.scale < 0)
        level.geometry.scale.stopMovement();

      // level.geometry.scale.applyAcceleration(10 * scroll->delta);
      level.setScale(level.geometry.scale.getValue() +
                     0.1 * scroll->delta);

      const auto playerCenter =
        level.geometry.isometric(level.player.position.getValue());
      viewCenter.setOrigin(playerCenter);
    }
  }

  void handleEvents(void) {
    while (const auto event = window.pollEvent()) {
      if (event->is<sf::Event::Closed>()) {
        window.close();
      }

      if (const auto *resized = event->getIf<sf::Event::Resized>()) {
        view.setSize(sf::Vector2f(resized->size));
        window.setView(view);
      }

      handleMouseDrag(event);
      handleMouseScroll(event);

      debug if (const auto *key = event->getIf<sf::Event::KeyPressed>()) {
        if (!key->control)
          continue;

        switch (key->code) {
        case sf::Keyboard::Key::Space:
          level.player.walk();
          level.setStatus("Walking (debug)");
          break;
        case sf::Keyboard::Key::C:
          level.player.turnClockwise();
          level.setStatus("Turning right (debug)");
          break;
        case sf::Keyboard::Key::X:
          level.player.turnAnticlockwise();
          level.setStatus("Turning left (debug)");
        default:
          break;
        }
      }
    }
  }

  void updateWindowTitle(void) {
    window.setTitle(
        std::format("{} [{}% coins] [{} metres] [player at {}, {}]",
                    default_window_title,
                    hud_overlay.coinsPercentage(), hud_overlay.getMeters(),
                    level.player.position.end.x,
                    level.player.position.end.y));
  }

  void checkGameEnd(void) {
    if (!gameEnd && level.player.reachedStar) {
      gameEnd = true;
      currentBackground.setTarget(vectorFromColor(victoryBackground));
    }

    if (!gameEnd) {
      const auto [x, y] = level.player.position.getValue();
      const auto [w, h] = level.geometry.dimensions;
      if (x < 0 || y < 0 || x >= w || y >= h) {
        gameEnd = true;
        currentBackground.setTarget(vectorFromColor(failureBackground));
      }
    }
  }

public:
  Game(std::filesystem::path &level_file)
      : window(sf::VideoMode(default_window_size), default_window_title),
        view(sf::Vector2f(0, 0), sf::Vector2f(window.getSize())),
        currentBackground(vectorFromColor(level.background)),
        level(level_file) {

      window.setFramerateLimit(60);
      window.setMinimumSize(minimum_window_size);

      currentBackground.setFunction(Interpolating_function::Ease_out_quad);

      viewCenter.setFunction(Interpolating_function::Ease_out_quad);
      viewCenter.setDuration(0.1);

      level.setScale(0.25);
  }

  bool running(void) { return window.isOpen(); }

  void update(void) {
    handleEvents();
    updateWindowTitle();

    // Update level
    const auto elapsed = clock.restart().asSeconds();
    level.update(elapsed);
    testObject.update(level.player);

    checkGameEnd();

    if (level.player.walking) {
      const auto playerCenter =
          level.geometry.isometric(level.player.position.getValue());
      viewCenter.setTarget(playerCenter);
    }

    view.setCenter(viewCenter);
  }

  void draw(void) {
    window.clear(colorFromVector(currentBackground));
    window.setView(view);
    window.draw(level);
    window.draw(testObject);
    window.display();
  }
};

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::println(stderr, "Error: No level file given.");
    std::println(stderr, "Usage:");
    std::println(stderr, "  {} path/to/level/file.tmx", argv[0]);
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
