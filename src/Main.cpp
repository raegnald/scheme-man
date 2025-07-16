#include <SFML/Config.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/Graphics/Color.hpp>
#include <SFML/Window.hpp>
#include <SFML/Window/Event.hpp>
#include <SFML/Window/Keyboard.hpp>
#include <algorithm>
#include <cstdio>
#include <fstream>
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
  return (1.0f / 255) * sf::Vector3f(color.r, color.g, color.b);
}

sf::Color colorFromVector(sf::Vector3f v) {
  return sf::Color(std::clamp(static_cast<int>(255 * v.x), 0, 255),
                   std::clamp(static_cast<int>(255 * v.y), 0, 255),
                   std::clamp(static_cast<int>(255 * v.z), 0, 255));
}

constexpr sf::Vector2u default_window_size{600, 450};
constexpr sf::Vector2u minimum_window_size{300, 225};

constexpr auto default_window_title =
#if defined __DEBUG__
  "Scheme-Man (DEBUG)";
#else
  "Scheme-Man";
#endif

constexpr auto window_state_file = ".scman_window_state";
constexpr auto window_state_file_magic = "scman";

struct Game {
private:
  sf::RenderWindow window;
  sf::View level_view, overlay_view;

  sf::Clock clock;

  sf::Vector2f oldPos;
  bool mouse_clicked = false;

  Level level;
  HUD hud_overlay{&level};

  Interpolated<sf::Vector3f> currentBackground;
  Interpolated<sf::Vector2f> viewCenter{sf::Vector2f(0, 0)};

  void loadWindowState(void) {
    std::ifstream state(window_state_file);
    std::string magic;

    state >> magic;
    if (magic != window_state_file_magic)
      return;

    sf::Vector2i window_position;
    state >> window_position.x;
    state >> window_position.y;
    window.setPosition(window_position);

    sf::Vector2u window_size;
    state >> window_size.x;
    state >> window_size.y;
    window.setSize(window_size);
  }

  void saveWindowState(void) const {
    std::ofstream state(window_state_file);

    auto [x, y] = window.getPosition();
    auto [w, h] = window.getSize();

    state << window_state_file_magic << " "
          << x << " " << y << " "
          << w << " " << h;
  }

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

        viewCenter.setTarget(level_view.getCenter() + deltaPos);
        window.setView(level_view);
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

      centerPlayerInWindow(true);
    }
  }

  void handleEvents(void) {
    // We set the view to the level in case any other views have been
    // set, since the mouse drag and zoom actions depend on the main
    // view (i.e. the level view) being set.
    window.setView(level_view);

    while (const auto event = window.pollEvent()) {
      if (event->is<sf::Event::Closed>()) {
        saveWindowState();
        window.close();
      }

      if (const auto *resized = event->getIf<sf::Event::Resized>()) {
        level_view.setSize(sf::Vector2f(resized->size));
        overlay_view.setSize(sf::Vector2f(resized->size));
        window.setView(level_view);
      }

      handleMouseDrag(event);
      handleMouseScroll(event);

      if (const auto *key = event->getIf<sf::Event::KeyPressed>()) {
        if (!key->control)
          continue;

        switch (key->code) {
        case sf::Keyboard::Key::R:
          level.reset();
          centerPlayerInWindow(false);
          level.setStatus("Reset level");
          break;

        case sf::Keyboard::Key::Space:
          debug {
            level.player.walk();
            level.setStatus("Walking (debug)");
          }
          break;
        case sf::Keyboard::Key::C:
          debug {
            level.player.turnClockwise();
            level.setStatus("Turning right (debug)");
          }
          break;
        case sf::Keyboard::Key::X:
          debug {
            level.player.turnAnticlockwise();
            level.setStatus("Turning left (debug)");
          }
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
    if (level.active && level.player.reachedStar) {
      level.active = false;
      currentBackground.setTarget(vectorFromColor(victoryBackground));
    }

    if (level.active) {
      const auto [x, y] = level.player.position.getValue();
      const auto [w, h] = level.geometry.dimensions;
      if (x < 0 || y < 0 || x >= w || y >= h ||
          !level.isFloor(level.player.position.end)) {
        level.active = false;
        currentBackground.setTarget(vectorFromColor(failureBackground));
      }
    }
  }

  void centerPlayerInWindow(bool instantly = false) {
    const auto pos = level.player.position.getValue();
    const auto player_center = level.geometry.isometric(pos);

    if (instantly)
      viewCenter.setOrigin(player_center);
    else
      viewCenter.setTarget(player_center);
  }

public:
  Game(std::filesystem::path &level_file)
      : window(sf::VideoMode(default_window_size), default_window_title),
        level_view(sf::Vector2f(0, 0), sf::Vector2f(window.getSize())),
        currentBackground(vectorFromColor(level.background)),
        level(level_file) {

    loadWindowState();

    window.setVerticalSyncEnabled(true);
    window.setMinimumSize(minimum_window_size);

    currentBackground.setFunction(Interpolating_function::Ease_out_quad);

    viewCenter.setFunction(Interpolating_function::Ease_out_quad);
    viewCenter.setDuration(0.1);

    level.setScale(0.25);
    centerPlayerInWindow(true);
  }

  bool running(void) { return window.isOpen(); }

  void update(void) {
    handleEvents();
    updateWindowTitle();

    // Update level
    const auto elapsed = clock.restart().asSeconds();
    level.update(elapsed);

    checkGameEnd();

    // When player is moving and not visible, center the view relative
    // to its position.
    if (level.player.walking) {
      const sf::FloatRect view_rect(level_view.getCenter() -
                                        level_view.getSize() / 2.0f,
                                    level_view.getSize());
      const bool visible = view_rect.contains(
          level.geometry.isometric(level.player.position.getValue()));
      if (!visible)
        centerPlayerInWindow();
    }

    level_view.setCenter(viewCenter);

    // Makes the level view have the same center as the window center,
    // plus the fact that the size is the whole window, it makes the
    // overlay view take up all the window.
    overlay_view.setCenter(sf::Vector2f(window.getSize()) * 0.5f);
  }

  void draw(void) {
    window.clear(colorFromVector(currentBackground));

    window.setView(level_view);
    window.draw(level);

    window.setView(overlay_view);
    window.draw(hud_overlay);

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
