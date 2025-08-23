// The state of the game

#pragma once

#include <SFML/Config.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/Graphics/Color.hpp>
#include <SFML/Window.hpp>
#include <SFML/Window/Event.hpp>
#include <SFML/Window/Keyboard.hpp>
#include <cstdio>
#include <fstream>
#include <optional>
#include <cstdlib>
#include <filesystem>
#include <format>
#include "Interpolated.hpp"
#include "Level.hpp"
#include "LevelObject.hpp"
#include "HUD.hpp"
#include "SFML/Window/WindowHandle.hpp"
#include "color.hpp"

constexpr auto victory_background = sf::Color(0xd9, 0xf9, 0xdf);
constexpr auto failure_background = sf::Color(0xf2, 0xc0, 0xb0);

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
  Interpolated<sf::Vector2f> view_center{sf::Vector2f(0, 0)};

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

    float scale;
    if (state >> scale)
      level.geometry.scale.setValue(scale);
  }

  void saveWindowState(void) const {
    std::ofstream state(window_state_file);

    const auto [x, y] = window.getPosition();
    const auto [w, h] = window.getSize();
    const auto scale = level.geometry.scale.getValue();

    state << window_state_file_magic << " "
          << x << " " << y << " "
          << w << " " << h << " "
          << scale;
  }

  void updateWindowTitle(void) {
    // window.setTitle(
    //     std::format("{} [{}% coins] [{} metres] [player at {}, {}]",
    //                 default_window_title,
    //                 hud_overlay.coinsPercentage(), hud_overlay.getMeters(),
    //                 level.player.position.end.x,
    //                 level.player.position.end.y));
  }

  void checkGameEnd(void) {
    if (level.active && level.player.reachedStar) {
      level.active = false;
      currentBackground.setTarget(vectorFromColor(victory_background));
    }

    if (level.active) {
      const auto [x, y] = level.player.position.getValue();
      const auto [w, h] = level.geometry.dimensions;
      if (x < 0 || y < 0 || x >= w || y >= h ||
          !level.isFloor(level.player.position.end)) {
        level.active = false;
        currentBackground.setTarget(vectorFromColor(failure_background));
      }
    }
  }

  void centerPlayerInWindow(bool instantly = false) {
    const auto pos = level.player.position.getValue();
    const auto player_center = level.geometry.isometric(pos);

    if (instantly)
      view_center.setOrigin(player_center);
    else
      view_center.setTarget(player_center);
  }

public:
  Game() = delete;

  Game(sf::WindowHandle handle, const std::filesystem::path &level_file)
      : window(handle),
        level_view(sf::Vector2f(0, 0), sf::Vector2f(window.getSize())),
        currentBackground(vectorFromColor(level.background)),
        level(level_file) {

    level.setScale(0.25);

    loadWindowState();

    window.setVerticalSyncEnabled(true);

    currentBackground.setFunction(Interpolating_function::Ease_out_quad);

    view_center.setFunction(Interpolating_function::Ease_out_quad);
    view_center.setDuration(0.1f);

    centerPlayerInWindow(true);
  }

  Level *getLevel(void) { return &level; }

  void setMouseClicked(bool clicked) { mouse_clicked = clicked; }

  void setMousePosition(sf::Vector2i position) {
    oldPos = window.mapPixelToCoords(position);
  }

  void handleMouseDrag(const std::optional<sf::Event> &event) {
    if (!mouse_clicked)
      return;

    if (const auto *dragged = event->getIf<sf::Event::MouseMoved>()) {
      const auto newPos = window.mapPixelToCoords(dragged->position);
      const auto deltaPos = oldPos - newPos;

      view_center.setTarget(level_view.getCenter() + deltaPos);
      window.setView(level_view);
    }
  }

  void handleMouseScroll(int delta) {
    // Reset acceleration when scrolling in different direction so
    // that it feels more snappy
    if (delta * level.geometry.scale < 0)
      level.geometry.scale.stopMovement();

    level.geometry.scale.applyAcceleration(10 * delta);
  }

  void close(void) {
    saveWindowState();
    level.shutdown();
    window.close();
  }

  void handleEvents(void) {
    // We set the view to the level in case any other views have been
    // set, since the mouse drag and zoom actions depend on the main
    // view (i.e. the level view) being set.
    window.setView(level_view);

    while (const auto event = window.pollEvent()) {
      if (event->is<sf::Event::Closed>())
        close();

      if (const auto *resized = event->getIf<sf::Event::Resized>()) {
        level_view.setSize(sf::Vector2f(resized->size));
        overlay_view.setSize(sf::Vector2f(resized->size));
        window.setView(level_view);
      }

      if (const auto *scroll = event->getIf<sf::Event::MouseWheelScrolled>())
        handleMouseScroll(scroll->delta);

      if (const auto *click = event->getIf<sf::Event::MouseButtonPressed>()) {
        setMouseClicked(true);
        setMousePosition(click->position);
      }

      if (event->is<sf::Event::MouseButtonReleased>())
        setMouseClicked(false);

      if (const auto *mouse = event->getIf<sf::Event::MouseButtonPressed>())
        handleMouseDrag(event);

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

  bool running(void) { return window.isOpen(); }

  void update(void) {
    handleEvents();
    // updateWindowTitle();

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

    level_view.setCenter(view_center);

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
