#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <SFML/Window/Event.hpp>
#include <SFML/Window/Keyboard.hpp>
#include <algorithm>
#include <tmxlite/Map.hpp>
#include <cstdlib>
#include <filesystem>
#include "Interpolated.hpp"
#include "Level.hpp"
#include "LevelObject.hpp"

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

void sfmlMain(int argc, char *argv[]) {
  sf::RenderWindow window(sf::VideoMode({600, 450}), "Scheme-Man");
  window.setFramerateLimit(30);

  sf::Vector2f oldPos;
  bool mouse_clicked = false;
  bool gameEnd = false;

  Level level(argv[1]);
  level.setScale(0.25);

  sf::View view(sf::Vector2f(0, 0), sf::Vector2f(window.getSize()));

  sf::CircleShape point(5);
  point.setFillColor(sf::Color::Magenta);

  sf::CircleShape originPoint(5);
  originPoint.setFillColor(sf::Color::Green);

  Interpolated<sf::Vector3f> currentBackground(vectorFromColor(level.background));
  currentBackground.setFunction(Interpolating_function::Ease_out_quad);

  Interpolated<sf::Vector2f> viewCenter{sf::Vector2f(0, 0)};
  viewCenter.setDuration(0.1);

  while (window.isOpen()) {
    while (const auto event = window.pollEvent()) {
      if (event->is<sf::Event::Closed>()) {
        window.close();
      }

      if (const auto *resized = event->getIf<sf::Event::Resized>()) {
        view.setSize(sf::Vector2f(resized->size));
        window.setView(view);
      }

      if (const auto *mouse = event->getIf<sf::Event::MouseButtonPressed>()) {
        mouse_clicked = true;
        oldPos = window.mapPixelToCoords(mouse->position);
      }

      if (const auto *key = event->getIf<sf::Event::KeyPressed>()) {
        switch (key->code) {
        case sf::Keyboard::Key::Space:
          level.player.walk();
          break;
        case sf::Keyboard::Key::C:
          level.player.turnClockwise();
          break;
        case sf::Keyboard::Key::X:
          level.player.turnAnticlockwise();
        default:
          break;
        }
      }

      if (event->is<sf::Event::MouseButtonReleased>())
        mouse_clicked = false;

      if (mouse_clicked) {
        if (const auto *dragged = event->getIf<sf::Event::MouseMoved>()) {
          const auto newPos = window.mapPixelToCoords(dragged->position);
          const auto deltaPos = oldPos - newPos;

          // view.setCenter(view.getCenter() + deltaPos);
          viewCenter.setTarget(view.getCenter() + deltaPos);
          window.setView(view);
        }
      }

      if (const auto *scroll = event->getIf<sf::Event::MouseWheelScrolled>()) {
        // TODO: Tenemos que hacer zoom respecto a la posición del
        // ratón para que se sienta más natural -- no respecto al
        // origen del nivel.
        auto t = window.mapPixelToCoords(scroll->position, view);
        point.setPosition(t);

        float delta = 0.05 * scroll->delta;

        level.setScale(level.geometry.scale + delta);

        const auto playerCenter =
          level.geometry.isometric(level.player.position.getValue());
        if (viewCenter.interpolationEnded())
          viewCenter.setTarget(playerCenter);
        else
          viewCenter.setOrigin(playerCenter);
      }
    }

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

    if (level.player.walking) {
      const auto playerCenter =
          level.geometry.isometric(level.player.position.getValue());
      viewCenter.setTarget(playerCenter);
    }

    view.setCenter(viewCenter.getValue());

    window.clear(colorFromVector(currentBackground));
    window.setView(view);
    level.update();
    window.draw(level);

    if (0) {
      originPoint.setPosition(level.geometry.origin);
      window.draw(originPoint);

      window.draw(point);
    }

    window.display();
  }
}

int main(int argc, char *argv[]) {
  sfmlMain(argc, argv);
  return EXIT_SUCCESS;
}
