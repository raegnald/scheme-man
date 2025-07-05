#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <SFML/Window/Event.hpp>
#include <tmxlite/Map.hpp>
#include <cstdlib>
#include <filesystem>
#include "Level.hpp"
#include "LevelObject.hpp"

void sfmlMain(int argc, char *argv[]) {
  sf::RenderWindow window(sf::VideoMode({600, 450}), "Scheme-Man");
  window.setFramerateLimit(30);

  sf::Vector2f oldPos;
  bool mouse_clicked = false;

  Level level(argv[1]);
  level.setScale(0.25);

  sf::View view(sf::Vector2f(0, 0), sf::Vector2f(window.getSize()));

  LevelObject point(sf::Vector2f(0, 0));
  sf::CircleShape originPoint(5);
  originPoint.setFillColor(sf::Color::Green);

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

      if (event->is<sf::Event::MouseButtonReleased>())
        mouse_clicked = false;

      if (mouse_clicked) {
        if (const auto *dragged = event->getIf<sf::Event::MouseMoved>()) {
          const auto newPos = window.mapPixelToCoords(dragged->position);
          const auto deltaPos = oldPos - newPos;

          view.setCenter(view.getCenter() + deltaPos);
          window.setView(view);
        }
      }

      if (const auto *scroll = event->getIf<sf::Event::MouseWheelScrolled>()) {
        // TODO: Tenemos que hacer zoom respecto a la posición del
        // ratón para que se sienta más natural -- no respecto al
        // origen del nivel.
        auto t = window.mapPixelToCoords(scroll->position, view);
        point.position = t;

        float delta = 0.05 * scroll->delta;

        level.setScale(level.geometry.scale + delta);
        // view.move(t * delta);
      }
    }

    window.clear(level.background);
    window.setView(view);
    level.update();
    window.draw(level);

    if (1) {
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
