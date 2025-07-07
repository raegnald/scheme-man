// A level in the game.

#pragma once

#include <SFML/System.hpp>
#include <SFML/Graphics.hpp>
#include <memory>
#include <optional>
#include <print>
#include <tmxlite/Map.hpp>
#include <filesystem>
#include <vector>
#include "LevelGeometry.hpp"
#include "LevelObject.hpp"
#include "Lisp.hpp"

struct Level : public sf::Drawable {
private:
  tmx::Map map;
  sf::Font m_font{"../assets/Fonts/Pangolin.ttf"};

  // Status text
  std::string m_status_text{};
  sf::Clock m_status_clock{};
  bool m_status_permanent{false};
  static constexpr float m_status_duration = 5.0; // in seconds

  Lisp interpreter;

public:
  std::filesystem::path tilemap;

  LevelGeometry geometry;


  std::vector<sf::Texture> textures;

  std::vector<std::unique_ptr<LevelObject>> objects;
  Player player{&geometry};

  bool active = true;

  sf::Color background = sf::Color(0xd8, 0xeb, 0xf9);


  explicit Level(std::filesystem::path t_tilemap) : tilemap(t_tilemap) {
    if (!load())
      std::println("Could not load level");
  }

  void setOrigin(sf::Vector2f u) { geometry.origin = u; }
  void setScale(float s) {
    geometry.scale.setValue(s);
    geometry.ensureScaleInBounds();
  }

  [[nodiscard]]
  bool load(void);

  void update(float dt);

  void draw(sf::RenderTarget &target, sf::RenderStates states) const;

  void setStatus(const std::string_view s, bool permanent = false) {
    m_status_text = s;
    m_status_clock.restart();
    m_status_permanent = permanent;
  }

  void clearStatus(void) {
    m_status_text.clear();
    m_status_clock.reset();
    m_status_permanent = false;
  }

  std::optional<const std::string> getStatus(void) const {
    if (m_status_permanent)
      return m_status_text;

    if (m_status_clock.isRunning() &&
        m_status_clock.getElapsedTime().asSeconds() < m_status_duration)
      return m_status_text;

    return std::nullopt;
  }

private:
  bool loadTextures(void);
};
