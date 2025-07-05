// A level in the game.

#pragma once

#include <SFML/System.hpp>
#include <SFML/Graphics.hpp>
#include <memory>
#include <print>
#include <tmxlite/Map.hpp>
#include <filesystem>
#include <vector>
#include "LevelGeometry.hpp"
#include "LevelObject.hpp"

constexpr float min_level_scale = 0.2;
constexpr float max_level_scale = 2.0;

struct Level : public sf::Drawable {
  std::filesystem::path tilemap;

  LevelGeometry geometry;

  tmx::Map map;

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
    if (s > 2.0) s = max_level_scale;
    if (s < 0.1) s = min_level_scale;
    geometry.scale = s;
  }

  [[nodiscard]]
  bool load(void);

  void update(void);

  void draw(sf::RenderTarget &target, sf::RenderStates states) const;

private:
  bool loadTextures(void);
};
