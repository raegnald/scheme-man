#include "Level.hpp"
#include "LevelGeometry.hpp"
#include <SFML/System.hpp>
#include <SFML/System/Vector2.hpp>
#include <cassert>
#include <cstdio>
#include <memory>
#include <tmxlite/Map.hpp>
#include <tmxlite/TileLayer.hpp>
#include <print>
#include <cstddef>

bool Level::loadTextures(void) {
  std::filesystem::path texturePaths[] = {
    "../assets/Floors/00-grass-dark-512x256.png",
    "../assets/Floors/01-grass-light-512x256.png",
    "../assets/Floors/02-stone-512x256.png",
    "../assets/Floors/03-brickpavers2-512x256.png",
    "../assets/Floors/04-concrete368a-512x256.png",
    "../assets/Floors/05-cretebrick970-512x256.png",
    "../assets/Floors/06-dirt-512x256.png",
    "../assets/Floors/07-dirtsand2-512x256.png",
    "../assets/Floors/08-rock-512x256.png",
    "../assets/Floors/09-snow-512x256.png"
  };

  for (auto &texturePath : texturePaths) {
    sf::Texture texture;

    if (!texture.loadFromFile(texturePath))
      std::println("Could not load texture {}", texturePath.c_str());

    if (texture.getSize() != geometry.tile_size)
      std::println("Tile {} does not have the correct size",
                   texturePath.c_str());

    textures.push_back(texture);
  }

  return true;
}

[[nodiscard]]
bool Level::load(void) {
  // Load level map (everything)
  bool loaded = map.load(tilemap);

  if (!loaded)
    return false;

  if (map.isInfinite()) {
    std::println("An infinite map: fuck off!");
    return false;
  }

  // Load textures
  loadTextures();

  geometry.dimensions =
    sf::Vector2u(map.getTileCount().x, map.getTileCount().y);

  // Load level objects
  for (auto &layer : map.getLayers()) {
    if (layer->getType() == tmx::Layer::Type::Tile &&
        layer->getName() == "Objects") {
      const auto *tile_layer =
        dynamic_cast<const tmx::TileLayer *>(layer.get());
      const auto &object_tiles = tile_layer->getTiles();
      const auto layer_size =
        sf::Vector2u(tile_layer->getSize().x, tile_layer->getSize().y);

      assert(layer_size == geometry.dimensions);

      for (int y = 0; y < layer_size.y; ++y) {
        for (int x = 0; x < layer_size.x; ++x) {
          Cannonical pos(x, y);
          const auto &object_tile = object_tiles[x + y * layer_size.x];

          // Coin
          if (object_tile.ID == 24) {
            auto coin = std::make_unique<Coin>(pos, &geometry);
            objects.push_back(std::move(coin));
          }

          // Star
          if (object_tile.ID == 11) {
            auto coin = std::make_unique<Star>(pos, &geometry);
            objects.push_back(std::move(coin));
          }
        }
      }
    }
  }

  return true;
}

void Level::update(void) {
  player.update(player);
  for (auto &levelObject : objects) {
    levelObject->update(player);
  }
}

void Level::draw(sf::RenderTarget &target, sf::RenderStates states) const {
  sf::Sprite sprite(textures[0]);
  sprite.setOrigin(static_cast<float>(0.5) * sprite.getGlobalBounds().size);

  for (auto &layer : map.getLayers()) {
    if (layer->getType() == tmx::Layer::Type::Tile &&
        layer->getName() == "Floor") {
      const auto *tile_layer =
          dynamic_cast<const tmx::TileLayer *>(layer.get());
      const auto &tiles = tile_layer->getTiles();
      const auto layer_size = tile_layer->getSize();
      const auto tile_size = map.getTileSize();

      sprite.setScale(sf::Vector2f(geometry.scale, geometry.scale));

      for (int y = 0; y < layer_size.y; ++y) {
        for (int x = 0; x < layer_size.x; ++x) {
          std::uint32_t tile_id = tiles[x + y * layer_size.x].ID;

          if (tile_id == 0)
            continue;

          if (tile_id <= textures.size())
            sprite.setTexture(textures[tile_id - 1]);

          // Isometric coordinate
          sf::Vector2f pos = geometry.isometric(sf::Vector2f(x, y));

          sprite.setPosition(pos);
          target.draw(sprite);
        }
      }
    }
  }

  for (const auto &levelObject : objects) {
    target.draw(*levelObject);
  }

  target.draw(player);
}
