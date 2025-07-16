#include "Level.hpp"
#include "Interpolated.hpp"
#include "LevelGeometry.hpp"
#include "tmxlite/ObjectGroup.hpp"
#include <SFML/System.hpp>
#include <SFML/System/Vector2.hpp>
#include <cassert>
#include <cmath>
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

  // Level geometry
  geometry.dimensions =
    sf::Vector2u(map.getTileCount().x, map.getTileCount().y);

  geometry.scale.setMinAcceleration(1);

  const auto tilesets = map.getAnimatedTiles();

  for (auto &layer : map.getLayers()) {

    // Positions layer
    if (layer->getType() == tmx::Layer::Type::Object &&
        layer->getName() == "Positions") {


      const auto *object_group =
          dynamic_cast<const tmx::ObjectGroup *>(layer.get());
      const auto objects = object_group->getObjects();

      for (const auto &object : objects) {
        const auto &name = object.getName();
        auto [x, y] = object.getPosition();

        if (name == "start") {
          const auto p = geometry.isometricProject(sf::Vector2f(x, y));
          const Cannonical c = geometry.cannonical<float>(p);
          player.start_position = {std::round(c.x), std::round(c.y)};
          player.position.setOrigin(player.start_position);

          for (const auto &prop : object.getProperties()) {
            const auto &name = prop.getName();
            if (name == "lookingat") {
              const auto &dir_str = prop.getStringValue();
              player.direction = Player::parseDirection(dir_str);
            }
          }
        }
      }
    }

    // Load level objects
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

          if (object_tile.ID == 0)
            continue;

          try {
            const auto tile = tilesets.at(object_tile.ID);
            const auto object_class = tile.className;

            // Coin
            if (object_class == "coin") {
              auto coin = std::make_unique<Coin>(pos, &geometry);
              objects.push_back(std::move(coin));
              total_coins++;
            }

            // Star
            else if (object_class == "star") {
              auto coin = std::make_unique<Star>(pos, &geometry);
              objects.push_back(std::move(coin));
            }

            else {
              debug std::println(stderr, "Unknown class name {} for tile",
                                 object_class);
            }

          } catch (std::out_of_range _) {
            debug std::println("Unknown tile ID {}", object_tile.ID);
            continue;
          }
        }
      }
    }
  }

  debug std::println("Loaded {} level objects", objects.size());

  interpreter.initialise();

  return true;
}

void Level::update(float dt) {
  // Update level geometry (specially scale)
  geometry.update(dt);
  setScale(geometry.scale); // ensures scale is between min and max

  interpreter.update(this);

  // Update player and objects
  player.update(player);
  for (auto &levelObject : objects) {
    levelObject->update(player);
  }
}

void Level::draw(sf::RenderTarget &target, sf::RenderStates states) const {
  // Floor tiles
  sf::Sprite sprite(textures[0]);
  sprite.setOrigin(0.5f * sprite.getGlobalBounds().size);

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

  // Level objects
  for (const auto &levelObject : objects) {
    target.draw(*levelObject);
  }

  // Player
  target.draw(player);

  // Player status
  if (const auto status = getStatus()) {
    const bool too_small = (geometry.scale < 0.25);

    const auto players_head =
        geometry.isometric(player.position.getValue()) -
        sf::Vector2f(0, 3.75 * player.getSize().y * geometry.scale);

    if (too_small) {
      static sf::Texture lightBulb_texture(
          "../assets/Objects/Light-bulb/light-bulb.png");
      sf::Sprite lightBulb(lightBulb_texture);
      lightBulb.setPosition(players_head);
      lightBulb.setOrigin(0.5f * lightBulb.getGlobalBounds().size);
      lightBulb.setScale(1.5f * geometry.scale * sf::Vector2f(1, 1));

      target.draw(lightBulb);
    } else {
      sf::Text text{m_font, status.value()};
      text.setCharacterSize(30);
      text.setFillColor(sf::Color::White);
      text.setOutlineColor(sf::Color::Black);
      text.setOutlineThickness(2.0);

      text.setPosition(players_head);
      text.setOrigin(0.5f * text.getGlobalBounds().size);
      text.setScale(2 * geometry.scale * sf::Vector2f(1, 1));

      target.draw(text);
    }
  }
}


int Level::getFloorID(Cannonical coord) const {
  for (auto &layer : map.getLayers()) {
    if (layer->getType() == tmx::Layer::Type::Tile &&
        layer->getName() == "Floor") {
      const auto *tile_layer =
        dynamic_cast<const tmx::TileLayer *>(layer.get());
      const auto &tiles = tile_layer->getTiles();
      const auto layer_size = tile_layer->getSize();

      if (coord.x < 0 || coord.y < 0 || coord.x >= layer_size.x ||
          coord.y >= layer_size.y)
        return 0;

      return tiles[coord.x + coord.y * layer_size.x].ID;
    }
  }

  return 0;
}

bool Level::isFloor(Cannonical coord) const {
  return getFloorID(coord) != 0;
}

void Level::reset(void) {
  active = true;
  player.reset();
  for (auto &object : objects) {
    object->reset();
  }
}

void Level::shutdown(void) {
  interpreter.shutdown();
}
