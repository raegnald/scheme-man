// Objects that can hold state and have different functionalities and
// can be drawn inside of a level.

#pragma once

#include "AnimatedTexture.hpp"
#include "LevelGeometry.hpp"
#include "Interpolated.hpp"
#include "SFML/Audio/Sound.hpp"
#include "SFML/Audio/SoundBuffer.hpp"
#include <SFML/Graphics/Rect.hpp>
#include <SFML/System.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/System/Time.hpp>
#include <SFML/System/Vector2.hpp>
#include <cstdlib>
#include <print>
#include <tmxlite/Object.hpp>
#include <vector>
#include "Direction.hpp"

struct Player;

struct Resettable {
  virtual void reset(void) = 0;
};

struct LevelObject : public sf::Drawable, public Resettable {
  AnimatedTexture frames;
  Interpolated<Cannonical> position{{0, 0}};
  LevelGeometry *geometry;

  Cannonical start_position;
  float start_opacity{1.0f};

  LevelObject(void) = delete;
  LevelObject(Cannonical t_start_position, LevelGeometry *t_geometry,
              std::filesystem::path t_first_frame_path)
      : position{t_start_position}, start_position{t_start_position},
        geometry{t_geometry}, frames{t_first_frame_path} {}

  // The name of the object seen by Scheme. This name may change
  // depending on the state of the level object. It should not contain
  // spaces; replace them with hyphens. All characters should be
  // lowercase.
  virtual std::string name(void) const { return "object"; }

  virtual void update(Player &player) { frames.update(); }

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const {
    sf::Sprite sprite{frames.getCurrentFrame()};

    sprite.setPosition(getIsometricPosition());
    sprite.setOrigin(0.5f * sprite.getGlobalBounds().size);

    if (geometry)
      sprite.setScale(geometry->scale.getValue() *
                      sf::Vector2f(frames.scale, frames.scale));

    const auto alpha = std::clamp(static_cast<int>(255 * frames.opacity), 0, 255);
    sprite.setColor(sf::Color(255, 255, 255, alpha));

    target.draw(sprite);
  }

  virtual void reset(void) {
    position.setOrigin(start_position);
    frames.opacity.setTarget(start_opacity);
  }

  void setCannonicalPosition(Cannonical p) { position.start = p; }
  Isometric getIsometricPosition(void) const {
    if (geometry)
      return geometry->isometric(position.getValue());
    else
      return position.getValue();
  }

  sf::Vector2u getSize(void) const {
    return frames.getCurrentFrame().getSize();
  }
};

struct Player final : public LevelObject {
private:
  std::vector<sf::Texture> idlePos, idleNeg, walkPos, walkNeg;
  bool needs_update = true;
public:
  // Start values are used to reset the player
  sf::Vector2f start_position;
  Direction start_direction;

  size_t coins_collected = 0;
  size_t keys_collected = 0;
  size_t meters_travelled = 0;

  bool reachedStar = false;
  bool walking = false;
  Direction lookingat;

  explicit Player(LevelGeometry *geometry);

  std::string name(void) const override { return "player"; }

  void update(Player &player) override;
  void draw(sf::RenderTarget &target, sf::RenderStates states) const override;
  void reset(void) override;

  void walk(void);
  void turnClockwise(size_t n = 1);
  void turnAnticlockwise(size_t n = 1);

  sf::Vector2f advancePosition(sf::Vector2f pos) const;
};

struct CollectableObject : public LevelObject {
  bool collected;

  CollectableObject(Cannonical t_position, LevelGeometry *t_geometry,
                    std::filesystem::path t_first_frame_path,
                    bool t_collected = false)
      : LevelObject{t_position, t_geometry, t_first_frame_path},
        collected{t_collected} {}

  bool collect(void) {
    if (!collected)
      return (collected = true);
    else
      return false;
  }

  virtual void reset() override {
    LevelObject::reset();
    collected = false;
  }
};

// Specific game objects

struct Coin : public CollectableObject {
private:
  static sf::SoundBuffer coin_soundbuffer;
  static sf::Sound coinfx;

public:
  Coin(Cannonical t_position, LevelGeometry *t_geometry);

  virtual std::string name(void) const final override {
    return collected ? "" : "coin";
  }

  void update(Player &player) final override;
};

struct Star : public CollectableObject {

  virtual std::string name(void) const final override {
    return collected ? "" : "star";
  }

  Star(sf::Vector2f t_position, LevelGeometry *t_geometry);

  void update(Player &player) final override;
};
