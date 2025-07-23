// Objects that can hold state and have different functionalities and
// can be drawn inside of a level.

#pragma once

#include "LevelGeometry.hpp"
#include "Interpolated.hpp"
#include "SFML/Audio/Sound.hpp"
#include "SFML/Audio/SoundBuffer.hpp"
#include "debug.hpp"
#include <SFML/Graphics/Rect.hpp>
#include <SFML/System.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/System/Time.hpp>
#include <SFML/System/Vector2.hpp>
#include <cstdlib>
#include <filesystem>
#include <print>
#include <tmxlite/Object.hpp>
#include <vector>

struct Player;

struct LevelObject : public sf::Drawable {
  Interpolated<Cannonical> position{sf::Vector2f(0, 0)};
  LevelGeometry *geometry;

  /// Name of the object that is seen by Scheme that changes depending
  /// on the state of the object.
  virtual std::string name(void) { return "object"; }

  LevelObject(void) {}
  explicit LevelObject(LevelGeometry *t_geometry) : geometry(t_geometry) {}
  explicit LevelObject(Cannonical t_position, LevelGeometry *t_geometry)
      : position(t_position), geometry(t_geometry) {}

  void setPosition(Cannonical p) { position.start = p; }

  Isometric getPosition(void) const {
    if (geometry)
      return geometry->isometric(position.getValue());
    else
      return position.getValue();
  }

  virtual void update(Player &player) {}

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const {
    sf::CircleShape point(5);

    point.setPosition(getPosition());

    point.setFillColor(sf::Color::Magenta);
    target.draw(point);
  };

  virtual void reset(void) {}
};

struct TexturedObject : public LevelObject {
  sf::Texture texture;

  explicit TexturedObject(Cannonical t_position,
                          std::filesystem::path texturePath,
                          LevelGeometry *t_geometry)
      : LevelObject(t_position, t_geometry), texture(texturePath) {}

  explicit TexturedObject(sf::Vector2f t_position, sf::Texture t_texture,
                          LevelGeometry *t_geometry = nullptr)
      : LevelObject(t_position, t_geometry), texture(t_texture) {}

  sf::Vector2u getSize(void) const {
    return texture.getSize();
  }

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const override {
    sf::Sprite sprite(texture);

    sprite.setPosition(getPosition());

    sprite.setOrigin(0.5f * sf::Vector2f(texture.getSize()));
    target.draw(sprite);
  };
};

struct AnimatedObject : public LevelObject {
private:
  bool m_started = false;

  float m_frameDuration{0.1}; // in seconds
  bool m_randomiseStartingFrame = false;

public:
  std::vector<sf::Texture> frameTextures;
  size_t currentFrameIndex{0};

  sf::Clock clock;

  float scale = 1.0;
  Interpolated<float> opacity{1.0};
  bool flip = false;


  explicit AnimatedObject(LevelGeometry *t_geometry) : LevelObject(t_geometry) {
    opacity.setDuration(0.2);
  }

  explicit AnimatedObject(sf::Vector2f t_position, LevelGeometry *t_geometry)
      : LevelObject(t_position, t_geometry) {
    opacity.setDuration(0.2);
  }

  void addFrame(std::filesystem::path texturePath) {
    sf::Texture frameTexture(texturePath);
    frameTextures.push_back(frameTexture);
  }

  void addFrame(sf::Texture &frameTexture) {
    frameTextures.push_back(frameTexture);
  }

  void setRandomiseStartingFrame(bool set) {
    m_randomiseStartingFrame = set;
    m_started = false;
  }

  void setFrameDuration(float seconds) { m_frameDuration = seconds; }
  void setScale(float t_scale) { scale = t_scale; }

  sf::Vector2u getSize(void) const {
    return frameTextures[currentFrameIndex].getSize();
  }

  void updateAnimation(void) {
    if (frameTextures.empty())
      return;

    if (!m_started) {
      m_started = true;
      // Start with a random frame
      if (m_randomiseStartingFrame)
        currentFrameIndex = std::rand() % frameTextures.size();
    }

    if (clock.getElapsedTime().asSeconds() >= m_frameDuration) {
      currentFrameIndex++;
      currentFrameIndex %= frameTextures.size();
      clock.restart();
    }
  }

  virtual void update(Player &player) override { updateAnimation(); }

  virtual void draw(sf::RenderTarget &target,
                    sf::RenderStates states) const override {
    if (frameTextures.empty())
      return;

    sf::Sprite sprite(frameTextures[currentFrameIndex]);

    sprite.setPosition(getPosition());
    sprite.setOrigin(0.5f * sprite.getGlobalBounds().size);
    if (geometry)
      sprite.setScale(geometry->scale.getValue() * sf::Vector2f(scale, scale));

    sprite.setColor(sf::Color(
        255, 255, 255, std::clamp(static_cast<int>(255 * opacity), 0, 255)));

    target.draw(sprite);
  }
};

struct Direction {
public:
  enum Dir { Xpos, Ypos, Xneg, Yneg };
private:
  Dir m_direction;
public:

  Direction() : m_direction{Xpos} {}

  Direction(Dir d) : m_direction{d} {}

  Direction(const std::string &dir) : m_direction{Xpos} {
    if (dir == "+X") m_direction = Xpos;
    if (dir == "-X") m_direction = Xneg;
    if (dir == "+Y") m_direction = Ypos;
    if (dir == "-Y") m_direction = Yneg;

    debug std::println(stderr, "Invalid player direction {}", dir);
  }

  void turnClockwise(size_t n = 1) {
    while (n-- > 0) {
      switch (m_direction) {
      case Xpos:
        m_direction = Ypos;
        return;
      case Ypos:
        m_direction = Xneg;
        return;
      case Xneg:
        m_direction = Yneg;
        return;
      case Yneg:
        m_direction = Xpos;
        return;
      }
    }
  }

  void turnAnticlockwise(size_t n = 1) {
    while (n-- > 0) {
      switch (m_direction) {
      case Xpos:
        m_direction = Yneg;
        return;
      case Yneg:
        m_direction = Xneg;
        return;
      case Xneg:
        m_direction = Ypos;
        return;
      case Ypos:
        m_direction = Xpos;
        return;
      }
    }
  }

  operator Dir() const { return m_direction; }
};

struct Player : public AnimatedObject {
private:
  std::vector<sf::Texture> idlePos;
  std::vector<sf::Texture> idleNeg;
  std::vector<sf::Texture> walkPos;
  std::vector<sf::Texture> walkNeg;

public:


  virtual std::string name(void) final override { return "player"; }

  sf::Vector2f start_position;
  Direction start_direction;

  size_t coins_collected = 0;
  size_t meters_travelled = 0;

  bool reachedStar = false;
  bool walking = false;
  Direction lookingat;

  bool needsUpdate = true;


  explicit Player(LevelGeometry *geometry);

  void update(Player &player) override {
    if (walking && position.interpolationEnded()) {
      walking = false;
      needsUpdate = true;
    }

    // Standing idle in a positive direction
    if (needsUpdate &&
        (lookingat == Direction::Xpos || lookingat == Direction::Ypos) &&
        !walking) {
      setFrameDuration(1);
      frameTextures = idlePos;
      currentFrameIndex = 0;
      needsUpdate = false;
    }

    // Walking in a positive direction
    if (needsUpdate &&
        (lookingat == Direction::Xpos || lookingat == Direction::Ypos) &&
        walking) {
      setFrameDuration(1.0 / walkPos.size());
      frameTextures = walkPos;
      setRandomiseStartingFrame(true);
      needsUpdate = false;
    }

    // Standing idle in a negative direction
    if (needsUpdate &&
        (lookingat == Direction::Yneg || lookingat == Direction::Xneg) &&
        !walking) {
      setFrameDuration(1);
      frameTextures = idleNeg;
      currentFrameIndex = 0;
      needsUpdate = false;
    }

    // Walking in a negative direction
    if (needsUpdate &&
        (lookingat == Direction::Yneg || lookingat == Direction::Xneg) &&
        walking) {
      setFrameDuration(1.0 / walkNeg.size());
      frameTextures = walkNeg;
      setRandomiseStartingFrame(true);
      needsUpdate = false;
    }

    AnimatedObject::update(player);
  }

  void turnClockwise(size_t n = 1) {
    needsUpdate = true;
    lookingat.turnClockwise(n);
  }

  void turnAnticlockwise(size_t n = 1) {
    needsUpdate = true;
    lookingat.turnAnticlockwise(n);
  }

  sf::Vector2f advancePosition(sf::Vector2f pos) {
    const auto dx = ((lookingat == Direction::Xpos) ? 1 : (lookingat == Direction::Xneg) ? (-1) : 0);
    const auto dy = ((lookingat == Direction::Ypos) ? 1 : (lookingat == Direction::Yneg) ? (-1) : 0);
    return sf::Vector2f(pos.x + dx, pos.y + dy);
  }

  void walk(void) {
    needsUpdate = true;
    walking = true;
    meters_travelled++;
    position.start = position.end;
    sf::Vector2f target = advancePosition(position.start);
    position.setTarget(target);
  }

  void draw(sf::RenderTarget &target, sf::RenderStates states) const override {
    if (frameTextures.empty())
      return;

    sf::Sprite sprite(frameTextures[currentFrameIndex]);

    if (geometry)
      sprite.setPosition(getPosition());

    if (lookingat == Direction::Ypos || lookingat == Direction::Yneg) {
      auto [width, height] = sprite.getGlobalBounds().size;
      const sf::IntRect rect(sf::Vector2i(width, 0),
                             sf::Vector2i(-width, height));
      sprite.setTextureRect(rect);
    }

    const auto size = sprite.getGlobalBounds().size;
    sprite.setOrigin(sf::Vector2f(0.5 * size.x, 0.75 * size.y));
    if (geometry)
      sprite.setScale(geometry->scale.getValue() * sf::Vector2f(scale, scale));
    target.draw(sprite);
  }

  void reset(void) final override {
    position.setOrigin(start_position);
    coins_collected = 0;
    meters_travelled = 0;
    lookingat = start_direction;
    needsUpdate = true;
  }
};



struct Collectable {
  bool collected = false;

  bool collect(void) {
    if (!collected)
      return (collected = true);
    else
      return false;
  }
};

// Specific game objects:

struct Coin : public AnimatedObject, public Collectable {

  static sf::SoundBuffer coin_soundbuffer;
  static sf::Sound coinfx;

  virtual std::string name(void) final override {
    return collected ? "" : "coin";
  }

  Coin(Cannonical t_position, LevelGeometry *t_geometry)
      : AnimatedObject(t_position, t_geometry) {
    setFrameDuration(0.2);
    setScale(4);
    addFrame("../assets/Objects/Coin/coin01.png");
    addFrame("../assets/Objects/Coin/coin02.png");
    addFrame("../assets/Objects/Coin/coin03.png");
    addFrame("../assets/Objects/Coin/coin04.png");
    addFrame("../assets/Objects/Coin/coin05.png");
    addFrame("../assets/Objects/Coin/coin06.png");
    addFrame("../assets/Objects/Coin/coin07.png");
    addFrame("../assets/Objects/Coin/coin08.png");
    addFrame("../assets/Objects/Coin/coin09.png");
    addFrame("../assets/Objects/Coin/coin10.png");
    setRandomiseStartingFrame(true);
  }

  void update(Player &player) final override {
    AnimatedObject::update(player);

    if (player.position.end == this->position.getValue()) {
      if (!collect())
        return;

      coinfx.play();
      player.coins_collected++;
      opacity.setTarget(0);
    }
  }

  virtual void draw(sf::RenderTarget &target,
                    sf::RenderStates states) const override {
    AnimatedObject::draw(target, states);
  }

  void reset(void) override {
    collected = false;
    opacity.setTarget(1);
  }
};

struct Star : public AnimatedObject, public Collectable {

  virtual std::string name(void) final override {
    return collected ? "" : "star";
  }

  Star(sf::Vector2f t_position, LevelGeometry *t_geometry)
      : AnimatedObject(t_position, t_geometry) {
    setFrameDuration(0.2);
    setScale(4);
    addFrame("../assets/Objects/Star/star01.png");
    addFrame("../assets/Objects/Star/star02.png");
    addFrame("../assets/Objects/Star/star03.png");
    addFrame("../assets/Objects/Star/star04.png");
    addFrame("../assets/Objects/Star/star05.png");
    addFrame("../assets/Objects/Star/star06.png");
    addFrame("../assets/Objects/Star/star07.png");
    addFrame("../assets/Objects/Star/star08.png");
    addFrame("../assets/Objects/Star/star09.png");
    addFrame("../assets/Objects/Star/star10.png");
    addFrame("../assets/Objects/Star/star11.png");
    addFrame("../assets/Objects/Star/star12.png");
    addFrame("../assets/Objects/Star/star13.png");
    setRandomiseStartingFrame(true);
  }

  void update(Player &player) final override {
    AnimatedObject::update(player);

    if (player.position.end == this->position.getValue()) {
      opacity.setTarget(0);
      collect();
      player.reachedStar = true;
    }
  }

  virtual void draw(sf::RenderTarget &target,
                    sf::RenderStates states) const override {
    AnimatedObject::draw(target, states);
  }

  void reset(void) override {
    collected = false;
    opacity.setTarget(1);
  }
};
