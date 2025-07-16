// Objects that can hold state and have different functionalities and
// can be drawn inside of a level.

#pragma once

#include "LevelGeometry.hpp"
#include "Interpolated.hpp"
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

struct Player : public AnimatedObject {
private:
  std::vector<sf::Texture> idlePos;
  std::vector<sf::Texture> idleNeg;
  std::vector<sf::Texture> walkPos;
  std::vector<sf::Texture> walkNeg;

public:
  enum LookingAt { Xpos, Ypos, Xneg, Yneg };

  sf::Vector2f start_position;

  size_t coins_collected = 0;
  size_t meters_travelled = 0;

  bool reachedStar = false;
  bool walking = false;
  LookingAt direction{Xpos};

  bool needsUpdate = true;

  static LookingAt parseDirection(const std::string &dir) {
    if (dir == "+X") return Xpos;
    if (dir == "-X") return Xneg;
    if (dir == "+Y") return Ypos;
    if (dir == "-Y") return Yneg;

    debug std::println(stderr, "Invalid player direction {}", dir);
    return Xpos;
  }

  explicit Player(LevelGeometry *geometry)
      : AnimatedObject(geometry) {

    position.setDuration(0.25); // transition time
    position.setFunction(Interpolating_function::Ease_out_quad);

    scale = 4;

    idlePos.push_back(sf::Texture("../assets/Objects/Character/idle-pos01.png"));
    idlePos.push_back(sf::Texture("../assets/Objects/Character/idle-pos02.png"));

    idleNeg.push_back(sf::Texture("../assets/Objects/Character/idle-neg01.png"));
    idleNeg.push_back(sf::Texture("../assets/Objects/Character/idle-neg02.png"));

    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos01.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos02.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos03.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos04.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos05.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos06.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos07.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos08.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos09.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos10.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos11.png"));
    walkPos.push_back(sf::Texture("../assets/Objects/Character/walk-pos12.png"));

    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg01.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg02.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg03.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg04.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg05.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg06.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg07.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg08.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg09.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg10.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg11.png"));
    walkNeg.push_back(sf::Texture("../assets/Objects/Character/walk-neg12.png"));
  }

  void update(Player &player) override {
    if (walking && position.interpolationEnded()) {
      walking = false;
      needsUpdate = true;
    }

    // Standing idle in a positive direction
    if (needsUpdate && (direction == Xpos || direction == Ypos) && !walking) {
      setFrameDuration(1);
      frameTextures = idlePos;
      currentFrameIndex = 0;
      needsUpdate = false;
    }

    // Walking in a positive direction
    if (needsUpdate && (direction == Xpos || direction == Ypos) && walking) {
      setFrameDuration(1.0 / walkPos.size());
      frameTextures = walkPos;
      setRandomiseStartingFrame(true);
      needsUpdate = false;
    }

    // Standing idle in a negative direction
    if (needsUpdate && (direction == Yneg || direction == Xneg) && !walking) {
      setFrameDuration(1);
      frameTextures = idleNeg;
      currentFrameIndex = 0;
      needsUpdate = false;
    }

    // Walking in a negative direction
    if (needsUpdate && (direction == Yneg || direction == Xneg) && walking) {
      setFrameDuration(1.0 / walkNeg.size());
      frameTextures = walkNeg;
      setRandomiseStartingFrame(true);
      needsUpdate = false;
    }

    AnimatedObject::update(player);
  }

  void turnClockwise(size_t n = 1) {
    needsUpdate = true;
    while (n-- > 0) {
      switch (direction) {
      case Xpos:
        direction = Ypos;
        continue;
      case Ypos:
        direction = Xneg;
        continue;
      case Xneg:
        direction = Yneg;
        continue;
      case Yneg:
        direction = Xpos;
        continue;
      }
    }
  }

  void turnAnticlockwise(size_t n = 1) {
    needsUpdate = true;
    while (n-- > 0) {
      switch (direction) {
      case Xpos:
        direction = Yneg;
        continue;
      case Yneg:
        direction = Xneg;
        continue;
      case Xneg:
        direction = Ypos;
        continue;
      case Ypos:
        direction = Xpos;
        continue;
      }
    }
  }

  void walk(void) {
    needsUpdate = true;
    walking = true;
    meters_travelled++;
    position.start = position.end;
    sf::Vector2f target(position.start.x + ((direction == Xpos) ? 1 : (direction == Xneg) ? (-1) : 0),
                        position.start.y + ((direction == Ypos) ? 1 : (direction == Yneg) ? (-1) : 0));
    position.setTarget(target);
  }

  void draw(sf::RenderTarget &target, sf::RenderStates states) const override {
    if (frameTextures.empty())
      return;

    sf::Sprite sprite(frameTextures[currentFrameIndex]);

    if (geometry)
      sprite.setPosition(getPosition());

    if (direction == Ypos || direction == Yneg) {
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
