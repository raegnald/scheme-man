// Objects that can hold state and have different functionalities and
// can be drawn inside of a level.

#pragma once

#include "LevelGeometry.hpp"
#include <SFML/System.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/System/Time.hpp>
#include <SFML/System/Vector2.hpp>
#include <cstdlib>
#include <filesystem>
#include <tmxlite/Object.hpp>
#include <vector>

struct LevelObject : public sf::Drawable {
  sf::Vector2f position; // not in isometric tile space
  LevelGeometry *geometry;

  LevelObject(void) {}
  explicit LevelObject(sf::Vector2f t_position,
                       LevelGeometry *t_geometry = nullptr)
      : position(t_position), geometry(t_geometry) {}

  virtual void update(void) {}

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const {
    sf::CircleShape point(5);

    if (geometry)
      point.setPosition(geometry->isometricProject(position));
    else
      point.setPosition(position);

    point.setFillColor(sf::Color::Magenta);
    target.draw(point);
  };
};

struct TexturedObject : public LevelObject {
  sf::Texture texture;

  explicit TexturedObject(sf::Vector2f t_position,
                          std::filesystem::path texturePath,
                          LevelGeometry *t_geometry = nullptr)
      : LevelObject(t_position, t_geometry), texture(texturePath) {}

  explicit TexturedObject(sf::Vector2f t_position, sf::Texture t_texture,
                          LevelGeometry *t_geometry = nullptr)
      : LevelObject(t_position, t_geometry), texture(t_texture) {}

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const override {
    sf::Sprite sprite(texture);

    if (geometry)
      sprite.setPosition(geometry->isometricProject(position));

    sprite.setOrigin(static_cast<float>(0.5) * sf::Vector2f(texture.getSize()));
    target.draw(sprite);
  };
};

struct AnimatedObject : public LevelObject {
private:
  bool m_started = false;
public:
  std::vector<sf::Texture> frameTextures{};
  size_t currentFrameIndex{0};
  sf::Clock clock;
  float frameDuration{0.1}; // in seconds
  float scale = 1;
  bool randomiseStartingFrame = false;

  explicit AnimatedObject(sf::Vector2f t_position,
                          LevelGeometry *t_geometry = nullptr)
      : LevelObject(t_position, t_geometry) {}

  void addFrame(std::filesystem::path texturePath) {
    sf::Texture frameTexture(texturePath);
    frameTextures.push_back(frameTexture);
  }

  void addFrame(sf::Texture &frameTexture) {
    frameTextures.push_back(frameTexture);
  }

  void setRandomiseStartingFrame(bool set) { randomiseStartingFrame = true; }

  void setFrameDuration(float seconds) { frameDuration = seconds; }
  void setScale(float t_scale) { scale = t_scale; }

  virtual void update(void) override {
    if (frameTextures.empty())
      return;

    // Start with a random frame
    if (!m_started && randomiseStartingFrame) {
      currentFrameIndex = std::rand() % frameTextures.size();
      m_started = true;
    }

    if (clock.getElapsedTime().asSeconds() >= frameDuration) {
      currentFrameIndex++;
      currentFrameIndex %= frameTextures.size();
      clock.restart();
    }
  }

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const override {
    if (frameTextures.empty())
      return;

    sf::Sprite sprite(frameTextures[currentFrameIndex]);

    if (geometry) {
      sprite.setPosition(geometry->isometricProject(position));
    } else {
      sprite.setPosition(position);
    }
    sprite.setOrigin(static_cast<float>(0.5) * sprite.getGlobalBounds().size);
    sprite.setScale(geometry->scale * sf::Vector2f(scale, scale));
    target.draw(sprite);
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

  Coin(sf::Vector2f t_position, LevelGeometry *t_geometry)
      : AnimatedObject(t_position, t_geometry) {
    setFrameDuration(0.2);
    setScale(4);
    addFrame("../assets/Objects/Coin/coin1.png");
    addFrame("../assets/Objects/Coin/coin2.png");
    addFrame("../assets/Objects/Coin/coin3.png");
    addFrame("../assets/Objects/Coin/coin4.png");
    addFrame("../assets/Objects/Coin/coin5.png");
    addFrame("../assets/Objects/Coin/coin6.png");
    addFrame("../assets/Objects/Coin/coin7.png");
    addFrame("../assets/Objects/Coin/coin8.png");
    addFrame("../assets/Objects/Coin/coin9.png");
    addFrame("../assets/Objects/Coin/coin10.png");
    setRandomiseStartingFrame(true);
  }

  virtual void draw(sf::RenderTarget &target, sf::RenderStates states) const override {
    AnimatedObject::draw(target, states);
  }
};

struct Star : public AnimatedObject, public Collectable {

  Star(sf::Vector2f t_position, LevelGeometry *t_geometry)
      : AnimatedObject(t_position, t_geometry) {
    sf::Texture starTexture("../assets/Objects/Star.png");
    sf::Texture starFrame;

    setFrameDuration(0.2);
    setScale(4);

    sf::Sprite s(starFrame);
    // s.setTextureRect(const IntRect &rectangle);
  }

};
