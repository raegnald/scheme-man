// Isometric geometry of a level.

#pragma once

#include <SFML/System.hpp>

using Cannonical = sf::Vector2f;
using Isometric = sf::Vector2f;

struct LevelGeometry {
  sf::Vector2u dimensions;
  sf::Vector2f origin;
  sf::Vector2u tile_size = sf::Vector2u(512, 256);
  float scale = 1;

  /// Convert an orthonometric, 1:1 scale, position into its
  /// corresponding isometric projection.
  template <typename T> Isometric isometricProject(sf::Vector2<T> u) const {
    return origin +
           scale * sf::Vector2f((u.x - u.y), (u.x + u.y - tile_size.y) / 2);
  }

  /// Convert a coordinate into a projected, scaled, position in the
  /// isometric space of the level.
  template <typename T> Isometric isometric(sf::Vector2<T> u) const {
    return origin + scale * sf::Vector2f(0.5 * (u.x - u.y) * tile_size.x,
                                         0.5 * (u.x + u.y) * tile_size.y);
  }

  /// Get the normalised, cannonical (with basis vectors i, j)
  /// coordiate back from a point in the isometric space of tiles.
  template <typename T> sf::Vector2<T> cannonical(Isometric u) const {
    float a = (u.x - origin.x) / tile_size.x;
    float b = (u.y - origin.y) / tile_size.y;
    return sf::Vector2<T>((a + b) / scale, (b - a) / scale);
  }
};
