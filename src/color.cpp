#include "color.hpp"
#include "SFML/Graphics/Color.hpp"
#include "SFML/System/Vector3.hpp"
#include <algorithm>

sf::Vector3f vectorFromColor(sf::Color color) {
  return (1.0f / 255) * sf::Vector3f(color.r, color.g, color.b);
}

sf::Color colorFromVector(sf::Vector3f v) {
  return sf::Color(std::clamp(static_cast<int>(255 * v.x), 0, 255),
                   std::clamp(static_cast<int>(255 * v.y), 0, 255),
                   std::clamp(static_cast<int>(255 * v.z), 0, 255));
}
