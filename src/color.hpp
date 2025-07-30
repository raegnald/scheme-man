// Operations on SFML colors

#pragma once

#include "SFML/Graphics/Color.hpp"
#include "SFML/System/Vector3.hpp"

sf::Vector3f vectorFromColor(sf::Color color);
sf::Color colorFromVector(sf::Vector3f v);
