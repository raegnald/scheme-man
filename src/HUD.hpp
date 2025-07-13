// A HUD (Heads-Up Display) that shows information regarding progress
// in a level.

#pragma once

#include "Level.hpp"
#include <SFML/Graphics.hpp>
#include <format>

struct HUD : public sf::Drawable {
private:
  const Level *level;

public:
  HUD(Level *t_level) : level(t_level) {}

  size_t coinsPercentage(void) const {
    return static_cast<int>(static_cast<float>(level->player.coins_collected) /
                            static_cast<float>(level->total_coins) * 100.0);
  }

  size_t getMeters(void) const { return level->player.meters_travelled; }

  void draw(sf::RenderTarget &target, sf::RenderStates states) const {
    const auto percentage = std::format("{}%", coinsPercentage());

    sf::Text coins_percentage{level->getFont()};
    coins_percentage.setString(percentage);

    target.draw(coins_percentage);
  }
};
