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
    constexpr auto pos = sf::Vector2f(30, 30);
    constexpr auto padding = sf::Vector2f(10, 10);

    const auto percentage = std::format("{}%", coinsPercentage());

    // Icon
    const auto icon_pos = pos + sf::Vector2f(10, 10);
    static Coin icon{icon_pos, nullptr};

    icon.position.setOrigin(icon_pos);
    icon.updateAnimation();

    // Text
    sf::Text coins_percentage{level->getFont()};
    coins_percentage.setString(percentage);
    coins_percentage.setPosition(
        {pos.x + 3 * padding.x, pos.y - padding.y});

    auto text_bounds = coins_percentage.getGlobalBounds();

    // Rectangle
    sf::RectangleShape rect(
        text_bounds.size +
        sf::Vector2f(icon.getSize()).componentWiseMul({1, 0}) + 2.0f * padding);

    rect.setPosition(pos - padding);
    rect.setFillColor(sf::Color(0, 0, 0, 128));

    target.draw(rect);
    target.draw(icon);
    target.draw(coins_percentage);
  }
};
