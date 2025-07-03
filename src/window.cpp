#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <filesystem>

struct TileGrid : public sf::Drawable {
  sf::Texture texture;
  sf::Sprite sprite;
  sf::Vector2f size;
  sf::Vector2f gridOrigin;
  sf::Vector2u dimensions; // width and height of tile grid

  explicit TileGrid(std::filesystem::path texturePath, sf::Vector2u gridSize)
      : texture(texturePath), sprite(texture), dimensions(gridSize) {
    setScale(sf::Vector2f(1, 1));
  }

  void setScale(sf::Vector2f scale) {
    sf::Vector2f textureSize(texture.getSize());
    sprite.setScale(scale);
    size = sf::Vector2f(textureSize.x * scale.x, textureSize.y * scale.y);
    sprite.setOrigin(static_cast<float>(0.5) * textureSize);
  }

  void setGridOrigin(sf::Vector2f newOrigin) { gridOrigin = newOrigin; }

  void draw(sf::RenderTarget &target, sf::RenderStates states) const {
    sf::Sprite tile(sprite);

    for (int x = 0; x < dimensions.x; x++) {
      for (int y = 0; y < dimensions.y; y++) {
        sf::Vector2f pos = gridOrigin + sf::Vector2f(0.5 * (x - y) * size.x,
                                                     0.5 * (x + y) * size.y);
        tile.setPosition(pos);
        target.draw(tile);
      }
    }
  }
};

struct Room : public sf::Drawable {
  TileGrid floor; // , leftWall, rightWall;

  explicit Room(sf::Vector2u gridSize, std::filesystem::path floorTexturePath,
                sf::Vector2f scale = sf::Vector2f(1, 1))
      : floor(floorTexturePath, gridSize) {
    floor.setScale(scale);
  }

  void setOrigin(sf::Vector2f newOrigin) { floor.setGridOrigin(newOrigin); }

  void draw(sf::RenderTarget &target, sf::RenderStates states) const {
    target.draw(floor);
  }
};

struct TileSet {};

int main(void) {
  sf::RenderWindow window(sf::VideoMode({800, 600}), "Scheme-Man", sf::Style::Close);
  window.setFramerateLimit(30);

  constexpr auto backgroundColor = sf::Color(0xd8, 0xeb, 0xf9);

  Room room(sf::Vector2u(6, 6), std::filesystem::path("assets/assets_1/snow.png"));
  room.setOrigin(sf::Vector2f(static_cast<float>(window.getSize().x) / 2, 300));

  while (window.isOpen()) {
    while (const auto event = window.pollEvent()) {
      if (event->is<sf::Event::Closed>())
        window.close();
    }

    window.clear(backgroundColor);
    window.draw(room);
    window.display();
  }
}
