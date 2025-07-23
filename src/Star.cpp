#include "LevelObject.hpp"

Star::Star(sf::Vector2f t_position, LevelGeometry *t_geometry)
    : CollectableObject{t_position, t_geometry,
                        "../assets/Objects/Star/star01.png"} {

  frames.opacity.setDuration(0.2f);
  frames.setFrameDuration(sf::seconds(0.2));
  frames.setRandomiseStartingFrame(true);
  frames.scale = 4;

  const auto frame_paths = {"../assets/Objects/Star/star02.png",
                            "../assets/Objects/Star/star03.png",
                            "../assets/Objects/Star/star04.png",
                            "../assets/Objects/Star/star05.png",
                            "../assets/Objects/Star/star06.png",
                            "../assets/Objects/Star/star07.png",
                            "../assets/Objects/Star/star08.png",
                            "../assets/Objects/Star/star09.png",
                            "../assets/Objects/Star/star10.png",
                            "../assets/Objects/Star/star11.png",
                            "../assets/Objects/Star/star12.png",
                            "../assets/Objects/Star/star13.png"};
  for (const auto &frame_path : frame_paths)
    frames.addFrame(frame_path);
}

void Star::update(Player &player) {
  LevelObject::update(player);

  if (player.position.end == this->position.getValue()) {
    frames.opacity.setTarget(0);
    collect();
    player.reachedStar = true;
  }
}
