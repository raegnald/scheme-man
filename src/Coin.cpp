#include "LevelObject.hpp"

sf::SoundBuffer Coin::coin_soundbuffer{"../assets/Objects/Coin/coin.mp3"};
sf::Sound Coin::coinfx{coin_soundbuffer};

Coin::Coin(Cannonical t_position, LevelGeometry *t_geometry)
    : CollectableObject{t_position, t_geometry,
                        "../assets/Objects/Coin/coin01.png"} {

  frames.opacity.setDuration(0.2f);
  frames.setFrameDuration(sf::seconds(0.2f));
  frames.setRandomiseStartingFrame(true);
  frames.scale = 4;

  // Adding rest of frames
  const auto frame_paths = {"../assets/Objects/Coin/coin02.png",
                            "../assets/Objects/Coin/coin03.png",
                            "../assets/Objects/Coin/coin04.png",
                            "../assets/Objects/Coin/coin05.png",
                            "../assets/Objects/Coin/coin06.png",
                            "../assets/Objects/Coin/coin07.png",
                            "../assets/Objects/Coin/coin08.png",
                            "../assets/Objects/Coin/coin09.png",
                            "../assets/Objects/Coin/coin10.png"};

  for (const auto &frame_path : frame_paths)
    frames.addFrame(frame_path);
}

void Coin::update(Player &player) {
  LevelObject::update(player);

  if (player.position.end == this->position.getValue()) {
    if (!collect())
      return;

    coinfx.play();
    player.coins_collected++;
    frames.opacity.setTarget(0);
  }
}
