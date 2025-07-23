
#include "LevelObject.hpp"

Player::Player(LevelGeometry *geometry) : AnimatedObject(geometry) {
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
