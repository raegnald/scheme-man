
#include "LevelObject.hpp"

Player::Player(LevelGeometry *geometry)
    : LevelObject{
          {0, 0}, geometry, "../assets/Objects/Character/idle-pos01.png"} {

  position.setDuration(0.25); // transition time
  position.setFunction(Interpolating_function::Ease_out_quad);

  frames.scale = 4;

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

void Player::update(Player &player) {
  if (walking && position.interpolationEnded()) {
    walking = false;
    needs_update = true;
  }

  // Standing idle in a positive direction
  if (needs_update &&
      (lookingat == Direction::Xpos || lookingat == Direction::Ypos) &&
      !walking) {
    frames.setFrameDuration(sf::seconds(1.0f));
    frames.setReference(idlePos);
    frames.setRandomiseStartingFrame(false);
    needs_update = false;
  }

  // Walking in a positive direction
  if (needs_update &&
      (lookingat == Direction::Xpos || lookingat == Direction::Ypos) &&
      walking) {
    frames.setFrameDuration(sf::seconds(1.0 / walkPos.size()));
    frames.setReference(walkPos);
    frames.setRandomiseStartingFrame(true);
    needs_update = false;
  }

  // Standing idle in a negative direction
  if (needs_update &&
      (lookingat == Direction::Yneg || lookingat == Direction::Xneg) &&
      !walking) {
    frames.setFrameDuration(sf::seconds(1.0f));
    frames.setReference(idleNeg);
    frames.setRandomiseStartingFrame(false);
    needs_update = false;
  }

  // Walking in a negative direction
  if (needs_update &&
      (lookingat == Direction::Yneg || lookingat == Direction::Xneg) &&
      walking) {
    frames.setFrameDuration(sf::seconds(1.0 / walkNeg.size()));
    frames.setReference(walkNeg);
    frames.setRandomiseStartingFrame(true);
    needs_update = false;
  }

  LevelObject::update(player);
}

void Player::turnClockwise(size_t n) {
  needs_update = true;
  lookingat.turnClockwise(n);
}

void Player::turnAnticlockwise(size_t n) {
  needs_update = true;
  lookingat.turnAnticlockwise(n);
}

sf::Vector2f Player::advancePosition(sf::Vector2f pos) const {
  const auto dx = ((lookingat == Direction::Xpos) ? 1 : (lookingat == Direction::Xneg) ? (-1) : 0);
  const auto dy = ((lookingat == Direction::Ypos) ? 1 : (lookingat == Direction::Yneg) ? (-1) : 0);
  return sf::Vector2f(pos.x + dx, pos.y + dy);
}

void Player::walk(void) {
  needs_update = true;
  walking = true;
  meters_travelled++;
  position.start = position.end;
  sf::Vector2f target = advancePosition(position.start);
  position.setTarget(target);
}

void Player::draw(sf::RenderTarget &target, sf::RenderStates states) const {
  sf::Sprite sprite{frames.getCurrentFrame()};

  sprite.setPosition(getIsometricPosition());

  if (lookingat == Direction::Ypos || lookingat == Direction::Yneg) {
    auto [width, height] = sprite.getGlobalBounds().size;
    const sf::IntRect rect(sf::Vector2i(width, 0),
                           sf::Vector2i(-width, height));
    sprite.setTextureRect(rect);
  }

  const auto size = sprite.getGlobalBounds().size;
  sprite.setOrigin(sf::Vector2f(0.5 * size.x, 0.75 * size.y));

  if (geometry)
    sprite.setScale(geometry->scale.getValue() * sf::Vector2f(frames.scale, frames.scale));

  target.draw(sprite);
}

void Player::reset(void) {
  position.setOrigin(start_position);
  coins_collected = 0;
  meters_travelled = 0;
  lookingat = start_direction;
  needs_update = true;
}
