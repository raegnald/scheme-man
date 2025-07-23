// A texture that changes with time

#pragma once

#include "Interpolated.hpp"
#include "SFML/Graphics/Texture.hpp"
#include "SFML/System/Clock.hpp"
#include "SFML/System/Time.hpp"
#include <filesystem>
#include <vector>

constexpr auto default_animated_frame_duration = sf::seconds(0.2f);

struct AnimatedTexture {
private:
  std::vector<sf::Texture> frames;
  std::vector<sf::Texture> *frames_ref{nullptr};
  size_t current_frame_idx{0};

  sf::Time frame_duration{default_animated_frame_duration};
  sf::Clock clock;

  bool started{false};
  bool randomise_starting_frame{false};

  size_t frameCount(void) {
    return frames_ref ? frames_ref->size() : frames.size();
  }

public:
  Interpolated<float> opacity{1.0f};
  float scale{1.0f};

  // The animation must have at least one frame
  AnimatedTexture(void) = delete;
  AnimatedTexture(sf::Texture &first_frame) { frames.push_back(first_frame); }
  AnimatedTexture(std::filesystem::path &first_frame_path) {
    addFrame(first_frame_path);
  }

  void addFrame(const sf::Texture &frame) { frames.push_back(frame); }
  void addFrame(std::filesystem::path texture_path) {
    frames.push_back(sf::Texture(texture_path));
  }

  void update(void) {
    if (!started) {
      started = true;

      if (randomise_starting_frame)
        current_frame_idx = std::rand() % frameCount();
      else
        current_frame_idx = 0;
    }

    if (clock.getElapsedTime() > frame_duration) {
      clock.restart();
      current_frame_idx = (current_frame_idx + 1) % frameCount();
    }
  }

  const sf::Texture &getCurrentFrame(void) const {
    return (frames_ref ? *frames_ref : frames)[current_frame_idx];
  }

  void setFrameDuration(sf::Time duration) { frame_duration = duration; }

  void setRandomiseStartingFrame(bool set) {
    randomise_starting_frame = set;
    started = false;
  }

  void resetAnimation(void) { started = true; }

  void setReference(std::vector<sf::Texture> &ref) { frames_ref = &ref; }
  void resetReference(void) { frames_ref = nullptr; }
};
