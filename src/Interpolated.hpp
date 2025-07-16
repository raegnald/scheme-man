// Generic interpolation.

#pragma once

#include <cmath>
#include <chrono>

enum Interpolating_function {
  Linear,
  Ease_out_quad,
  Ease_out_elastic
};

float interpolate(Interpolating_function type, float x);

template <typename T> struct Interpolated {
private:
  float m_startTime{};
  // float m_extra_t{0.0}; // used when an interpolation is interrupted

  static float getCurrentTime() {
    auto const now = std::chrono::steady_clock::now();
    auto const duration = now.time_since_epoch();
    auto const seconds =
        std::chrono::duration_cast<std::chrono::duration<float>>(duration);
    return seconds.count();
  }

  float getElapsed(void) {
    return getCurrentTime() - m_startTime;
  }

public:
  T start{};
  T end{};

  float duration{1.0}; // in seconds

  Interpolating_function interp{Interpolating_function::Linear};

  explicit Interpolated(T x) : start{x}, end{start} {}

  void resetInterpolation(void) { m_startTime = getCurrentTime(); }

  void setOrigin(const T &origin) {
    start = origin;
    end = origin;
    resetInterpolation();
  }

  void setTarget(const T &target) {
    start = getValue();
    end = target;
    resetInterpolation();
  }

  void setDuration(float d) { duration = d; }

  bool interpolationEnded(void) {
    return getElapsed() / duration  >= 1;
  }

  T getValue(void) const {
    const float elapsed = getCurrentTime() - m_startTime;
    const float t = elapsed / duration;

    if (t >= 1)
      return end;

    const T delta{end - start};
    return start + delta * interpolate(interp, t);
  }

  void setFunction(Interpolating_function type) { interp = type; }

  operator T() const { return getValue(); }
  operator T() { return getValue(); }
  void operator=(const T &target) { setTarget(target); }
};
