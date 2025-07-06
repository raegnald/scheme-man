// Implementation of kinetic movement (mainly applied to
// natural-feeling scrolling).

#pragma once

#include <cstdlib>

template <typename T> struct Kinetic {
private:
  T m_x; // the value (oftentimes position)
  T m_v; // velocity
  T m_a; // acceleration
  T m_min_a; // minimum acceleration

  float m_coeff = 0.5; // coefficient of acceleration that remains
                       // after friction loss is applied
  bool m_movementStopped = false;
public:
  explicit Kinetic(T value) : m_x(value) {}

  void applyAcceleration(T acc) {
    m_a += acc;
    m_movementStopped = false;
  }

  void update(float dt) {
    if (m_movementStopped)
      return;

    if (std::abs(m_a) < m_min_a) {
      stopMovement();
      return;
    }

    m_a *= m_coeff;
    m_v = m_a * dt;
    m_x += m_v * dt;
  }

  void setMinAcceleration(T min) { m_min_a = min; }

  void setValue(T value) {
    m_x = value;
    stopMovement();
  }

  T getValue(void) const { return m_x; }

  void stopMovement() {
    m_a *= 0;
    m_movementStopped = true;
  }

  operator T() const { return getValue(); }
  operator T() { return getValue(); }
  void operator=(const T &x) { setValue(x); }
};
