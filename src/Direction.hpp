// A direction in 2D space

#pragma once

#include "debug.hpp"
#include <string>
#include <print>

struct Direction {
public:
  enum Dir { Xpos, Ypos, Xneg, Yneg };
private:
  Dir m_direction;
public:

  Direction() : m_direction{Xpos} {}

  Direction(Dir d) : m_direction{d} {}

  Direction(const std::string &dir) : m_direction{Xpos} {
    if (dir == "+X")
      m_direction = Xpos;
    else if (dir == "-X")
      m_direction = Xneg;
    else if (dir == "+Y")
      m_direction = Ypos;
    else if (dir == "-Y")
      m_direction = Yneg;
    else
      debug std::println(stderr, "Invalid player direction {}", dir);
  }

  void turnClockwise(size_t n = 1) {
    while (n-- > 0) {
      switch (m_direction) {
      case Xpos:
        m_direction = Ypos;
        return;
      case Ypos:
        m_direction = Xneg;
        return;
      case Xneg:
        m_direction = Yneg;
        return;
      case Yneg:
        m_direction = Xpos;
        return;
      }
    }
  }

  void turnAnticlockwise(size_t n = 1) {
    while (n-- > 0) {
      switch (m_direction) {
      case Xpos:
        m_direction = Yneg;
        return;
      case Yneg:
        m_direction = Xneg;
        return;
      case Xneg:
        m_direction = Ypos;
        return;
      case Ypos:
        m_direction = Xpos;
        return;
      }
    }
  }

  operator Dir() const { return m_direction; }
};
