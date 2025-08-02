// wxSFMLControl and Game

#pragma once

#include "SFML/Graphics/RenderWindow.hpp"
#include "wx/event.h"
#include "wxSFMLControl.hpp"
#include "Game.hpp"

class wxSchemeMan : public wxSFMLControl {
public:
  Game game;

  wxSchemeMan(wxWindow *parent, wxWindowID id, const wxPoint &pos,
              const wxSize &size)
      : wxSFMLControl(parent, id, pos, size),
        game(GetWindowHandle(),
             std::filesystem::path("../assets/Levels/001.tmx")) {}
  // TODO: Enable dynamic loading of levels ^^^^

  void Frame(sf::RenderWindow &window) override {
    game.update();
    game.draw();
  }

  void OnScroll(wxMouseEvent &event) override {
    // I hate having to use magical constants, and I bet you do too...
    game.handleMouseScroll(event.GetWheelRotation() * event.GetWheelDelta() *
                           0.001);
  }

  void OnMotion(wxMouseEvent &event) override {
    if (event.LeftDown())
      std::println("Button down!");

    if (!event.Dragging())
      return;

    auto [x, y] = event.GetPosition();
    game.setMousePosition(sf::Vector2i(x, y));
    // std::println("Set mouse pos to {}, {}", x, y);
  }

  void OnClose(wxCloseEvent &event) override { game.close(); }
};
