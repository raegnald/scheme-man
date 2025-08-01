// wxSFMLControl and Game

#pragma once

#include "SFML/Graphics/RenderWindow.hpp"
#include "wxSFMLControl.hpp"
#include "Game.hpp"

class wxSchemeMan : public wxSFMLControl {
private:
  Game game;

public:
  wxSchemeMan(wxWindow *parent, wxWindowID id, const wxPoint &pos,
              const wxSize &size)
      : wxSFMLControl(parent, id, pos, size),
        game(GetWindowHandle(),
             std::filesystem::path("../assets/Levels/001.tmx")) {}
  // TODO: Enable dynamic loading of levels ^^^^

  virtual void Frame(sf::RenderWindow &window) {
    game.update();
    game.draw();
  }
};
