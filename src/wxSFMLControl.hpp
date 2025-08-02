// SFML render integration with wxWidgets

#pragma once

#include "SFML/Graphics/RenderWindow.hpp"
#include "SFML/Window/WindowHandle.hpp"
#include <wx/wx.h>
#include <wx/control.h>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>

class wxSFMLControl : public wxControl {
public:
  float FPS = 60.0f;

  wxSFMLControl(wxWindow *parent, wxWindowID id, const wxPoint &pos,
                const wxSize &size);
  ~wxSFMLControl(void);

  sf::WindowHandle GetWindowHandle(void);

  virtual void Frame(sf::RenderWindow &window) = 0;
  void Render(void);

  virtual void OnScroll(wxMouseEvent& event) {}
  virtual void OnMotion(wxMouseEvent& event) {}
  virtual void OnClose(wxCloseEvent& event) {}

private:
  sf::RenderWindow *m_sfml_window;
  bool m_window_initialised = false;
  wxTimer m_render_timer;

  void InitSFML(void);

  void OnRenderTimer(wxTimerEvent &event);
  void OnShow(wxShowEvent &event);
  void OnSize(wxSizeEvent& event);
};
