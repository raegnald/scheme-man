#include "wxSFMLControl.hpp"
#include "SFML/Window/WindowHandle.hpp"
#include "wx/event.h"
#include <print>
#include <tuple>

#ifdef __WXGTK__
# include <gdk/gdkx.h>
# include <gtk/gtk.h>
#endif

wxSFMLControl::wxSFMLControl(wxWindow *parent, wxWindowID id,
                             const wxPoint &pos, const wxSize &size)
    : wxControl(parent, id, pos, size, wxBORDER_NONE), m_render_timer(this) {
  // Avoids flickering when SFML is drawing
  SetBackgroundStyle(wxBG_STYLE_CUSTOM);

  m_render_timer.Start(1000 / FPS);

  Bind(wxEVT_TIMER, &wxSFMLControl::OnRenderTimer, this);
  Bind(wxEVT_SHOW, &wxSFMLControl::OnShow, this);
  Bind(wxEVT_SIZE, &wxSFMLControl::OnSize, this);
  Bind(wxEVT_MOUSEWHEEL, &wxSFMLControl::OnScroll, this);
  Bind(wxEVT_MOTION, &wxSFMLControl::OnMotion, this);
  Bind(wxEVT_CLOSE_WINDOW, &wxSFMLControl::OnClose, this);
}

wxSFMLControl::~wxSFMLControl(void) {
  m_render_timer.Stop();
  if (m_sfml_window) {
    m_sfml_window->close();
    delete m_sfml_window;
  }
}

sf::WindowHandle wxSFMLControl::GetWindowHandle(void) {
  sf::WindowHandle handle;

#ifdef __WXGTK__
  gtk_widget_realize(this->m_wxwindow);

  auto *window = gtk_widget_get_window(static_cast<GtkWidget *>(GetHandle()));
  XFlush(GDK_WINDOW_XDISPLAY(window));

  handle = GDK_WINDOW_XID(window);
#else
  handle = reinterpret_cast<sf::WindowHandle>(GetHandle());
#endif

  return handle;
}

void wxSFMLControl::InitSFML(void) {
  if (m_window_initialised)
    return;

  m_sfml_window = new sf::RenderWindow(GetWindowHandle());
  if (!m_sfml_window) return;

  // Set initial size and view to match control
  wxSize size = GetSize();
  m_sfml_window->setSize(sf::Vector2u(size.x, size.y));
  m_sfml_window->setView(
      sf::View(sf::FloatRect({0, 0}, sf::Vector2f(size.x, size.y))));

  m_window_initialised = true;
}

void wxSFMLControl::Render(void) {
  if (!m_window_initialised)
    InitSFML();

  if (!m_sfml_window)
    return;

  // Only render if window is visible
  if (!IsShownOnScreen())
    return;

  std::ignore = m_sfml_window->setActive(true);

  Frame(static_cast<sf::RenderWindow &>(*m_sfml_window));

  // m_sfml_window->clear(sf::Color::Blue);
  // {
  //   sf::CircleShape shape(50);
  //   shape.setFillColor(sf::Color::Black);
  //   shape.setPosition({50, 50});
  //   m_sfml_window->draw(shape);
  // }
  // m_sfml_window->display();
}

void wxSFMLControl::OnRenderTimer(wxTimerEvent &event) { Render(); }

void wxSFMLControl::OnShow(wxShowEvent& event) {
  if (event.IsShown()) {
    m_render_timer.Start(1000 / FPS);
  } else {
    m_render_timer.Stop();
  }
  event.Skip();
}

void wxSFMLControl::OnSize(wxSizeEvent& event) {
  if (m_sfml_window && m_window_initialised) {
    wxSize newSize = event.GetSize();
    m_sfml_window->setSize(sf::Vector2u(newSize.x, newSize.y));
    m_sfml_window->setView(
        sf::View(sf::FloatRect({0, 0}, sf::Vector2f(newSize.x, newSize.y))));
  }
  event.Skip();
}
