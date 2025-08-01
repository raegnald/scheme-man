#pragma once

#include <wx/wx.h>
#include "wxSFMLControl.hpp"

class MainFrame : public wxFrame {
public:
  MainFrame();

private:
  wxSFMLControl *m_canvas;

  void OnHello(wxCommandEvent& event);
  void OnExit(wxCommandEvent& event);
  void OnAbout(wxCommandEvent& event);

  void setMenus(void);
};

class Application : public wxApp {
public:
  virtual bool OnInit();
};

enum { ID_Hello = 1 };
