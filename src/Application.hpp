#pragma once

#include <wx/wx.h>
#include "Level.hpp"
#include "wxSchemeMan.hpp"

class MainFrame : public wxFrame {
public:
  MainFrame();

private:
  wxSchemeMan *m_canvas;
  Level *level;

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
