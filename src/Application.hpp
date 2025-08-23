#pragma once

#include <wx/wx.h>
#include "Level.hpp"
#include "wxSchemeMan.hpp"

class MainFrame : public wxFrame {
public:
  MainFrame(void) = delete;
  MainFrame(std::filesystem::path level_path);

private:
  std::filesystem::path level_path;
  wxSchemeMan *m_canvas;
  Level *level;

  void OnHello(wxCommandEvent& event);
  void OnExit(wxCommandEvent& event);
  void OnAbout(wxCommandEvent& event);

  void setMenus(void);

  wxPanel *interpreterPanel(wxWindow *parent);

  wxPanel *interpreterInputPanel(wxWindow *parent);
  wxPanel *interpreterOutputPanel(wxWindow *parent);

  wxPanel *schemeManPanel(wxWindow *parent);
  wxTextCtrl *schemeTextCtrl(wxWindow *parent);
};

class Application : public wxApp {
public:
  virtual bool OnInit();
};

enum { ID_Hello = 1 };
