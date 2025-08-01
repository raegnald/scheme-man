#include "Application.hpp"
#include "wxSFMLControl.hpp"
#include "wxSchemeMan.hpp"
#include <memory>

bool Application::OnInit() {
  auto frame = new MainFrame;
  frame->Show(true);
  return true;
}

void MainFrame::setMenus(void) {
  auto *menuFile = new wxMenu;
  menuFile->Append(ID_Hello, "&Hello...\tCtrl-H",
                   "Help string shown in status bar for this menu item");
  menuFile->AppendSeparator();
  menuFile->Append(wxID_EXIT);

  auto *menuHelp = new wxMenu;
  menuHelp->Append(wxID_ABOUT);

  auto *menuBar = new wxMenuBar;
  menuBar->Append(menuFile, "&File");
  menuBar->Append(menuHelp, "&Help");

  SetMenuBar(menuBar);
}

MainFrame::MainFrame() : wxFrame(NULL, wxID_ANY, "Hello World") {
  setMenus();

  wxPanel* panel = new wxPanel(this);
  wxSizer* sizer = new wxBoxSizer(wxVERTICAL);

  m_canvas =
      new wxSchemeMan(panel, wxID_ANY, wxPoint(10, 10), wxSize(200, 200));
  // m_canvas->SetMaxSize(wxSize(200, 200));

  sizer->Add(m_canvas, 1, wxEXPAND | wxALL, 0);
  panel->SetSizer(sizer);

  CreateStatusBar();
  SetStatusText("Welcome to wxWidgets!");

  Bind(wxEVT_MENU, &MainFrame::OnHello, this, ID_Hello);
  Bind(wxEVT_MENU, &MainFrame::OnAbout, this, wxID_ABOUT);
  Bind(wxEVT_MENU, &MainFrame::OnExit, this, wxID_EXIT);
}

void MainFrame::OnExit(wxCommandEvent &event) { Close(true); }

void MainFrame::OnAbout(wxCommandEvent &event) {
  wxMessageBox("This is a wxWidgets Hello World example", "About Hello World",
               wxOK | wxICON_INFORMATION);
}

void MainFrame::OnHello(wxCommandEvent &event) {
  wxLogMessage("Hello world from wxWidgets!");
}
