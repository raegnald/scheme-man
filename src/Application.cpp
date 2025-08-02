#include "Application.hpp"
#include "wx/gdicmn.h"
#include "wx/richtext/richtextbuffer.h"
#include "wxSchemeMan.hpp"
#include <wx/wx.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/textctrl.h>

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

  // Create the wxSplitterWindow window and set a minimum pane size to
  // prevent unsplitting
  wxSplitterWindow *splitterWindow = new wxSplitterWindow(this, wxID_ANY);
  splitterWindow->SetMinimumPaneSize(300);

  wxPanel *left_panel = new wxPanel(splitterWindow, wxID_ANY);
  wxPanel *right_panel = new wxPanel(splitterWindow, wxID_ANY);


  m_canvas =
    new wxSchemeMan(right_panel, wxID_ANY, wxPoint(10, 10), wxSize(200, 200));
  level = m_canvas->game.getLevel();

  // Scheme text input
  wxTextCtrl *textCtrl1 =
    new wxTextCtrl(left_panel, wxID_ANY, "", wxDefaultPosition, wxDefaultSize,
                   wxTE_MULTILINE);
  textCtrl1->SetHint("Type expression, press Ctrl+Enter...");
  textCtrl1->Bind(wxEVT_CHAR_HOOK, [this, textCtrl1](wxKeyEvent &event) {
    if ((event.GetKeyCode() == WXK_RETURN ||
         event.GetKeyCode() == WXK_NUMPAD_ENTER) &&
        event.ControlDown()) {
      const auto content = std::string(textCtrl1->GetValue());
      std::println("Contents are {}", content);
      level->evaluateScheme(content);
    } else {
      event.Skip();
    }
  });
  wxBoxSizer *panel1Sizer = new wxBoxSizer(wxHORIZONTAL);
  panel1Sizer->Add(textCtrl1, 1, wxEXPAND);
  left_panel->SetSizer(panel1Sizer);


  // Game visualisation panel
  wxBoxSizer *panel2Sizer = new wxBoxSizer(wxHORIZONTAL);
  panel2Sizer->Add(m_canvas, 1, wxEXPAND | wxALL, 0);
  right_panel->SetSizer(panel2Sizer);


  splitterWindow->SplitVertically(left_panel, right_panel);


  // Set up the sizer for the frame and resize the frame according to
  // its contents
  wxBoxSizer *topSizer = new wxBoxSizer(wxHORIZONTAL);
  topSizer->Add(splitterWindow, 1, wxEXPAND);
  SetSizerAndFit(topSizer);

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
