#include "Application.hpp"
#include "Lisp.hpp"
#include "wx/font.h"
#include "wx/gdicmn.h"
#include "wx/gtk/colour.h"
#include "wxSchemeMan.hpp"
#include <wx/wx.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/textctrl.h>
#include <wx/richtext/richtextctrl.h>

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

wxTextCtrl *MainFrame::schemeTextCtrl(wxWindow *parent) {
  wxTextCtrl *text_ctrl =
    new wxTextCtrl(parent, wxID_ANY, "", wxDefaultPosition, wxDefaultSize,
                   wxTE_MULTILINE);

  text_ctrl->SetHint("Type expression, press Ctrl+Enter...");

#if defined __WXCOCOA__
  textCtrl1->OSXDisableAllSmartSubstitutions();
#endif

  wxFont font(12, wxFONTFAMILY_TELETYPE, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
  text_ctrl->SetFont(font);

  text_ctrl->Bind(wxEVT_CHAR_HOOK, [this, text_ctrl](wxKeyEvent &event) {
    if ((event.GetKeyCode() == WXK_RETURN ||
         event.GetKeyCode() == WXK_NUMPAD_ENTER) &&
        event.ControlDown()) {
      const auto content = text_ctrl->GetValue().ToStdString();
      level->evaluateScheme(content);
    } else {
      event.Skip();
    }
  });

  return text_ctrl;
}

wxPanel *MainFrame::interpreterOutputPanel(wxWindow *parent) {
  wxPanel *panel = new wxPanel(parent, wxID_ANY);

  wxRichTextCtrl *richTextCtrl = new wxRichTextCtrl(
      panel, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(200, 200),
      wxVSCROLL | wxHSCROLL | wxBORDER_NONE | wxWANTS_CHARS);

  wxFont mono_font{12, wxFONTFAMILY_TELETYPE, wxFONTSTYLE_NORMAL,
                   wxFONTWEIGHT_NORMAL};

  richTextCtrl->SetFont(mono_font);
  richTextCtrl->BeginSuppressUndo();
  richTextCtrl->BeginParagraphSpacing(0, 20);

  level->interpreter.setLogCallback([richTextCtrl](const Lisp_log &log) {
    wxString text = wxString::FromUTF8(log.message);

    richTextCtrl->CallAfter([richTextCtrl, text](void) {
      richTextCtrl->AppendText("\n");
      richTextCtrl->AppendText(text);
      richTextCtrl->ShowPosition(richTextCtrl->GetLastPosition());
    });
  });

  wxBoxSizer *panel1Sizer = new wxBoxSizer(wxHORIZONTAL);
  panel1Sizer->Add(richTextCtrl, 1, wxEXPAND);
  panel->SetSizer(panel1Sizer);

  return panel;
}

wxPanel *MainFrame::interpreterInputPanel(wxWindow *parent) {
  wxPanel *interpreter_panel = new wxPanel(parent, wxID_ANY);
  interpreter_panel->SetSize({200, 200});

  // Scheme text input
  wxTextCtrl *textCtrl1 = schemeTextCtrl(interpreter_panel);
  wxBoxSizer *panel1Sizer = new wxBoxSizer(wxHORIZONTAL);
  panel1Sizer->Add(textCtrl1, 1, wxEXPAND);
  interpreter_panel->SetSizer(panel1Sizer);

  return interpreter_panel;
}

wxPanel *MainFrame::interpreterPanel(wxWindow *parent) {
  wxPanel *panel = new wxPanel(parent, wxID_ANY);

  wxSplitterWindow *interpreter_splitter =
      new wxSplitterWindow(panel, wxID_ANY);
  interpreter_splitter->SetMinimumPaneSize(100);

  wxPanel *output_panel = interpreterOutputPanel(interpreter_splitter);
  wxPanel *input_panel = interpreterInputPanel(interpreter_splitter);

  interpreter_splitter->SplitHorizontally(output_panel, input_panel);

  wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
  sizer->Add(interpreter_splitter, 1, wxEXPAND);
  panel->SetSizer(sizer);

  return panel;
}

wxPanel *MainFrame::schemeManPanel(wxWindow *parent) {
  wxPanel *game_panel = new wxPanel(parent, wxID_ANY);

  m_canvas = new wxSchemeMan(game_panel, wxID_ANY, wxDefaultPosition,
                             wxSize(200, 400));
  level = m_canvas->game.getLevel();

  wxBoxSizer *panel2Sizer = new wxBoxSizer(wxHORIZONTAL);
  panel2Sizer->Add(m_canvas, 1, wxEXPAND | wxALL, 0);
  game_panel->SetSizer(panel2Sizer);

  return game_panel;
}

MainFrame::MainFrame() : wxFrame(NULL, wxID_ANY, "Hello World") {
  setMenus();

  // Create the wxSplitterWindow window and set a minimum pane size to
  // prevent unsplitting
  wxSplitterWindow *main_splitter = new wxSplitterWindow(this, wxID_ANY);
  main_splitter->SetMinimumPaneSize(300);

  wxPanel *right_panel = schemeManPanel(main_splitter);
  wxPanel *left_panel = interpreterPanel(main_splitter);

  main_splitter->SplitVertically(left_panel, right_panel);

  // Set up the sizer for the frame and resize the frame according to
  // its contents
  wxBoxSizer *topSizer = new wxBoxSizer(wxHORIZONTAL);
  topSizer->Add(main_splitter, 1, wxEXPAND);
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
