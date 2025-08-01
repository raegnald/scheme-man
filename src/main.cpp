// Entry point of the Scheme-Man executable

#include <SFML/Config.hpp>
#include <s7.h>
#include <wx/wx.h>
#include "Application.hpp"

static_assert(SFML_VERSION_MAJOR == 3, "SFML version 3 required");
static_assert(__cplusplus >= 202302L, "Use C++23 or later");
static_assert(S7_MAJOR_VERSION == 11, "Use s7 scheme version 11");

wxIMPLEMENT_APP(Application);
