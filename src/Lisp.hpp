// An abstraction to initialise, manage a Lisp interpreter
// (specifically GNU Guile Scheme) and interact with the game logic in
// C++.

#pragma once

#include <libguile.h>
#include <thread>
#include <cstdlib>

struct Level;

struct Lisp {
private:
  std::thread m_guile_thread;

  SCM m_level_access, m_action_symbol, m_action_arg, m_action_result,
      m_game_ended_p, m_reset_values_fun;

  void m_bind_scman_guile_values(void) {
    m_level_access     = scm_c_lookup("scman-intrinsic/level-access");
    m_action_symbol    = scm_c_lookup("scman-intrinsic/action-to-perform");
    m_action_arg       = scm_c_lookup("scman-intrinsic/action-argument");
    m_action_result    = scm_c_lookup("scman-intrinsic/action-result");
    m_game_ended_p     = scm_c_lookup("scman-intrinsic/game-ended-p");
    m_reset_values_fun = scm_c_lookup("scman-internal/reset-action-values");
  }

  void m_execute_guile(void) {
    setenv("GUILE_AUTO_COMPILE", "1", 0);
    scm_init_guile();
    scm_c_primitive_load("../assets/Lisp/prelude.scm");
    m_bind_scman_guile_values();
    scm_shell(0, NULL);
  }

public:
  Lisp(void) : m_guile_thread([this]() { this->m_execute_guile(); }) {
    m_guile_thread.detach();
  }

  void update(Level *level);
};
