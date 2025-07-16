// An abstraction to initialise, manage a Lisp interpreter
// (specifically GNU Guile Scheme) and interact with the game logic in
// C++.

#pragma once

#include <libguile.h>
#include <libguile/gsubr.h>
#include <libguile/scm.h>
#include <mutex>
#include <thread>
#include <cstdlib>

struct Level;

SCM lisp_start_action(SCM _args_whatever);
SCM lisp_finalise_action(SCM _args_whatever);

struct Lisp {
private:
  std::thread m_guile_thread;

  SCM m_action_symbol, m_action_arg, m_action_result, m_game_ended_p;

  void m_bind_scman_guile_values(void) {
    m_action_symbol = scm_c_lookup("scman-intrinsic/action-to-perform");
    m_action_arg    = scm_c_lookup("scman-intrinsic/action-argument");
    m_action_result = scm_c_lookup("scman-intrinsic/action-result");
    m_game_ended_p  = scm_c_lookup("scman-intrinsic/game-ended-p");

    scm_c_define_gsubr("scman-intrinsic/start-action", 0, 0, 0,
                       (void *)lisp_start_action);
    scm_c_define_gsubr("scman-intrinsic/finalise-action", 0, 0, 0,
                       (void *)lisp_finalise_action);
  }

  void m_execute_guile(void) {
    setenv("GUILE_AUTO_COMPILE", "0", 0);
    scm_init_guile();
    scm_c_primitive_load("../assets/Lisp/prelude.scm");
    m_bind_scman_guile_values();
    scm_shell(0, NULL);
  }

public:
  void initialise(void) {
    if (m_guile_thread.joinable())
      return;

    m_guile_thread = std::thread([this]() { this->m_execute_guile(); });
    m_guile_thread.detach();
  }

  void update(Level *level);

  void shutdown(void);
};
