// An abstraction to initialise, manage a Lisp interpreter
// (specifically s7 scheme) and interact with the game logic in C++.

#pragma once

#include <iostream>
#include <print>
#include <ostream>
#include <s7.h>
#include <thread>

struct Level;

// SCM lisp_start_action(SCM _args_whatever);
// SCM lisp_finalise_action(SCM _args_whatever);
// SCM lisp_wait_for_action_completion(SCM _args_whatever);

struct Lisp {
private:
  std::thread m_s7_thread;
  s7_scheme *s7{nullptr};

  // SCM m_action_symbol, m_action_arg, m_action_result, m_game_ended_p;

  void m_bind_scman_s7_values(void) {
    // m_action_symbol = scm_c_lookup("scman-intrinsic/action-to-perform");
    // m_action_arg    = scm_c_lookup("scman-intrinsic/action-argument");
    // m_action_result = scm_c_lookup("scman-intrinsic/action-result");
    // m_game_ended_p  = scm_c_lookup("scman-intrinsic/game-ended-p");

    // scm_c_define_gsubr("scman-intrinsic/start-action", 0, 0, 0,
    //                    (void *)lisp_start_action);
    // scm_c_define_gsubr("scman-intrinsic/finalise-action", 0, 0, 0,
    //                    (void *)lisp_finalise_action);
    // scm_c_define_gsubr("scman-intrinsic/wait-for-action-completion", 0, 0, 0,
    //                    (void *)lisp_wait_for_action_completion);
  }

  void m_s7_shell(void) {
    std::string str;
    s7_pointer val;

    while (true) {
      std::print("> ");
      if (!std::getline(std::cin, str))
        break;

      val = s7_eval_c_string(s7, str.c_str());

      std::println("{}", s7_object_to_c_string(s7, val));
    }

    free(s7);
  }

  void m_start_s7(void) {
    s7 = s7_init();
    s7_load(s7, "../assets/Lisp/prelude.scm");
    m_bind_scman_s7_values();

    m_s7_shell();
  }

public:
  void initialise(void) {
    if (m_s7_thread.joinable())
      return;

    m_s7_thread = std::thread([this]() { this->m_start_s7(); });
    m_s7_thread.detach();
  }

  void update(Level *level);

  void shutdown(void);
};
