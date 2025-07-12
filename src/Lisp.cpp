
#include <cstdlib>
#include <cstring>
#include <libguile.h>
#include <libguile/variable.h>
#include <print>
#include "Lisp.hpp"
#include "Level.hpp"

std::mutex performing_action;

SCM lisp_start_action(SCM _args_whatever) {
  performing_action.lock();
  return SCM_UNSPECIFIED;
}

SCM lisp_finalise_action(SCM _args_whatever) {
  performing_action.unlock();
  return SCM_UNSPECIFIED;
}

void Lisp::update(Level *level) {
  if (!level)
    return;

  if (level->player.walking)
    return;

  // If I can lock the mutex, then there is no action to perform
  if (performing_action.try_lock()) {
    performing_action.unlock();
    return;
  }

  // No action has to be performed because to action is set
  if (scm_is_null(scm_variable_ref(m_action_symbol)))
    return;

  // An action has to be performed

  char *action = scm_to_latin1_string(
      scm_symbol_to_string(scm_variable_ref(m_action_symbol)));

  if (strcmp(action, "walk") == 0) {
    level->player.walk();
  } else if (strcmp(action, "turn-right") == 0) {
    level->player.turnClockwise();
  } else if (strcmp(action, "turn-left") == 0) {
    level->player.turnAnticlockwise();
  } else if (strcmp(action, "set-status") == 0) {
    if (!scm_is_null(scm_variable_ref(m_action_arg))) {
      char *status = scm_to_latin1_string(scm_variable_ref(m_action_arg));
      level->setStatus(status);
    }
  }

  free(action);

  performing_action.unlock();
}
