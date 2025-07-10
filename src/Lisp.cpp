
#include <cstdlib>
#include <cstring>
#include <libguile.h>
#include <libguile/boolean.h>
#include <libguile/eval.h>
#include <libguile/srfi-14.h>
#include <libguile/strings.h>
#include <libguile/threads.h>
#include <print>
#include "Lisp.hpp"
#include "Level.hpp"

void Lisp::update(Level *level) {
  if (!level)
    return;

  if (level->player.walking)
    return;

  if (scm_is_false(scm_mutex_locked_p(scm_variable_ref(m_level_access))))
    return;

  // No action has to be performed
  if (scm_is_null(scm_variable_ref(m_action_symbol)))
    return;

  // An action has to be performed

  char *action = scm_to_latin1_string(
      scm_symbol_to_string(scm_variable_ref(m_action_symbol)));

  // std::println("Action symbol is {}", action);
  level->setStatus(action);

  if (strcmp(action, "walk") == 0) {
    level->player.walk();
  } else if (strcmp(action, "turn-right") == 0) {
    level->player.turnClockwise();
  } else if (strcmp(action, "turn-left") == 0) {
    level->player.turnAnticlockwise();
  }

  free(action);

  // scm_call_0(scm_variable_ref(m_reset_values_fun));
  scm_unlock_mutex(scm_variable_ref(m_level_access));
}
