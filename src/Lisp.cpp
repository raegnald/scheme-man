#include <cstdlib>
#include <cstring>
#include <print>
#include "Lisp.hpp"
#include "Level.hpp"
#include "debug.hpp"

std::mutex performing_action;

// SCM lisp_start_action(SCM _args_whatever) {
//   performing_action.lock();
//   return SCM_UNSPECIFIED;
// }

// SCM lisp_finalise_action(SCM _args_whatever) {
//   performing_action.unlock();
//   return SCM_UNSPECIFIED;
// }

// SCM lisp_wait_for_action_completion(SCM _args_whatever) {
//   while (!performing_action.try_lock());
//   performing_action.unlock();
//   return SCM_UNSPECIFIED;
// }

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

  // // No action has to be performed because to action is set
  // if (scm_is_null(scm_variable_ref(m_action_symbol)))
  //   return;

  // An action has to be performed

  // const std::string action = scm_to_latin1_string(
  //     scm_symbol_to_string(scm_variable_ref(m_action_symbol)));

  // if (action == "walk") {
  //   level->player.walk();
  // } else if (action == "turn-right") {
  //   level->player.turnClockwise();
  // } else if (action == "turn-left") {
  //   level->player.turnAnticlockwise();
  // } else if (action == "set-status") {
  //   if (!scm_is_null(scm_variable_ref(m_action_arg))) {
  //     char *status = scm_to_latin1_string(scm_variable_ref(m_action_arg));
  //     level->setStatus(status);
  //   }
  // } else if (action == "reset-level") {
  //   level->reset();
  // } else if (action == "see") {
  //   int distance = 1;
  //   if (!scm_is_null(scm_variable_ref(m_action_arg)))
  //     distance = scm_to_int(scm_variable_ref(m_action_arg));

  //   if (const auto object = level->see(distance)) {
  //     std::string obj_str = object.value();

  //     // Executes in a Guile-context so that it runs thread-safe
  //     scm_with_guile(
  //         [](void *data) -> void * {
  //           std::string *obj_str = static_cast<std::string *>(data);

  //           SCM var = scm_c_lookup("scman-intrinsic/action-result");
  //           SCM symbol = scm_string_to_symbol(
  //               scm_from_latin1_stringn(obj_str->c_str(), obj_str->size()));
  //           scm_variable_set_x(var, symbol);

  //           delete obj_str;
  //           return nullptr;
  //         },
  //         new std::string(obj_str));
  //   }
  // }

  // performing_action.unlock();
}

void Lisp::shutdown(void) {
  // scm_with_guile(
  //     [](void *_data) -> void * {
  //       scm_call_0(scm_variable_ref(scm_c_lookup("exit")));
  //       return nullptr;
  //     },
  //     nullptr);
}
