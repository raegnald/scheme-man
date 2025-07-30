#include <cstdlib>
#include <cstring>
#include <future>
#include <print>
#include "Lisp.hpp"
#include "Level.hpp"
#include "s7.h"

const char *Lisp::m_playing       = "scman:playing?",
           *Lisp::m_action_symbol = "scman:action-to-perform",
           *Lisp::m_action_arg    = "scman:action-argument",
           *Lisp::m_action_result = "scman:action-result";

std::mutex performing_action;

void Lisp::initialise(void) {
  if (m_s7_thread.joinable())
    return;

  m_s7_thread = std::thread([this]() { this->m_start_s7(); });
  m_s7_thread.detach();
}

void Lisp::shutdown(void) {
  {
    std::lock_guard lk(m_task_mutex);
    m_running = false;
  }
  m_task_cv.notify_one();
}

void Lisp::enqueue(Task t) {
  {
    std::lock_guard lk(m_task_mutex);
    m_tasks.push(std::move(t));
  }
  m_task_cv.notify_one();
}

void Lisp::update(Level *level) {
  if (!level)
    return;

  if (level->player.walking)
    return;

  // If the mutex can be locked, then there is no action to perform
  if (performing_action.try_lock()) {
    performing_action.unlock();
    return;
  }

  // No action has to be performed because no action is set
  if (s7_is_null(s7, s7_name_to_value(s7, m_action_symbol)))
    return;

  // An action has to be performed

  std::promise<std::string> p;
  auto f = p.get_future();

  // Obtaining the action
  enqueue([this, &p]() {
    s7_pointer sym = s7_name_to_value(s7, m_action_symbol);
    p.set_value(s7_symbol_name(sym));
  });

  const std::string action = f.get(); // blocks until we get the value

  if (action == "walk") {
    level->player.walk();
  } else if (action == "turn-right") {
    level->player.turnClockwise();
  } else if (action == "turn-left") {
    level->player.turnAnticlockwise();
  } else if (action == "set-status") {
    if (!s7_is_null(s7, s7_name_to_value(s7, m_action_arg))) {
      const char *status = s7_string(s7_name_to_value(s7, m_action_arg));
      level->setStatus(status);
    }
  } else if (action == "reset-level") {
    level->reset();
  } else if (action == "see") {
    int distance = 1;
    if (!s7_is_null(s7, s7_name_to_value(s7, m_action_arg)))
      distance = s7_integer(s7_name_to_value(s7, m_action_arg));

    if (const auto object = level->see(distance)) {
      const std::string object_name = object.value();

      enqueue([this, &object_name]() {
        s7_pointer name_symbol = s7_make_symbol(s7, object_name.c_str());
        s7_symbol_set_value(s7, s7_make_symbol(s7, m_action_result),
                            name_symbol);
      });
    }
  }

  performing_action.unlock();
}

s7_pointer lisp_start_action(s7_scheme *sc, s7_pointer args) {
  performing_action.lock();
  return s7_unspecified(sc);
}

s7_pointer lisp_finalise_action(s7_scheme *sc, s7_pointer args) {
  performing_action.unlock();
  return s7_unspecified(sc);
}

s7_pointer lisp_wait_for_action_completion(s7_scheme *sc, s7_pointer args) {
  return s7_unspecified(sc);
}

void Lisp::m_define_scman_s7_values(void) {
  s7_define_variable(s7, m_playing, s7_t(s7));
  s7_define_variable(s7, m_action_symbol, s7_nil(s7));
  s7_define_variable(s7, m_action_arg, s7_nil(s7));
  s7_define_variable(s7, m_action_result, s7_nil(s7));

  s7_define_function(s7, "scman:start-action", lisp_start_action, 0, 0, false,
                     "Stops Scheme code from executing while an action is "
                     "being prepared from within Scheme");
  s7_define_function(s7, "scman:finalise-action", lisp_finalise_action, 0, 0,
                     false, "Resume execution of Scheme code");
  s7_define_function(s7, "scman:wait-for-action-completion",
                     lisp_wait_for_action_completion, 0, 0, false,
                     "Stop execution of the Scheme code until the action is "
                     "performed by C++");
}

void Lisp::m_start_s7(void) {
  s7 = s7_init();

  m_define_scman_s7_values();

  s7_add_to_load_path(s7, "../assets/Lisp/");
  s7_load(s7, "../assets/Lisp/prelude.scm");

  while (m_running) {
    Task task;
    {
      std::unique_lock lk(m_task_mutex);
      m_task_cv.wait(lk, [&] { return !m_tasks.empty() || !m_running; });

      if (!m_running && m_tasks.empty())
        break;

      task = std::move(m_tasks.front());
      m_tasks.pop();
    }
    task();
  }

  s7_quit(s7);
}
