// An abstraction to initialise, manage a Lisp interpreter
// (specifically s7 scheme) and interact with the game logic in C++.

#pragma once

#include <condition_variable>
#include <functional>
#include <queue>
#include <s7.h>
#include <print>
#include <thread>

struct Level;

struct Lisp_log {
  enum Type { Normal, Error } type;
  std::string message;
};

struct Lisp {
  using Task = std::function<void()>;

private:
  s7_scheme              *s7{nullptr};
  std::thread             m_s7_thread;
  std::queue<Task>        m_tasks;
  std::mutex              m_task_mutex;
  std::condition_variable m_task_cv;
  bool                    m_running = true;
  static std::function<void(const Lisp_log &log)> log_callback;
  static std::mutex log_callback_mutex;

  static const char *m_playing, *m_action_symbol, *m_action_arg,
      *m_action_result;

  void m_define_scman_s7_values(void);
  void m_start_s7(void);

  static s7_pointer receive_log(s7_scheme *sc, s7_pointer args);

  static void callLogCallbackIfSet(const Lisp_log &log) {
    std::function<void(const Lisp_log &)> callback;
    {
      std::lock_guard<std::mutex> lk(log_callback_mutex);
      callback = log_callback;
    }
    if (callback)
      callback(log);
  }

public:
  void initialise(void);
  void shutdown(void);

  void enqueue(Task t);
  void update(Level *level);

  void evaluate(const std::string &expr);

  static void setLogCallback(std::function<void(const Lisp_log &)> callback) {
    std::lock_guard<std::mutex> lk(log_callback_mutex);
    log_callback = std::move(callback);
  }
};
