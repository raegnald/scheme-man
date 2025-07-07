// An abstraction to initialise a (Common) Lisp interpreter, execute
// code, and free the interpreter.

#pragma once

#include <ecl/ecl.h>
// #include <ecl.
#include <string>

struct Lisp {
  using Object = cl_object;

  Lisp(void) { cl_boot(0, NULL); }
  Lisp(int argc, char *argv[]) { cl_boot(argc, argv); }

  ~Lisp(void) { cl_shutdown(); }

  Object eval(Object object) { return cl_eval(object); }
  Object eval(const std::string &source) {
    return cl_eval(cl_string_to_object(source.c_str()));
  }
};
