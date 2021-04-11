// Author: Jordan Randleman -- jordanran199@gmail.com -- help.hpp
// => Defines the primitive "help" function written in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_HELP_HPP_
#define HEIST_SCHEME_CORE_STDLIB_HELP_HPP_

#include "implementation.hpp"

namespace heist {

  data primitive_HELP(data_vector&& args) {
    if(args.empty()) {
      stdlib_help::launch_interactive_menu();
    } else {
      if(args.size() != 1)
        HEIST_THROW_ERR("'help didn't receive a query symbol!"
          "\n     (help <optional-query-symbol-or-string>)" << HEIST_FCN_ERR("help",args));
      // Search for the query passed as a symbol | string
      if(args[0].is_type(types::sym))
        stdlib_help::query_datum(args[0].sym);
      else if(args[0].is_type(types::str))
        stdlib_help::query_datum(*args[0].str);
      // Query data about the object passed based on its type
      else if(stdlib_streams::data_is_stream_pair(args[0]))
        stdlib_help::query_datum("stream");
      else
        stdlib_help::query_datum(args[0].type_name());
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

} // End of namespace heist

#endif