// Author: Jordan Randleman -- jrandleman@scu.edu -- invariants.hpp
// => Defines the primitive invariant manipulation functions written 
//    in C++ for the Heist Scheme Interpreter

#ifndef HEIST_SCHEME_CORE_STDLIB_INVARIANTS_HPP_
#define HEIST_SCHEME_CORE_STDLIB_INVARIANTS_HPP_

#include "implementation.hpp"

namespace heist {

  /******************************************************************************
  * READ/TOGGLE INVARIANTS FROM "lib/core/type_system/scheme_types/process.hpp"
  ******************************************************************************/

  // Defaults to disabling ANSI escape sequences, if not given a boolean.
  // Returns whether ANSI escapes sequences were disabled prior this call
  data primitive_SET_NANSI_BANG(data_vector&& args) {
    if(args.size() > 1)
      HEIST_THROW_ERR("'set-nansi! received incorrect # of args:"
        "\n     (set-nansi! <optional-bool>)" << HEIST_FCN_ERR("set-nansi!",args));
    bool original_setting_status = !G.USING_ANSI_ESCAPE_SEQUENCES;
    G.USING_ANSI_ESCAPE_SEQUENCES = false;
    if(!args.empty()) G.USING_ANSI_ESCAPE_SEQUENCES = args[0].is_falsey();
    return boolean(original_setting_status);
  }

  data primitive_NANSIP(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"nansi?");
    return boolean(!G.USING_ANSI_ESCAPE_SEQUENCES);
  }

  data primitive_CIP(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"ci?");
    return boolean(!GLOBALS::USING_CASE_SENSITIVE_SYMBOLS);
  }

  data primitive_SET_PPRINT_COLUMN_WIDTH_BANG(data_vector&& args) {
    return stdlib_invariants::primitive_TOGGLE_NUMERIC_SETTING(args,"set-pprint-column-width!",G.PPRINT_MAX_COLUMN_WIDTH);
  }

  data primitive_PPRINT_COLUMN_WIDTH(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"pprint-column-width");
    return num_type(G.PPRINT_MAX_COLUMN_WIDTH);
  }

  data primitive_SET_MAX_RECURSION_DEPTH_BANG(data_vector&& args) {
    return stdlib_invariants::primitive_TOGGLE_NUMERIC_SETTING(args,"set-max-recursion-depth!",G.MAX_RECURSION_DEPTH);
  }

  data primitive_MAX_RECURSION_DEPTH(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"max-recursion-depth");
    return num_type(G.MAX_RECURSION_DEPTH);
  }

  // Changes the REPL's line-by-line prompt from the default "> "
  data primitive_SET_REPL_PROMPT_BANG(data_vector&& args) {
    if(args.size() != 1)
      HEIST_THROW_ERR("'set-repl-prompt! didn't receive exactly 1 arg!"
        "\n     (set-repl-prompt! <prompt-string>)" << HEIST_FCN_ERR("set-repl-prompt!",args));
    if(!args[0].is_type(types::str))
      HEIST_THROW_ERR("'set-repl-prompt! "<<HEIST_PROFILE(args[0])<<" isn't a string:"
        "\n     (set-repl-prompt! <prompt-string>)" << HEIST_FCN_ERR("set-repl-prompt!",args));
    string old_prompt = G.REPL_PROMPT;
    if(args[0].str->empty()) {
      G.REPL_PROMPT = G.REPL_TAB = "";
      return make_str(old_prompt);
    }
    G.REPL_PROMPT = *args[0].str;
    G.REPL_TAB = string(G.REPL_PROMPT.size(), ' ');
    return make_str(old_prompt);
  }

  data primitive_REPL_PROMPT(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"repl-prompt");
    return make_str(G.REPL_PROMPT);
  }

  // Toggles Dynamic Procedure Call Tracing (returns the previous state prior toggle)
  data primitive_SET_DYNAMIC_CALL_TRACE_BANG(data_vector&& args) {
    return stdlib_invariants::primitive_TOGGLE_BOOLEAN_SETTING(args,"set-dynamic-call-trace!",G.TRACING_ALL_FUNCTION_CALLS);
  }

  data primitive_DYNAMIC_CALL_TRACEP(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"dynamic-call-trace?");
    return boolean(G.TRACING_ALL_FUNCTION_CALLS);
  }

  // Toggles Procedure Argument Call Tracing (returns the previous state prior toggle)
  data primitive_SET_TRACE_ARGS_BANG(data_vector&& args) {
    return stdlib_invariants::primitive_TOGGLE_BOOLEAN_SETTING(args,"set-trace-args!",G.TRACE_ARGS);
  }

  data primitive_TRACE_ARGSP(data_vector&& args) {
    stdlib_invariants::confirm_no_args_given(args,"trace-args?");
    return boolean(G.TRACE_ARGS);
  }

  data primitive_SET_DOT_BANG(data_vector&& args) {
    if(args.size() != 1 || !args[0].is_type(types::sym))
      HEIST_THROW_ERR("'set-dot! didn't receive 1 symbol!"
        "\n     (set-dot! <symbol>)" << HEIST_FCN_ERR("set-dot!",args));
    data original_dot = G.dot;
    G.dot = args[0].sym;
    return original_dot;
  }

  data primitive_DOT(data_vector&& args) {
    if(!args.empty())
      HEIST_THROW_ERR("'dot doesn't accept any args: (dot)" << HEIST_FCN_ERR("dot",args));
    return data(G.dot);
  }

  /******************************************************************************
  * REGISTER FALSEY VALUES
  ******************************************************************************/

  data primitive_SET_FALSEY_BANG(data_vector&& args) {
    if(args.empty()) return GLOBALS::VOID_DATA_OBJECT;
    // Verify not adding #t as a falsey value
    for(auto& arg : args) {
      if(arg.is_type(types::bol) && arg.bol.val) {
        HEIST_THROW_ERR("'set-falsey! can't set #t to be falsey!"
          "\n     (set-falsey! <datum> ...)" << HEIST_FCN_ERR("set-falsey!",args));
      }
    }
    // Register falsey values
    for(auto& arg : args) {
      bool found = false;
      for(auto& val : G.FALSEY_VALUES) {
        if(val.equal(arg)) {
          found = true;
          break;
        }
      }
      if(!found) G.FALSEY_VALUES.push_back(arg.copy());
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * REGISTER TRUTHY VALUES (EFFECTIVELY RMS FROM SET OF FALSEY VALUES IF PRESENT)
  ******************************************************************************/

  data primitive_SET_TRUTHY_BANG(data_vector&& args) {
    if(args.empty()) return GLOBALS::VOID_DATA_OBJECT;
    // Verify not adding #f as a truthy value
    for(auto& arg : args) {
      if(arg.is_type(types::bol) && !arg.bol.val) {
        HEIST_THROW_ERR("'set-truthy! can't set #f to be truthy!"
          "\n     (set-truthy! <datum> ...)" << HEIST_FCN_ERR("set-truthy!",args));
      }
    }
    // Remove truthy values from set of false values
    for(auto& arg : args) {
      for(size_type i = 0; i < G.FALSEY_VALUES.size();) {
        if(G.FALSEY_VALUES[i].equal(arg)) {
          G.FALSEY_VALUES.erase(G.FALSEY_VALUES.begin()+i);
        } else {
          ++i;
        }
      }
    }
    return GLOBALS::VOID_DATA_OBJECT;
  }

  /******************************************************************************
  * GET LIST OF FALSEY VALUES
  ******************************************************************************/

  data primitive_FALSEY_VALUES(data_vector&& args) {
    if(!args.empty())
      HEIST_THROW_ERR("'falsey-values doesn't accept any args!" << HEIST_FCN_ERR("falsey-values",args));
    return primitive_toolkit::convert_data_vector_to_proper_list(G.FALSEY_VALUES.begin(),G.FALSEY_VALUES.end());
  }

} // End of namespace heist

#endif